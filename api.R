library(stars)
library(abind)
library(lubridate)
library(xts)

DEBUG = FALSE

#TODO define float maximum digits

source("data_transformation.R")

.measure_time = function(fun,message,envir=parent.frame()) {
  if (!DEBUG) return(eval(fun,envir = envir))
  tryCatch({
    start = Sys.time()
    return(eval(fun,envir=envir))
  },error=function(e){
    stop(e$message)
  },finally = {
    cat(message)
    cat("\n")
    print(Sys.time()-start)
  })
}

.read_data_requirement = function(code) {
  require_annotation = "@require"
  require_regex = paste0("[#]+\\s*",require_annotation,"\\s*(\\w+):(\\w+)\\s*\\n")
  selection = unlist(regmatches(code,regexec(require_regex,code,perl=TRUE)))
  
  if (length(selection) > 0) {
    return(list(
      variable_name = selection[2],
      target_class = selection[3]
    ))
  } else {
    return(list())
  }
}

#* Interprete JSON, divide code and data and assign classes
#* @filter check-data
check_data = function(req, res) {
  if (DEBUG) {
    cat("=== Started executing at endpoint /udf ===\n")
  }
  if (length(req$postBody) > 0) {
    if (DEBUG) {
      cat("Upload data:\n")
      print(Sys.time()-as_datetime(as.numeric(req$HEADERS["date"]),tz=Sys.timezone()))
    }
    
    json_in = .measure_time(quote(jsonlite::fromJSON(req$postBody)),"Read json. Runtime:")
    
    req$postBody = NULL
    if (is.null(json_in$code$language) || !tolower(json_in$code$language)=="r") {
      res$status = 400 #maybe 422
      return(list(error = "Cannot interprete code source, due to missing programming language."))
    }
    
    req$code = json_in$code
    req$data = json_in$data
    
    if (length(req$data$raster_collection_tiles) > 0) {
      class(req$data) = "RasterCollectionTile"
    } else if (length(req$data$hypercubes) > 0) {
      class(req$data) = "HyperCube"
    } else {
      res$status = 400
      return(list(error = "Data other than RasterCollectionTile and Hypercube are not supported yet."))
    }
  }
  plumber::forward()
}

#* @apiTitle R UDF API
#*
#* Takes a UDFRequest containing data and code and runs the code on the data
#* 
#* @post /udf
post_udf.json = function(req,res) {
  
  # prepare the executable code
  fun = function() {}
  formals(fun) = alist(data=) #TODO also metadata from run_udf (processes api)
  
  tryCatch({
    if (!startsWith(req$code$source,"{")) {
      # if a starting bracket is missing set opening and closing ones, otherwise we assume that the
      # provided code is clean
      req$code$source = paste0("{",req$code$source,"}")
    }
    
    
    body(fun) = parse(text=req$code$source)
  },
  error = function(e) {
    stop("Provided R code is not valid. Please check code syntax, parenthesis and spelling.")
  })
  
  # transform data into stars
  stars_in = .measure_time(quote(as(req$data,"stars")),"Translated list into stars. Runtime:")
  # if data requirements states something else than stars we need to convert it
  data_requirement = .read_data_requirement(req$code$source)
  if (length(data_requirement) > 0) {
    if (length(data_requirement$variable_name) > 0) {
      #replace variable name in fun
      names(formals(fun)) = data_requirement$variable_name
      # TODO when we use the context this needs to be accounted for!
    }
    if (length(data_requirement$target_class) > 0 && data_requirement$target_class == "xts") {
      # coerce stars_in into the target class
      data_in = lapply(stars_in, as.xts)
      
    } else {
      if (!(length(data_requirement$target_class) > 0 && data_requirement$target_class == "stars")) {
        stop("Not supported variable class. Use 'stars' or 'xts'")
      } else {
        data_in = stars_in
      }
    }
  }
  # run the UDF
  stars_out = .measure_time(quote(lapply(data_in, fun)),"Executed script. Runtime:")
  
  
  # map to stars if other class
  stars_out = lapply(1:length(stars_out), function(index) {
    if (class(stars_out[[index]]) == "stars") {
      return(stars_out[[index]])
    } else if (class(data_in[[1]]) == "xts") {
      
      return(reduce_4d_time_xts(udf_result = stars_out[[index]],
                                stars_in = stars_in[[index]],
                                time = "t"
                                ))
      
      # if (is.null(dim(return_obj))) {
      #   attr_name = names(return_obj)
      #   dim(return_obj) = 1
      #   names(dim(return_obj)) = attr_name
      # }
      # if (!is.null(dim(return_obj))) {
      #   return_obj = t(return_obj)
      #   dim(return_obj) = c(dim(input)[-t], new = prod(dim(return_obj))/prod(dim(input)[-t]))
      #   return(st_as_stars(list(x = return_obj))) # but this looses all the dimension metadata)
      # } else { # f() returns a vector, not a matrix
      #   if (length(dim(input)) > 1) {
      #     dim(return_obj) = dim(input)[-t]
      #     
      #     return(st_as_stars(list(x = return_obj), dimensions = st_dimensions(input)[-t] ))
      #   } else {
      #     # in order to keep the dimensionality > 0 make a attribute dimension
      #     dim(return_obj) = length(names(input))
      #     return(st_set_dimensions(return_obj,
      #                              which=1,
      #                              values=names(stars_out[[index]])))
      #   }
      #   
      #   
      # } 
    } else {
      stop("UDF data return is not xts or stars.")
    }
  })
  # transform stars into JSON
  json_out = .measure_time(quote(lapply(stars_out,function(obj) as(obj,"HyperCube"))),"Translated from stars to list. Runtime:")
  
  # if length 1 return, if more join
  if (length(json_out) == 1) {
    json_out = json_out[[1]]
  } else {
    shell = json_out[[1]]
    shell$hypercubes = lapply(unname(json_out),function(obj) obj$hypercubes[[1]])
    json_out = shell
    rm(shell)
  }
  
  json = .measure_time(quote(jsonlite::toJSON(json_out,auto_unbox = TRUE)),"Prepared JSON from list. Runtime:")
  
  res$setHeader(name = "CONTENT-TYPE",value = "application/json")
  res$setHeader(name = "date", value = Sys.time())
  res$body = json
  
  return(res)
}

#* Gets the library configuration of this udf service
#* @get /libs
#* @serializer unboxedJSON
#* @preempt check-data
get_installed_libraries = function() {
  libs = as.data.frame(installed.packages()[,c("Package","Version")])
  rownames(libs) = NULL

  return(libs)
}

reduce_4d_time_xts = function(udf_result, stars_in, time = "t", ...) {
  t = which(names(dim(stars_in)) == time)
  
  
  if (class(udf_result) %in% c("numeric","character","factor")) {
    if (length(udf_result) == 0) stop("No values are returned in xts UDF.")
    variable_names = names(udf_result)
    has_variable_names = !is.null(variable_names)
    
    if (length(udf_result) >= 1) {
      #univariat
      dim(udf_result) = c(variable = length(udf_result)) # -> array
    } 
    
    if (has_variable_names) {
      # add names as dimension values
      udf_result = st_as_stars(list(x = udf_result))
      udf_result = st_set_dimensions(udf_result,which="variable",values=variable_names)
      return(udf_result)
    } else {
      return(st_as_stars(list(x = udf_result)))
    }
    
  }
  if (is.xts(udf_result)) {
    return(st_as_stars(udf_result))
  }
  
  if (!is.null(dim(udf_result))) { # xts?
    udf_result = t(udf_result)
    
    ncols = ncol(udf_result)
    variable_names = colnames(udf_result)
    has_variable_names = !is.null(variable_names)
    
    # "variable" will be the new dimension which is created for the multivariate analysis
    dim(udf_result) = c(dim(stars_in)[-t], 
                        variable = prod(dim(udf_result))/prod(dim(stars_in)[-t])) 
    
    dims = st_dimensions(stars_in)[-t]
    if (has_variable_names) {
      dims$variables = stars:::create_dimension(values = variable_names)
    } else {
      dims$variables = stars:::create_dimension(from = 1 , to = ncols)
    }
    
    return(st_as_stars(list(x = udf_result), dimensions = dims))
  } else { # f() returns a vector, not a matrix
    dim(udf_result) = dim(stars_in)[-t] # with the new additions this should not occur
    st_as_stars(list(x = udf_result), dimensions = st_dimensions(stars_in)[-t]) 
  }
}

