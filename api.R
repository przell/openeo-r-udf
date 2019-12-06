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
  data_in = .measure_time(quote(as(req$data,"stars")),"Translated list into stars. Runtime:")
  
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
      data_in = lapply(data_in, as.xts)
      
    } else {
      stop("Not supported variable class. Use 'stars' or 'xts'")
    }
  }
  
  # run the UDF
  stars_out = .measure_time(quote(lapply(data_in, fun)),"Executed script. Runtime:")
  
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

