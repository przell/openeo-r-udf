library(stars)
library(abind)
library(raster)
library(lubridate)
library(tibble)
library(dplyr)
library(magrittr)

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

#* Interprete JSON, divide code and data and assign classes
#* @filter check-data
function(req, res) {
  if (length(req$postBody) > 0) {
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
run_UDF.json = function(req,res) {
  if (DEBUG) print(format(Sys.time(),format = "%F %H:%M:%OS"))
  
  cat("Started executing at endpoint /udf\n")
  
  # prepare the executable code
  fun = function() {}
  formals(fun) = alist(data=) #TODO also metadata?
  body(fun) = parse(text=req$code$source)
  #TODO check which data type comes in, then create an according data structure
  # transform data into stars
  stars_in = .measure_time(quote(as(req$data,"stars")),"Translated list into stars. Runtime:")
  
  # run the UDF
  stars_out = .measure_time(quote(fun(data=stars_in)),"Executed script. Runtime:")
  #TODO build type related output transformation (user has to define dimensionality)
  # transform stars into JSON
  json_out = .measure_time(quote(as(stars_out,"RasterCollectionTile")),"Translated from stars to list. Runtime:")
  
  json=.measure_time(quote(jsonlite::toJSON(json_out,auto_unbox = TRUE)),"Prepared JSON from list. Runtime:")
  
  if (DEBUG) print(format(Sys.time(),format = "%F %H:%M:%OS"))
  res$setHeader(name = "CONTENT-TYPE",value = "application/json")
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

