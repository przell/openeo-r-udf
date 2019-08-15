library(stars)
library(abind)
library(raster)
library(lubridate)

DEBUG = TRUE

#TODO define float maximum digits

source("data_transformation.R")


.measure_time = function(fun,message,envir=parent.frame()) {
  if (!DEBUG) return(eval(fun,envir = envir))
  tryCatch({
    start = Sys.time()
    eval(fun,envir=envir)
  },error=function(e){
    stop(e$message)
  },finally = {
    cat(message)
    cat("\n")
    print(Sys.time()-start)
  })
}

#* @apiTitle R UDF API
#*
#* Takes a UDFRequest containing data and code and runs the code on the data
#* 
#* @post /udf
run_UDF.json = function(req,res) {
  if (DEBUG) print(format(Sys.time(),format = "%F %H:%M:%OS"))
  
  cat("Started executing at endpoint /udf\n")
  
  json_in = .measure_time(quote(jsonlite::fromJSON(req$postBody)),"Read json. Runtime:")
  
  if (is.null(json_in$code$language) || !tolower(json_in$code$language)=="r") stop("Cannot interprete code source, due to missing programming language.")
  
  # prepare the executable code
  fun = function() {}
  formals(fun) = alist(data=) #TODO also metadata?
  body(fun) = parse(text=json_in$code$source)
  #TODO check which data type comes in, then create an according data structure
  # transform data into stars
  stars_in = .measure_time(quote(json2stars_array(json_in)),"Translated list into stars. Runtime:")
  
  rm(json_in)
  # run the UDF
  stars_out = .measure_time(quote(fun(data=stars_in)),"Executed script. Runtime:")
  #TODO build type related output transformation (user has to define dimensionality)
  # transform stars into JSON
  json_out = .measure_time(quote(stars2json.raster_collection_tiles(stars_obj = stars_out)),"Translated from stars to list. Runtime:")
  
  json=.measure_time(quote(toJSON(json_out,auto_unbox = TRUE)),"Prepared JSON from list. Runtime:")
  
  if (DEBUG) print(format(Sys.time(),format = "%F %H:%M:%OS"))
  res$setHeader(name = "CONTENT-TYPE",value = "application/json")
  res$body = json
  
}

#* Gets the library configuration of this udf service
#* @get /libs
#* @serializer unboxedJSON
get_installed_libraries = function() {
  libs = as.data.frame(installed.packages()[,c("Package","Version")])
  rownames(libs) = NULL

  return(libs)
}

