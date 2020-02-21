# Test r-udf

# libs -------------------------------------------------------------------------
library(jsonlite)
library(httr)

# send_udf() function ----------------------------------------------------------
# this is taken from branch master... in the develop branch i can't find the function send_udf() anywhere
# data is the parsed JSON (a list)
# code is quoted R code
send_udf = function(data, 
                    code, host="http://localhost", 
                    port=NULL, 
                    debug = FALSE, 
                    download_info = FALSE) {
  if (is.character(data)) {
    data = read_json(data, simplifyVector = TRUE)
  }
  
  payload = list(
    code = list(
      source = paste(deparse(code),collapse = "\n"),
      language = "R"),
    data = data
  )
  
  if (is.null(port)) {
    url = paste0(host,"/udf")
  } else {
    url = paste0(host,":",port,"/udf")
  }
  
  options(digits.secs=3)
  start = Sys.time()
  res = httr::POST(url,
                   config=add_headers(Date=start),
                   body=toJSON(payload,auto_unbox = TRUE),
                   encode=c("json"), 
                   verbose())
  end = Sys.time()
  if (debug) {
    print(end-start)
  }
  
  if (download_info) {
    cat("Download time:\n")
    print(end-as_datetime(res$date,tz=Sys.timezone()))
  }
  
  if (res$status > 400) {
    message(paste0("[Server-ERROR] ",content(res,as = "parsed")$message))
  } else {
    message(paste0("Successful"))
    return(content(res,as = "text",encoding = "UTF-8"))
  }
}

# scripts ----------------------------------------------------------------------
script_min_ndvi = quote({
  all_dim = names(dim(data))
  ndvi_result = st_apply(data, FUN = function(X,...) {
    (X[8]-X[4])/(X[8]+X[4])
  }, MARGIN = all_dim[-which(all_dim=="band")])
  
  all_dim = names(dim(ndvi_result))
  min_ndvi = st_apply(ndvi_result,FUN = min, MARGIN = all_dim[-which(all_dim=="time")])
  
  min_ndvi
})

script_ndvi = quote({
  all_dim = names(dim(data))
  ndvi_result = st_apply(data, FUN = function(X,...) {
    (X[8]-X[4])/(X[8]+X[4])
  }, MARGIN = all_dim[-which(all_dim=="band")])
  
  ndvi_result
})

script_mult2 = quote({
  data2 = data*(2); data2
})

script_data = quote({
  data
})


# load input for comparison ----------------------------------------------------
inp = jsonlite::fromJSON("/home/split00/openeo-r-udf/examples/data/minimal_example.json")
inp$hypercubes$data
inp$hypercubes$dimensions

# exectue udf ------------------------------------------------------------------

port = 5555 
host = "http://10.8.246.140"

# min_ndvi
res_min_ndvi = jsonlite::fromJSON(send_udf(data = "/home/split00/openeo-r-udf/examples/data/minimal_example.json",
                                               code = script_min_ndvi,
                                               host = host,
                                               port = port),
                                      simplifyVector = TRUE)
res_min_ndvi$hypercubes$data
res_min_ndvi$hypercubes$dimensions

# min_ndvi
res_ndvi = jsonlite::fromJSON(send_udf(data = "/home/split00/openeo-r-udf/examples/data/minimal_example.json",
                                           code = script_ndvi,
                                           host = host,
                                           port = port),
                                  simplifyVector = TRUE)
res_ndvi$hypercubes$data
res_ndvi$hypercubes$dimensions

# min_ndvi
res_mult2 = jsonlite::fromJSON(send_udf(data = "/home/split00/openeo-r-udf/examples/data/minimal_example.json",
                                       code = script_mult2,
                                       host = host,
                                       port = port),
                              simplifyVector = TRUE)
res_mult2$hypercubes$data
res_mult2$hypercubes$dimensions

# only data
res_data = jsonlite::fromJSON(send_udf(data = "/home/split00/openeo-r-udf/examples/data/minimal_example.json",
                                        code = script_data,
                                        host = host,
                                        port = port),
                               simplifyVector = TRUE)
res_data$hypercubes$data
res_data$hypercubes$dimensions



# observations:
# - x and y coordinates are always altered from 50, 60 to 55, 65 and 40, 50 to 45, 55
# - the ndvi examples both produce NA values... when i send it in postman it returns values!
