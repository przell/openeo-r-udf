library(plumber)

server = plumb("api.R")
server$run(host = "0.0.0.0", port = as.integer(Sys.getenv("PLUMBER_PORT")),swagger = TRUE)
