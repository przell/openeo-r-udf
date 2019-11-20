if (file.exists("/opt/openeo-r-udf/libraries.json")) {
  
  libs = jsonlite::read_json("libraries.json")
  
  # cran packages
  if (!is.null(libs$cran)) {
    lib_names = lapply(libs$cran, function(entry)entry$Package)
    lib_version = lapply(libs$cran, function(entry){
      version=entry$Version
      if (is.null(version)) return(NULL)
      
      if (tolower(version) %in% c("latest","last","new","newest")) return(NULL)
      
      return(version)
    })
    
    for (i in seq_along(lib_names)) {
    if (length(lib_version[[i]]) == 0) {
  	remotes::install_version(package = lib_names[[i]],repos="https://cran.rstudio.com/")
    } else {
  	remotes::install_version(package = lib_names[[i]],repos="https://cran.rstudio.com/", version=lib_version[[i]])
    }
      
    }
    
  }
  
  if (!is.null(libs$github)) {
    
  }
}