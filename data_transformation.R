setClass("RasterCollectionTile")
setClass("HyperCube")
setClass("StructuredData")

# raster collection tile -> stars ----
as.RasterCollectionTile.stars = function(from) {
  # raster_collection_tile order: [band][time][y][x]
  arr = abind::abind(band=from$raster_collection_tiles$data,along=0)
  
  # rearrange dim order
  arr_new = aperm(arr,c(4,3,2,1))
  
  dims = dim(arr_new)
  names(dims) = c("x","y","time","band")
  dim(arr_new) = dims
  
  s = stars::st_as_stars(arr_new)
  
  # remember that we deal with "raster" tiles, which means images therefore assume 
  # value ordering of the array in image coodinates (origin in the top left and downwards facing y axis)
  start_times = lubridate::as_datetime(from$raster_collection_tiles$start_times[[1]])
  ymin = from$raster_collection_tiles[1,]$extent$top
  xmin = from$raster_collection_tiles[1,]$extent$left
  dx = from$raster_collection_tiles[1,]$extent$width
  dy = -from$raster_collection_tiles[1,]$extent$height
  
  s = stars::st_set_dimensions(s,which="x", offset=xmin, delta=dx)
  s = stars::st_set_dimensions(s,which="y",offset=ymin, delta=dy)
  #TODO think about having a non continuous time series, e.g. just some intervals
  s = stars::st_set_dimensions(s,which="time",offset=start_times[1],delta=mean(diff(start_times)))
  
  s = stars::st_set_dimensions(s,xy = c("x","y"))
  sf::st_crs(s) = sf::st_crs(from$proj)
  
  names(s)=c("value")
  return(s)
}
setAs(from="RasterCollectionTile",to="stars",def=as.RasterCollectionTile.stars)

# hypercube -> stars ----
as.HyperCube.stars = function(from) {
  
  stars_objs = apply(from$hypercubes,MARGIN = 1,FUN = function(row) {
    dimensions = row$dimensions #potentially multi row
    dim_sizes = sapply(dimensions$coordinates, length)
    names(dim_sizes) = dimensions$name
    
    arr = array(row$data,dim_sizes) #assumption that only one cube is sent
    stars = stars::st_as_stars(arr) 
    
    stars = stars::st_set_dimensions(stars, names=dimensions$name)
    
    for (i in 1:nrow(dimensions)) {
      dimname = dimensions[i,"name"]
      
      if (dimname == "t" || dimname == "time"){
        stars = stars::st_set_dimensions(stars,
                                         which=dimname,
                                         values = as_datetime(dimensions[[i,"coordinates"]]))
      } else {
        stars = stars::st_set_dimensions(stars,
                                         which=dimname,
                                         values = dimensions[[i,"coordinates"]])
      }
      
      
    }
    if (all(c("x","y") %in% names(st_dimensions(stars)))) { # we have spatial dimensions (have to be x and y for now)
      stars = stars::st_set_dimensions(stars,xy = c("x","y"))
      if (grepl(x = tolower(from$proj),pattern = "epsg:")) {
        st_crs(stars) = st_crs(as.numeric(strsplit(tolower(from$proj),"epsg:")[[1]][2]))
      } else {
        st_crs(stars) = st_crs(from$proj)
      }
      
    } else {
      stars = stars::st_set_dimensions(stars,xy = c(NA,NA))
    }
    
    return(stars)
  })
  
  return(stars_objs)
  
}
setAs(from="HyperCube",to="stars",def=as.HyperCube.stars)

# stars -> raster collection tiles ----
as.stars.RasterCollectionTiles = function(from) {
  # predefined dimension names [band][time][y][x]
  order = c("band","time","y","x")
  from = aperm(from, order[order %in% names(dim(from))])
  
  if ("band" %in% names(dim(from))) {
    band_names = stars::st_get_dimension_values(from,"band")
    bands = lapply(seq_along(band_names),function(dimval) {
      list(
        id = as.character(band_names[dimval]),
        data = stars::slice.stars(from,along="band",index=dimval)[[1]]
      )
    })
  } else {
    # use attributes as bands:
    band_names = names(from)
    bands = lapply(seq_along(band_names),function(dimval) {
      list(
        id = as.character(band_names[dimval]),
        data = from[[dimval]]
      )
    })
  }
  
  bands = lapply(bands,function(band) {
    # add time values if available
    if ("time" %in% names(dim(from))) {
      values = stars::st_get_dimension_values(from, which="time")
      
      band$start_times = NA
      band$end_times = NA
      
      if ("Intervals" %in% class(values)) {
        band$start_times = lubridate::int_start(values)
        band$end_times = lubridate::int_end(values)
      }
      
      if ("POSIXt" %in% class(values)) {
        band$start_times = values
        band$end_times = band$start_times+stars::st_dimensions(from)$time$delta
      }
    }
    
    # add extent
    if (all(c("x","y") %in% names(dim(from)))) {
      bbox = stars::st_bbox(from)
      
      band$extent = list(
        top = bbox$ymax,
        bottom = bbox$ymin,
        left = bbox$xmin,
        right=bbox$xmax,
        width = stars::st_dimensions(from)$x$delta,
        height = stars::st_dimensions(from)$y$delta
      )
    }
    return(band)
  })
  
  srs = stars::st_crs(from)$proj4string
  
  return(list(
    id="udf_results",
    proj=srs,
    raster_collection_tiles=bands
  ))
}
setAs(to="RasterCollectionTile",from="stars",def=as.stars.RasterCollectionTiles)


# stars -> hypercube ----
as.stars.HyperCube = function(from) {
  
  dimnames=names(st_dimensions(from))
  dimensions = lapply(dimnames, function (dim) {
    list(name=dim,
         coordinates=st_get_dimension_values(from,which=dim))
    
  })
  # dimensions = data.frame(name = dimnames, coordinates = dimvalues)
  crs = paste0("EPSG:",st_crs(from)$epsg)
  
  if (!is.null(crs) && is.na(crs)) crs = NULL
  return(list(id="udf_result",
              proj = as.character(crs),
              hypercubes = list(
                list(
                  id="value",
                  dimensions=dimensions,
                  data=from[[1]]
                )# assumption that we only allow 1 attribute
              )
  )) 
}
setAs(to="HyperCube",from="stars",def=as.stars.HyperCube)

# simple data -> structured data ----
as.StructuredData = function(from) {
  
  if (!any(class(from) %in% c("list","numeric","integer","character","factor","logical","matrix","data.frame")) || length(from) == 0) {
    stop("Cannot create 'StructuredData' output for given object. Either the data is no simple type or it is NULL.")
  }
  
  if (class(from) %in% c("matrix","data.frame") && nrow(from) > 1) {
    type = "table"
  } else if (!is.null(names(from)) || !is.null(colnames(from))) {
    type = "dict" 
  } else {
    type = "list"
  }
  
  # modify data
  switch(type,
         table={
           # decompose into header, row1, row2, ..., rowN
           data = unname(split(from,row(from)))
           data = lapply(data,unname)
           from = append(list(colnames(from)),data)
           rm(data)
         },
         dict = {
           if (!is.list(from)) {
             names = names(from)
             if (is.null(names)) {
               names = colnames(from)
             }
             
             from = as.list(from)
             names(from) = names
             
           }
         },
         list = {
           
         })
  
  # 'proj' will be NULL, because we have no spatial dimension
  return(list(id="udf_result",
              proj = NULL,
              structured_data = list(
                list(
                  id="value", #TODO name dynamically
                  type=type,
                  data=from
                )
              )
  )) 
}

setAs(from = "list", to="StructuredData",def = as.StructuredData)
setAs(from = "numeric", to="StructuredData",def = as.StructuredData)
setAs(from = "character", to="StructuredData",def = as.StructuredData)
setAs(from = "factor", to="StructuredData",def = as.StructuredData)
setAs(from = "integer", to="StructuredData",def = as.StructuredData)
setAs(from = "logical", to="StructuredData",def = as.StructuredData)
setAs(from = "matrix", to="StructuredData",def = as.StructuredData)
setAs(from = "data.frame", to="StructuredData",def = as.StructuredData)

# structured data -> simple / basic data types ----
as.StructuredData.base = function(from) {
  
  if (class(from$structured_data) == "data.frame") {
    rowwise = unname(split(from$structured_data,row(from$structured_data)))
    return(lapply(rowwise, function(sd){
      switch(sd$type,
             table={
               data = sd$data[[1]]
               colnames = data[1,]
               data=data[-1,]
               colnames(data) = colnames
  
               row_types = sapply(data[1,],function(obj) {
                 class(type.convert(obj,stringsAsFactors=FALSE))
               })
               
               if (!all(row_types == row_types[[1]])) {
                 data = as.data.frame(data)
               }
               
               return(type.convert(data))
             },
             dict = {
               return(sd$data)
             },
             list = {
               return(unlist(sd$data))
             })
    }))
  }
  
  
}
