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
  start_times = lubridate::as_datetime(from$raster_collection_tiles$start_times[[1]])
  ymin = from$raster_collection_tiles[1,]$extent$bottom
  xmin = from$raster_collection_tiles[1,]$extent$left
  dx = from$raster_collection_tiles[1,]$extent$width
  dy = from$raster_collection_tiles[1,]$extent$height
  
  s = stars::st_set_dimensions(s,which="x",refsys=from$proj, offset=xmin, delta=dx)
  s = stars::st_set_dimensions(s,which="y",refsys=from$proj,offset=ymin, delta=dy)
  #TODO think about having a non continuous time series, e.g. just some intervals
  s = stars::st_set_dimensions(s,which="time",offset=start_times[1],delta=mean(diff(start_times)))
  
  names(s)=c("value")
  return(s)
}
setAs(from="RasterCollectionTile",to="stars",def=as.RasterCollectionTile.stars)

# hypercube -> stars ----
as.HyperCube.stars = function(from) {
  dimensions = from$hypercubes[1,"dimensions"][[1]]
  dim_sizes = sapply(dimensions$coordinates, length)
  names(dim_sizes) = dimensions$name
  
  arr = array(from$hypercubes[1,"array"][[1]],dim_sizes) #assumption that only one cube is sent
  stars = stars::st_as_stars(arr) 
  
  stars = stars::st_set_dimensions(stars, names=dimensions$name)
  for (i in 1:nrow(dimensions)) {
    dimname = dimensions[i,"name"]
    stars = stars::st_set_dimensions(stars,
                                     which=dimname,
                                     values = dimensions[[i,"coordinates"]])
    
  }
  stars = stars::st_set_dimensions(stars,xy = c("x","y")) #hardcoded
  st_crs(stars) = st_crs(from$proj)
  
  
  return(stars)
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
  dimvalues = lapply(dimnames, function (dim) {
    st_get_dimension_values(from,which=dim)
  })
  dimensions = tibble(name = dimnames, coordinates = dimvalues)
  
  crs = st_crs(from)$proj4string
  if (!is.null(crs) && is.na(crs)) crs = NULL
  return(list(id="udf_result",
              proj = as.character(crs),
              hypercubes = list(
                list(
                  id="value",
                  dimensions=dimensions,
                  array=from[[1]]
                )# assumption that we only allow 1 attribute
              )
             )) 
}
setAs(to="HyperCube",from="stars",def=as.stars.HyperCube)
