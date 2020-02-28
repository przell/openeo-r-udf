# set dummy S4 classes for data types from the udf api
setClass("RasterCollectionTile")
setClass("HyperCube")
setClass("StructuredData")
setClass("DataCollection") # new one!

# raster collection tile -> stars ----
as.stars.RasterCollectionTile = function(from) {
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
setAs(from="RasterCollectionTile",to="stars",def=as.stars.RasterCollectionTile)

# hypercube -> stars ----
as.stars.HyperCube = function(from) {

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
        # shift the values by half the resolution, when we differ the extent and cell values from coordinate labels
        if (any(dimname == c("x","y"))) {
          resolution = mean(diff(dimensions[[i,"coordinates"]]),na.rm = TRUE)
          dimensions[[i,"coordinates"]] = dimensions[[i,"coordinates"]] - (resolution/2)
          
        }
        
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
setAs(from="HyperCube",to="stars",def=as.stars.HyperCube)

# stars -> raster collection tiles ----
as.RasterCollectionTiles.stars = function(from) {
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
setAs(to="RasterCollectionTile",from="stars",def=as.RasterCollectionTiles.stars)


# stars -> hypercube ----
as.HyperCube.stars = function(from) {
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
setAs(to="HyperCube",from="stars",def=as.HyperCube.stars)

# stars -> data_collection ----
as.data_collection.stars = function(x) {
  if ("stars" %in% class(x)) {
    x = list(x)
  }
  
  if (!all(sapply(x,function(obj)"stars"==class(obj)))) stop("Not all elements in x as a list are stars objects.")
  
  result = list()
  result$geometry_collection = list()
  result$type = "DataCollection"
  
  # meta data element
  md = list()
  md$name="r-udf-result"
  md$description="a result"
  md$creator="R-UDF-service"
  md$creation_time=format(now(),format="%Y%m%dT%H%M%SZ")
  #TODO this needs to be adapted
  md$number_of_object_collections = 0 
  md$number_of_geometries = 0
  md$number_of_variables = 0
  md$number_of_time_stamps = 0
  
  data_cubes = lapply(1:length(x), function(index) {
    obj = x[[index]]
    # create data cube descriptions
    dc = list()
    dc$name = "data_cube"
    dc$description = "structural description of the dimensionality of the result"
    dc$dim = dimnames(obj)
    dc$size = unname(dim(obj))
    #TODO has to be set...
    dc$variable_collection = index-1
    
    dims = st_dimensions(obj)
    dim_models = lapply(names(dims), function(key) {
      d = dims[[key]]
      
      if ("POSIXct" %in% class(d$values)) {
        d$values = format(d$values,format="%Y%m%dT%H%M%SZ")
      }
      
      if (!length(d$values) == 0 && !is.na(d$values)) {
        ex = d$values[c(d$from,d$to)]
        vals = d$values
        nums = length(d$values)
      } else {
        ex = d$offset + c(d$from-1,d$to) *d$delta
        vals = list()
        nums = d$to
      }
      
      type = switch(key,
                    x="spatial",
                    y="spatial",
                    z="spatial",
                    t = "temporal",
                    band="bands",
                    "other")
      
      
      # TODO correct this / categorical values or leave out
      unit = switch(type,
                    spatial={
                      switch(key,
                             x="m",
                             y="m",
                             z="m")  
                    },
                    temporal = "ISO8601",
                    band="nm",
                    {
                      if (!is.na(d$refsys) && d$refsys == "POSIXct") {
                        "ISO8601"
                      } else {
                        NA
                      }
                    })
      
      if (type == "spatial" && !is.na(d$refsys)) {
        crs=st_crs(d$refsys)
        refsys = crs$epsg
      } else if (type == "temporal"){
        refsys = "gregorian"
      } else {
        refsys = NA
      }
      
      
      dim = list(
        extent=ex,
        values = vals,
        number_of_cells = nums,
        axis=key,
        type=type,
        unit = unit,
        reference_system = refsys
      )
      
      return(dim)
    })
    
    dc$dimensions = dim_models
    
    return(dc)
  })
  
  result$object_collections = list()
  result$object_collections$data_cubes = data_cubes
  
  result$variable_collections = list()
  # create variables
  vars = lapply(1:length(x), function(index) {
    obj = x[[index]]
    
    vcoll = list(
      name="cube_data",
      size = unname(dim(obj)),
      number_of_variables = length(names(obj)),
      variables = lapply(names(obj), function(variable_name) {
        data = obj[[variable_name]]
        variable = list(
          name = variable_name,
          values = as.vector(data)
        )
        return(variable)
      })
    )
    return(vcoll)
  })
  
  result$variable_collections = vars
  result$metadata$number_of_variables = length(result$variable_collections)
  
  class(result) = "DataCollection"
  
  return(result)
}
setAs(to="DataCollection",from="stars",def=as.data_collection.stars)

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
