
json2stars_array = function(json) {
  # raster_collection_tile order: [band][time][y][x]
  
  arr = abind(band=json$data$raster_collection_tiles$data,along=0)
  
  # rearrange dim order
  arr_new = aperm(arr,c(4,3,2,1))
  
  dims = dim(arr_new)
  names(dims) = c("x","y","time","band")
  dim(arr_new) = dims
  
  s = st_as_stars(arr_new)
  start_times = as_datetime(json$data$raster_collection_tiles$start_times[[1]])
  ymin = json$data$raster_collection_tiles[1,]$extent$south
  xmin = json$data$raster_collection_tiles[1,]$extent$west
  dx = json$data$raster_collection_tiles[1,]$extent$width
  dy = json$data$raster_collection_tiles[1,]$extent$height
  s = st_set_dimensions(s,which="x",refsys=json$data$proj, offset=xmin, delta=dx)
  s = st_set_dimensions(s,which="y",refsys=json$data$proj,offset=ymin, delta=dy)
  s = st_set_dimensions(s,which="time",offset=start_times[1],delta=mean(diff(start_times)))
  
  names(s)=c("value")
  
  return(s)
}

stars2json = function(stars_obj, json_in) # , json_out_file = 'udf_response.json')
{
  json_out = json_in[["data"]]
  json_out$proj = st_crs(stars_obj)$proj4string
  band_names = st_get_dimension_values(stars_obj,which="band")
  tot_bands = length(band_names)
  if (tot_bands == 0) {
    #if we have no dedicated band dimension then use the attributes as bands
    band_names = names(stars_obj)
    tot_bands = length(names(stars_obj)) 
  }
  
  bbox = st_bbox(stars_obj)
  x1 = unname(bbox$xmin)
  x2 = unname(bbox$xmax)
  y1 = unname(bbox$ymin)
  y2 = unname(bbox$ymax)
  
  # assuming that x and y are always the spatial dimensions
  delta_x = (x2-x1)/length(st_get_dimension_values(stars_obj,which="x"))
  delta_y = (y1-y2)/length(st_get_dimension_values(stars_obj,which="y"))
  
  
  calc_y = function(ys, bt_df) {
    as.list(as.numeric(subset(bt_df, subset = bt_df$y == ys, select = "layer")[[1]]))
  }
  
  calc_data = function(t, bands, stars_obj) {
    if (length(st_get_dimension_values(stars_obj,which="band")) == 0) bands = NA
    bt_raster = if (!is.na(t)) {
      if (!is.na(bands)) {
        as(stars_obj[, , , bands, t, drop = TRUE], "Raster")
      } else {
        as(stars_obj[, , , t, drop = TRUE], "Raster")
      }
    } else {
      if (!is.na(bands)) {
        as(stars_obj[, , , bands, drop = TRUE], "Raster")
      } else {
        as(stars_obj[, , ], "Raster")
      }
    }
         
    
    bt_df = as.data.frame(bt_raster, xy = TRUE)
    uy = as.list(unique(bt_df[, 2]))
    y_list = lapply(uy, calc_y, bt_df)
  }
  
  if (!is.na(tot_bands)) {
    length(json_out$raster_collection_tiles) = tot_bands
    list_of_bands = list()
    for (bands in 1:tot_bands) {
      band_obj = list()
      
      band_obj$id = band_names[bands]
      
      band_obj$extent = list(north = y2, 
                             south = y1, 
                             west = x1, 
                             east = x2, 
                             height = delta_y, 
                             width = delta_x)
      
      numIntervals = length(st_get_dimension_values(stars_obj,which="time"))
      
      t_start = NA
      t_end = NA
      if (numIntervals > 0) {
        intervals = st_get_dimension_values(stars_obj,which="time")
        
        t_start = int_start(intervals)
        t_end = int_end(intervals)
        band_obj$start_times = as.list(as.character.POSIXt(t_start, format = "%Y-%m-%dT%T %Z"))
        band_obj$end_times = as.list(as.character.POSIXt(t_end, format = "%Y-%m-%dT%T %Z"))
        data = lapply(as.list(1:numIntervals), calc_data, bands, stars_obj)
      } else {
        band_obj$start_times = as.list(t_start)
        band_obj$end_times = as.list(t_end)
        data = lapply(as.list(NA), calc_data, bands, stars_obj)
      }
      
      band_obj$data = data
      
      list_of_bands[[bands]] = band_obj
    }
    
    json_out$raster_collection_tiles = list_of_bands
  } 
  # json_response
  json_out
}
