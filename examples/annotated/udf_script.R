# @require x:stars

all_dim = names(dim(x))
ndvi_result = st_apply(x, FUN = function(X,...) {
  (X[8]-X[4])/(X[8]+X[4])
}, MARGIN = all_dim[-which(all_dim=="band")])

all_dim = names(dim(ndvi_result))
min_ndvi = st_apply(ndvi_result,FUN = min, MARGIN = all_dim[-which(all_dim=="time")])

min_ndvi