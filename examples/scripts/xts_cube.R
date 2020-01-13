# @require obj:xts

# band, x, y
# 13, 300, 300

indexing = function(x,y,band) {
  index = 300 * 13 * (y-1) + 13 * (x-1)+ band
  return(index)
}

results = NULL
for(x in 1:100) {
  for (y in 1:100) {
    single_ts = obj[,indexing(x,y,1:13)]
    intermediate = apply(single_ts, 1, function(x) {
      (x[8]-x[4]) / (x[8]+x[4])
    })
    if (is.null(results)) {
      results = unname(as.xts(intermediate))
    } else {
      results = unname(cbind(results,as.xts(intermediate)))
    }

  }
}
results
