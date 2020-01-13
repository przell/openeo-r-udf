# @require obj:xts

# calculate the mean of the time series

apply(obj, 1, function(x) {
  (x[8]-x[4]) / (x[8]+x[4])
})

