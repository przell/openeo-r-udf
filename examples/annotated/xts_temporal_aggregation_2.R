# @require x:xts

a = aggregate(x, cut(index(x), breaks = "months"), median)
result = xts(as.matrix(a), as.POSIXct(index(a)))
(result[,8] - result[,4]) / (result[,8] + result[,4])
