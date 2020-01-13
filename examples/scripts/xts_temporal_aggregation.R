# @require x:xts

a = aggregate(x, cut(index(x), breaks = "months"), max)
xts(as.matrix(a), as.POSIXct(index(a)))
