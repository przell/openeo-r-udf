# @require x:xts

t(apply(x, 2, function(x) c(mean = mean(x), var = var(x))))
