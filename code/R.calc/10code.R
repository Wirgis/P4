MakeGrowthRates <- function(data, years){
    len <- length(years)
    res <- foreach(ind = 2:len, .combine = cbind) %dopar% {
        (data[, years[ind]] - data[, years[(ind - 1)]])/data[, years[(ind - 1)]]
    }
    data[, years[-1]] <- res
    data[, years[1]] <- NULL
    data
}

ChangeInf <- function(data){
    data.Inf <- which(data == Inf, arr.ind = TRUE)
    for(i in 1:dim(data.Inf)[1])
        data[data.Inf[i, ][1], data.Inf[i, ][2]] = NA
    data
}

ChangeZero <-  function(data){
    data.zero <- which(apply(data[, -c(1, 2)], 1,
                                      function(x) all(x == 0)))
    data <- data[-data.zero, ]
    data
}

ChangeNA <-  function(data){
    data.na <- which(apply(data[, -c(1, 2)], 1,
                           function(x) all(is.na(x))))
    data[-data.na, ]
}
