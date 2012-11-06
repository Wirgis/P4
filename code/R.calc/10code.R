MakeGrowthRates <- function(data, years){
    len <- length(years)
    res <- foreach(ind = 2:len, .combine = cbind) %do% {
        (data[, years[ind]] - data[, years[(ind - 1)]])/data[, years[(ind - 1)]]
    }
    data[, years[-1]] <- res
    data[, years[1]] <- NULL
    data
}
