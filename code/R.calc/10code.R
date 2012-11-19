MakeGrowthRates <- function(data, years){
    len <- length(years)
    res <- foreach(ind = 2:len, .combine = cbind) %do% {
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

RemoveZero <-  function(data){
    data.zero <- which(apply(data[, -c(1, 2, 3)], 1,
                                      function(x) all(x == 0)))
    data <- data[-data.zero, ]
    data
}

RemoveNA <-  function(data){
    data.na <- which(apply(data[, -c(1, 2, 3)], 1,
                           function(x) all(is.na(x))))
    data[-data.na, ]
}

SetLayout <- function(ncol, nrow){
  Layout <- grid.layout(nrow = nrow, ncol = ncol,
                        widths = unit(rep(1, ncol),
                                      rep("null", (ncol + 1))))
  grid.newpage()
  pushViewport(viewport(layout = Layout))
}

subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

InsertCorr <- function(data, sql.base, indexes, con, range1, range2){
    foreach(i = 1:dim(indexes)[2]) %do%{
        print(i)
        cor <- cor(data[indexes[, i][1], -c(1:2)],
                   data[indexes[, i][2], -c(1:2)], method = "kendall",
                   use = "complete.obs")
        if(range1 <= abs(cor) & abs(cor) <= range2) {
            sql <- paste(sql.base, "(",paste(data[indexes[, i][1], "Country"],
                                             data[indexes[, i][1],
                                                  "CategorySub"],
                                             data[indexes[, i][2], "Country"],
                                             data[indexes[, i][2],
                                                  "CategorySub"],
                                             Corr = round(cor, 2), sep = ","),
                         ")", sep = "")
            dbGetQuery(con, sql)
        }
    }
}


InsertCorr2 <- function(data1, data2, sql.base, con, range1, range2){
    for(i in 1:dim(data1)[1]){
        for(j in 1:dim(data2)[1]){
            print(paste(i, j))
            cor <- cor(data1[i, -c(1, 2)], data2[j, -c(1, 2)],
                       method = "kendall", use = "complete.obs")
            if(range1 <= abs(cor) & abs(cor) <= range2) {
                sql <- paste(sql.base, "(",paste(data1[i, "Country"],
                                                 data1[i, "CategorySub"],
                                                 data2[j, "Country"],
                                                 data2[j, "CategorySub"],
                                                 Corr = round(cor, 2),
                                                 sep = ","),
                             ")", sep = "")
                dbGetQuery(con, sql)
            }
        }
    }
}
