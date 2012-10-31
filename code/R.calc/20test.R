library(foreach)
library(Kendall)
library(doParallel)
library(RSQLite)

## source code
source("code/R.calc/10code.R")

## get growth rates
source("code/R.calc/01readata.R")

## create the initial DB with two tables: Countries and Categories. Files needed
## data/rawdata/EMI_private/Country.id.csv and
## data/rawdata/EMI_private/Category.id.csv

#source("code/R.calc/30initDB.R") # P4.sqlite

## load P4.sqlite
drv <- SQLite()
con <- dbConnect(drv, dbname = "output/R/P4.sqlite")
## dbListTables(con)
## dbListFields(con, "Categories")
## dbListFields(con, "Countries")

## data.PC.flow
## dbGetQuery(con, "CREATE Table Correlations (Country1 SMALLINT,
## Category1 SMALLINT, Country2 SMALLINT, Category2 SMALLINT, Corr NUMERIC(1,2))")

test <- data.PC.flow[data.PC.flow$Country %in% c(1, 2), ]
test.run <- as.matrix(na.omit(test))
indexes <- combn(1:dim(test.run)[1], 2)

sequential.time <- system.time({
    foreach(i = 1:dim(indexes)[2]) %do%{
        cor <- cor(test.run[indexes[, i][1], -c(1, 2)],
                   test.run[indexes[, i][2], -c(1, 2)], method = "kendall")

        sql <- paste(sql.base, "(",paste(test.run[indexes[, i][1], "Country"],
                     test.run[indexes[, i][1], "CategorySub"],
                     test.run[indexes[, i][2], "Country"],
                     test.run[indexes[, i][2], "CategorySub"],
                     Corr = round(cor, 2), sep = ","), ")", sep = "")
        dbGetQuery(con, sql)
    }
})[3]








