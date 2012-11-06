library(foreach)
library(Kendall)
library(doParallel)
library(RSQLite)

## source code
source("code/R.calc/10code.R")

## get growth rates
source("code/R.calc/01readata.R")

## DB for testing
drv <- SQLite()
con <- dbConnect(drv, dbname = "output/R/test.sqlite")

## data
test <- data.PC.flow[data.PC.flow$Country %in% c(1, 2), ]
test.run <- as.matrix(na.omit(test))
indexes <- combn(1:dim(test.run)[1], 2)

##
dbGetQuery(con, "CREATE Table Correlations (Country1 SMALLINT,
Category1 SMALLINT, Country2 SMALLINT, Category2 SMALLINT, Corr NUMERIC(1,2))")


# in 126
all.time <- system.time({
    foreach(i = 1:10000) %do%{
        cor <- as.numeric(Kendall(test.run[indexes[, i][1], -c(1, 2)],
                                  test.run[indexes[, i][2], -c(1, 2)])$tau)

        data <- data.frame(Country1 = test.run[indexes[, i][1], "Country"],
                           Category1 = test.run[indexes[, i][1], "CategorySub"],
                           Country2 = test.run[indexes[, i][2],"Country"],
                           Category2 = test.run[indexes[, i][2], "CategorySub"],
                           Corr = round(cor, 2))

                sql <- "INSERT INTO Correlations VALUES ($Country1, $Category1,
        $Country2, $Category2, $Corr)"
                dbGetQuery(con, sql, bind.data = data)
    }
})[3]

dbGetQuery(con, "DELETE FROM Correlations")


## without data.frame
sql.base <- "INSERT INTO Correlations ('Country1', 'Category1', 'Country2',
'Category2', 'Corr') VALUES "

# 94
without.time <- system.time({
    foreach(i = 1:10000) %do%{
        cor <- as.numeric(Kendall(test.run[indexes[, i][1], -c(1, 2)],
                                  test.run[indexes[, i][2], -c(1, 2)])$tau)

        sql <- paste(sql.base, "(",paste(test.run[indexes[, i][1], "Country"],
                     test.run[indexes[, i][1], "CategorySub"],
                     test.run[indexes[, i][2], "Country"],
                     test.run[indexes[, i][2], "CategorySub"],
                     Corr = round(cor, 2), sep = ","), ")", sep = "")
        dbGetQuery(con, sql)
    }
})[3]

dbGetQuery(con, "DELETE FROM Correlations")

## use cor function
# 84
cor.time <- system.time({
    foreach(i = 1:10000) %do%{
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

dbGetQuery(con, "DELETE FROM Correlations")

## dbListTables(con)
## dbListFields(con, "Categories")
## dbListFields(con, "Countries")
## dbListFields(con, "Correlations")

## dbDisconnect(con)

## sql <- "SELECT cn1.Country cnn1, cn2.Country cnn2, cat1.Subcategory,
## cat2.Subcategory, cor.*
## FROM Correlations cor, Countries cn1, Countries cn2, Categories cat1,
## Categories cat2
## WHERE cor.Country1 = cn1.id_cn AND cor.Country2 = cn2.id_cn AND
## cor.Category1 = cat1.id_cat AND cor.Category2 = cat2.id_cat AND cor.Corr = 1"
## res <- dbSendQuery(con, sql)
## fetch(res, n=2)
## dbClearResult(res)

## dbGetQuery(con, "DROP TABLE Correlations")

