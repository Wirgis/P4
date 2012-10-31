library(unitls)
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
      cor <- as.numeric(Kendall(test.run[indexes[, i][1], -c(1, 2)],
                                test.run[indexes[, i][2], -c(1, 2)])$tau)

      data <- data.frame(Country1 = test.run[indexes[, i][1], "Country"],
                         Category1 = test.run[indexes[, i][1], "CategorySub"],
                         Country2 = test.run[indexes[, i][2],"Country"],
                         Category2 = test.run[indexes[, i][2], "CategorySub"],
                         Corr = round(cor, 2))

      sql <- "INSERT INTO Correlations VALUES ($Country1, $Category1, $Country2,
 $Category2, $Corr)"
      ## dbBeginTransaction(con) # 823686 in 3 hour and 46 min
      ## dbGetPreparedQuery(con, sql, bind.data = data)
      ## dbCommit(con)

      dbGetQuery(con, sql, bind.data = data) # 823686 in 1 hour and 40 min
      ##dbSendQuery(con, sql, bind.data = data)

  }
})


dbListTables(con)
dbListFields(con, "Categories")
dbListFields(con, "Countries")
dbListFields(con, "Correlations")

dbDisconnect(con)

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





