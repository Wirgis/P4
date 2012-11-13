library(foreach)
library(doParallel)
library(RSQLite)
library(ggplot2)
library(grid)

## source code
source("code/R.calc/10code.R")

## get growth rates
#source("code/R.calc/01readata.R") # USD
source("code/R.calc/02readata.R") # local currency

## set hierarchy level
data <- data.loc[data.loc$Hierarchy.Level == 4, ]

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

##
test.run <- as.matrix(data)
indexes <- combn(1:dim(test.run)[1], 2)

sql.base <- "INSERT INTO Correlations ('Country1', 'Category1', 'Country2',
'Category2', 'Corr') VALUES "

range1 <- 0.25
range2 <- 1

#foreach(i = 1:dim(indexes)[2]) %do%{
foreach(i = 1:100) %do%{
    cor <- cor(test.run[indexes[, i][1], -c(1, 2)],
               test.run[indexes[, i][2], -c(1, 2)], method = "kendall",
               use = "complete.obs")
    if(range1 <= abs(cor) & abs(cor) <= range2) {
        sql <- paste(sql.base, "(",paste(test.run[indexes[, i][1], "Country"],
                                         test.run[indexes[, i][1],
                                                  "CategorySub"],
                                         test.run[indexes[, i][2], "Country"],
                                         test.run[indexes[, i][2],
                                                  "CategorySub"],
                                         Corr = round(cor, 2), sep = ","), ")",
                     sep = "")
        dbGetQuery(con, sql)
    }
}

## dbGetQuery(con, "SELECT * from Correlations")

## # graph
## library("graph")
## library("RBGL")
## library("Rgraphviz")


## try <- dbGetQuery(con, "SELECT * from Correlations limit 10")

## from <- paste(try[, "Country1"], try[, "Category1"])
## to <- paste(try[, "Country2"], try[, "Category2"])
## W <-  try[, "Corr"]

## ft <- cbind(from, to)
## g <- ftM2graphNEL(ft, edgemode = "undirected", W = W)

## nodes(g)
## edges(g)
## adj(g, "1 601")
## degree(g)
## acc(g, "1 601")

## as(g, "matrix")


## ## sparse matrix ordering
## a <- cuthill.mckee.ordering(g)
## as(g, "matrix")[rev(a[[1]]), rev(a[[1]])]

## b <- minDegreeOrdering(g)
## as(g, "matrix")[b[[1]], b[[1]]]

## c <- sloan.ordering(g)
## as(g, "matrix")[c[[1]], c[[1]]]


## ## betweenness centrality and clustering
## betweenness.centrality.clustering(g)



