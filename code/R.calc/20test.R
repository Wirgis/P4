library(foreach)
library(doParallel)
library(RSQLite)
library(ggplot2)
library(grid)
library(lattice)

## source code
source("code/R.calc/10code.R")

## get growth rates
#source("code/R.calc/01readata.R") # USD
source("code/R.calc/02readata.R") # local currency

## set hierarchy level
data <- data.loc[data.loc$Hierarchy.Level == 4, ]
data$Hierarchy.Level <- NULL

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
test.run <- data[data$Country == 1, ]
test.run <- as.matrix(test.run)
interest <- sample(test.run[, "CategorySub"], 218)
ind <- combn(interest, 2)
indexes <- apply(ind, 2, function(x)
                 c(which(test.run[, "CategorySub"] == x[1]),
                   which(test.run[, "CategorySub"] == x[2])))

#indexesexes <- combn(1:dim(test.run)[1], 2)

dbGetQuery(con, "DELETE from Correlations")

sql.base <- "INSERT INTO Correlations ('Country1', 'Category1', 'Country2',
'Category2', 'Corr') VALUES "

range1 <- 0
range2 <- 1

#foreach(i = 1:dim(indexes)[2]) %do%{
foreach(i = 1:dim(indexes)[2]) %do%{
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


## ## neblogai
## library("qgraph")
## data(big5)
## data(big5groups)
## qgraph(cor(big5),minimum=0.25,cut=0.4,vsize=2,groups=big5groups,legend=TRUE,borders=FALSE)
## title("Big 5 correlations",line=-2,cex.main=2)


## library(gclus)
## library(cluster)
## library(hexbin)

## g <- ggobi(iris)
## clustering <- hclust(dist(iris[,1:4]),
## method="average")
## glyph_colour(g[1]) <- cutree(clustering, 3)



## g <- ggobi(mtcars)
## display(g[1], vars=list(X=4, Y=5))


## hierarchy <- read.csv("data/rawdata/hierarchy.csv")
## level <- 4



