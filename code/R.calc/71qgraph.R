library(qgraph)
library(foreach)
library(RSQLite)
library(reshape)
source("code/R.calc/10code.R")

drv <- SQLite()
con <- dbConnect(drv, dbname = "output/R/P4.sqlite")

######### Graph that has one target node and n nodes with highest correlations
###
### con - connection to database;
### node - specifies the target node. A character vector with two elements:
###        "Country" and "Category"
### n - integer, n nodes with highest correlations
### table - that table from databse to use
### egde.labels - logical indicating if edge labels should be printed
### minimum -  edges with absolute weights under this value are omitted
### center - ligical indicating if the target node should be placed in the
###          center
### same.country - logical indicating if the most correlated nodes could be
###                of the same country,
###                if FALSE then the most correlated nodes will be searched
###                among all nodes expect ones that indicates the country of
###                the target node
###                if TRUE then the most correlated ones are searched among all
GraphOne(con, node = c("Country" = 1, "Category" = 350), n = 50,
         table = "Correlations", edge.labels = FALSE, minimum = 0.45,
         center = TRUE, same.country = TRUE)








