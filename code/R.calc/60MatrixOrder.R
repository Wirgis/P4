library(foreach)
library(doParallel)
library(RSQLite)
library(graph)
library(RBGL)
library(Rgraphviz)
library(corrplot)

######### Source code, some functions needed
source("code/R.calc/10code.R")

######### Load your database P4.sqlite
drv <- SQLite()
con <- dbConnect(drv, dbname = "output/R/P4.sqlite")

######### Load info about hierarchy level
hierarchy <- read.csv("data/rawdata/hierarchy.csv")

######### Within country ordering and visualizing
### Selects one country from database, orders a matrix with correlations
### and visualises it.
###
### Function CorrMatWithin with params:
###
### con - connection to database;
### cn - country to select, one of c(1:18)
### table - that table from database to use
### level - hierarchy level, one of c(0:4)
### range1, range2 - assigns 0 to corrs which satisfy condition:
###                  range1 <= abs(corrs) <= range2
### RBGL - what function from the RBGL package to use for ordering, one of
###        c("cuthill.mckee.ordering", "sloan.ordering", "minDegreeOrdering").
###        If NULL, then other methods are used
### corrplot - method for ordering from corrplot package, one of
###            c("AOE", "FPC", "hclust"). For more details see ?corrMatOrder.
###            If NULL, then other methods are used.
### hclust.method - the method to be used when corrplot is 'hclust', one of
###                 c("complete", "ward", "single","average", "mcquitty",
###                   "median", "centroid")
###                 For more details see ?corrMatOrder.

CorrMatWithin(con, cn = 1, table = "Correlations", level = 4,
              hierarchy = hierarchy, range1 = 0, range2 = 0.4,
              RBGL = "sloan.ordering")

CorrMatWithin(con, cn = 1, table = "Correlations", level = 4,
              hierarchy = hierarchy, range1 = 0, range2 = 0.4, corrplot = "AOE")

CorrMatWithin(con, cn = 1, table = "Correlations", level = 4,
              hierarchy = hierarchy, range1 = 0, range2 = 0.4,
              corrplot = "hclust", hclust.method = "ward")

######### Between country ordering and visualizing
### Selects two countries of which cross correlations are inspected,
### orders a matrix with correlations and visualises it.
###
### Function CorrMatBetween with params:
###
### con - connection to database;
### cn1 - country 1, one of c(1:18)
### cn2 - country 2, one of c(1:18)
### table - that table from databse to use
### level - hierarchy level, one of c(0:4)
### range1, range2 - assigns 0 to corrs which satisfy
###                  range1 <= abs(corrs) <= range2
### RBGL - what function from the RBGL package to use for ordering, one of
###        c("cuthill.mckee.ordering", "sloan.ordering", "minDegreeOrdering").

CorrMatBetween(con, cn1 = 6, cn2 = 17, table = "Correlations", level = 4,
               hierarchy = hierarchy, range1 = 0, range2 = 0.4,
               RBGL = "cuthill.mckee.ordering")
