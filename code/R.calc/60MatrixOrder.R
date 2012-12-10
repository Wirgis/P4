library(foreach)
library(doParallel)
library(RSQLite)
library(graph)
library(RBGL)
library(Rgraphviz)
library(corrplot)
library(lattice)

######### Source code, some functions needed
source("code/R.calc/10code.R")

######### Load your database P4.sqlite
drv <- SQLite()
con <- dbConnect(drv, dbname = "output/R/P4.sqlite")

######### Load info about hierarchy level
hierarchy <- read.csv("data/rawdata/hierarchy.csv")

######### parameters
# connection to database
con <- con

# country to select, one of c(1:18)
cn <- 1 # CorrMatWithin
cn1 <- 6 # CorrMatBetween parameter
cn2 <- 17 # CorrMatBetween parameter

# what table from the database to use
table <- "Correlations"

# hierarchy level, one of c(0:4)
level <- 4

# assigns 0 to corrs which satisfy condition:range1 <= abs(corrs) <= range2
range1 <- 0
range2 <- 0.4

# what function from the RBGL package to use for ordering, one of
# c("cuthill.mckee.ordering", "sloan.ordering", "minDegreeOrdering").
# If NULL, then a method supplied by the corrplot parameter is used.
RBGL <- "sloan.ordering"

# parameter only for CorrMatWithin
# method for ordering from corrplot package, one of
# c("AOE", "FPC", "hclust"). For more details see ?corrMatOrder.
# If NULL, then a method supplied by the RBGL parameter is used.
corrplot <- NULL

# parameter only for CorrMatWithin
# the method to be used when corrplot is 'hclust', one of
#                 c("complete", "ward", "single","average", "mcquitty",
#                   "median", "centroid")
#                 For more details see ?corrMatOrder.
hclust.method <- NULL

######### Within country ordering and visualizing
### Selects one country from database, orders a matrix with correlations
### and visualises it.
CorrMatWithin(con = con, cn = cn, table = table, level = level,
              hierarchy = hierarchy, range1 = range1, range2 = range2,
              RBGL = RBGL, corrplot = corrplot, hclust = hclust)

######### Between country ordering and visualizing
### Selects two countries of which cross correlations are inspected,
### orders a matrix with correlations and visualises it.
CorrMatBetween(con = con, cn1 = cn1, cn2 = cn2, table = table, level = level,
               hierarchy = hierarchy, range1 = range1, range2 = range2,
               RBGL = RBGL)
