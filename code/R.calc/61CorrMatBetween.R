library(foreach)
library(doParallel)
library(RSQLite)
library(graph)
library(RBGL)
library(Rgraphviz)
library(corrplot)

######### Source code, some functions needed;
source("code/R.calc/10code.R")

########## Choose the script for reading the data you need:
#  01readata.R - USD data, output :
#    1: data.flow -  US million, year on year exchange rate;
#    2: data.fix - US million, fixed 2011 exchange rate;
#    3: data.PC.flow - US per capita, year on year exchange rate;
#    4: data.PC.fix - US per capita, fixed 2011 exchange rate;
#    5: data.PHH.flow - US per household, year on year exchange rate;
#    6: data.PHH.fix - US per household, fixed 2011 exchange rate;

#  02readata.R - local currency data, output :
#    1: data.loc -  local currency;
#    2: data.PC - local currency per capita;
#    3: data.PHH - local currency per household;

#source("code/R.calc/01readata.R") # USD
source("code/R.calc/02readata.R") # local currency

data.name <- "data_loc" # will be used below, should match the data.
data <- data.loc

######### Create the initial DB with two tables: Countries and Categories.
# Source this script only if you don't have P4.sqlite file in the
# output/R folder.
# Files needed which you needs to be added into the EMI_private folder:
#    1. data/rawdata/EMI_private/Country.id.csv
#    2. data/rawdata/EMI_private/Category.id.csv
#source("code/R.calc/30initDB.R") # P4.sqlite

######### Load your database P4.sqlite
drv <- SQLite()
con <- dbConnect(drv, dbname = "output/R/P4.sqlite")
table.names <- dbListTables(con) # tables
## dbListFields(con, "Categories") # columns in the "Categories" table
## dbListFields(con, "Countries") # columns in the "Categories" table

## Corrs between countries
data.sub <- as.matrix(data[data$CategorySub == 1, -3])
ind <- combn(1:dim(data.sub)[1], 2)

sql1 <- "CREATE Table"
sql2 <- paste("Corr" , "BetweenCN", sep = "_")
sql3 <- "(Country1 SMALLINT, Category1 SMALLINT, Country2 SMALLINT,
Category2 SMALLINT, Corr NUMERIC(1,2))"
sql <- paste(sql1, sql2, sql3)
dbGetQuery(con, sql)
sql.base <- "INSERT INTO Corr_BetweenCN ('Country1', 'Category1', 'Country2',
'Category2', 'Corr') VALUES "

InsertCorr(data = data.sub, sql.base = sql.base, indexes = ind, con = con,
           range1 = 0, range2 = 1)

######### Choose a hierarchy level you need;
#unique(data$Hierarchy.Level)
data <- data[data$Hierarchy.Level == 4, ]
data$Hierarchy.Level <- NULL
data <- as.matrix(data)

######### finding some countries which are correlated
try <- dbGetQuery(con, "SELECT * from Corr_BetweenCN")
from <- paste(try[, "Country1"], try[, "Category1"])
to <- paste(try[, "Country2"], try[, "Category2"])
W <-  try[, "Corr"]

ft <- cbind(from, to)
g <- ftM2graphNEL(ft, edgemode = "undirected", W = W)
g.mat <- as(g, "matrix")

col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F",
                           "cyan", "#007FFF", "blue","#00007F"))
pdf("output/notes/CorrMatrix/All.pdf")
corrplot(g.mat, method = "square",
         title = "All Countries, Category = Industrial (Entire Economy)",
         col = col4(200), order = "AOE")
dev.off()


######### Country pairs
# 6 & 17; 6 & 5; 6 & 9
data.6 <- data[data[, "Country"] %in% 6, ]
data.17 <- data[data[, "Country"] %in% 17, ]
data.5 <- data[data[, "Country"] %in% 5, ]
data.9 <- data[data[, "Country"] %in% 9, ]

## colours
pal <- colorRampPalette(c("red", "yellow"), space = "rgb")

# 6 & 17
sql2 <- paste("Corr" , "CN6_17", sep = "_")
sql <- paste(sql1, sql2, sql3)
dbGetQuery(con, sql)
sql.base <- "INSERT INTO Corr_CN6_17 ('Country1', 'Category1', 'Country2',
'Category2', 'Corr') VALUES "
subs.6 <- unique(data.6[, "CategorySub"])
subs.17 <- unique(data.17[, "CategorySub"])

InsertCorr2(data1 = data.6,
            data2 = data.17[data.17[, "CategorySub"] %in% subs.6,], sql.base,
            con, range1 = 0, range2 = 1)

try <- dbGetQuery(con, "SELECT * from Corr_CN6_17")
from <- paste(try[, "Country1"], try[, "Category1"])
to <- paste(try[, "Country2"], try[, "Category2"])
W <-  try[, "Corr"]

ft <- cbind(from, to)
g <- ftM2graphNEL(ft, edgemode = "directed", W = W)
g.mat <- as(g, "matrix")

ord1 <- cuthill.mckee.ordering(g)$`reverse cuthill.mckee.ordering`
ordm1 <- g.mat[ord1, ord1]

pdf("output/notes/CorrMatrix/Germany_UK_cut.pdf")
levelplot(ordm1, main = "Germany_UK", xlab="", ylab="",
          col.regions=pal(100), cuts = 99)
dev.off()

## ord2 <- minDegreeOrdering(g)$inverse_permutation
## ordm2 <- g.mat[ord2, ord2]

## pdf("output/notes/CorrMatrix/Germany_UK_min.pdf")
## levelplot(ordm2, main = "Germany_UK", xlab="", ylab="",
##           col.regions=pal(100), cuts = 99)
## dev.off()

## ord3 <- sloan.ordering(g)$sloan.ordering
## ordm3 <- g.mat[ord3, ord3]

## pdf("output/notes/CorrMatrix/Germany_UK_sloan.pdf")
## levelplot(ordm3, main = "Germany_UK", xlab="", ylab="",
##           col.regions=pal(100), cuts = 99)
## dev.off()

# 6 & 5
sql2 <- paste("Corr" , "CN6_5", sep = "_")
sql <- paste(sql1, sql2, sql3)
dbGetQuery(con, sql)
sql.base <- "INSERT INTO Corr_CN6_5 ('Country1', 'Category1', 'Country2',
'Category2', 'Corr') VALUES "
subs.6 <- unique(data.6[, "CategorySub"])
subs.5 <- unique(data.5[, "CategorySub"])

InsertCorr2(data1 = data.6,
            data2 = data.5[data.5[, "CategorySub"] %in% subs.6,], sql.base,
            con, range1 = 0, range2 = 1)

try <- dbGetQuery(con, "SELECT * from Corr_CN6_5")
from <- paste(try[, "Country1"], try[, "Category1"])
to <- paste(try[, "Country2"], try[, "Category2"])
W <-  try[, "Corr"]

ft <- cbind(from, to)
g <- ftM2graphNEL(ft, edgemode = "directed", W = W)
g.mat <- as(g, "matrix")

ord1 <- cuthill.mckee.ordering(g)$`reverse cuthill.mckee.ordering`
ordm1 <- g.mat[ord1, ord1]

pdf("output/notes/CorrMatrix/Germany_France_cut.pdf")
levelplot(ordm1, main = "Germany_France", xlab="", ylab="",
          col.regions=pal(100), cuts = 99)
dev.off()


# 6 & 9
sql2 <- paste("Corr" , "CN6_9", sep = "_")
sql <- paste(sql1, sql2, sql3)
dbGetQuery(con, sql)
sql.base <- "INSERT INTO Corr_CN6_9 ('Country1', 'Category1', 'Country2',
'Category2', 'Corr') VALUES "
subs.6 <- unique(data.6[, "CategorySub"])
subs.9 <- unique(data.9[, "CategorySub"])

InsertCorr2(data1 = data.6[data.6[, "CategorySub"] %in% subs.9,],
            data2 = data.9, sql.base,
            con, range1 = 0, range2 = 1)

try <- dbGetQuery(con, "SELECT * from Corr_CN6_9")
from <- paste(try[, "Country1"], try[, "Category1"])
to <- paste(try[, "Country2"], try[, "Category2"])
W <-  try[, "Corr"]

ft <- cbind(from, to)
g <- ftM2graphNEL(ft, edgemode = "directed", W = W)
g.mat <- as(g, "matrix")

ord1 <- cuthill.mckee.ordering(g)$`reverse cuthill.mckee.ordering`
ordm1 <- g.mat[ord1, ord1]

pdf("output/notes/CorrMatrix/Germany_Italy_cut.pdf")
levelplot(ordm1, main = "Germany_Italy", xlab="", ylab="",
          col.regions=pal(100), cuts = 99)
dev.off()
