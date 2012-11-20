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

######### Choose a hierarchy level you need;
#unique(data$Hierarchy.Level)
data <- data[data$Hierarchy.Level == 4, ]
data$Hierarchy.Level <- NULL
data <- as.matrix(data)

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

## Take sample of countries
CN <- unique(data$Country)
cn <- sample(CN, 3) # 7, 3, 1

## Creating tables
sql1 <- "CREATE Table"
sql2 <- paste("CorrCN" , cn, sep = "_")
sql3 <- "(Country1 SMALLINT, Category1 SMALLINT, Country2 SMALLINT,
Category2 SMALLINT, Corr NUMERIC(1,2))"
sql <- paste(sql1, sql2, sql3)
for(s in sql) dbGetQuery(con, s)

######### Form INSERT query
sql1 <- "INSERT INTO"
sql2 <- paste("CorrCN" , cn, sep = "_")
sql3 <- "('Country1', 'Category1', 'Country2', 'Category2', 'Corr') VALUES "
sql.base <- paste(sql1, sql2, sql3)

## data
test.run1 <- data[data[, "Country"] %in% cn[1], ]
test.run1 <- as.matrix(test.run1)

test.run2 <- data[data[, "Country"] %in% cn[2], ]
test.run2 <- as.matrix(test.run2)

test.run3 <- data[data[, "Country"] %in% cn[3], ]
test.run3 <- as.matrix(test.run3)

interest1 <- test.run1[, "CategorySub"]
ind1 <- combn(interest1, 2)
indexes1 <- apply(ind1, 2, function(x)
                 c(which(test.run1[, "CategorySub"] == x[1]),
                   which(test.run1[, "CategorySub"] == x[2])))

interest2 <- test.run2[, "CategorySub"]
ind2 <- combn(interest2, 2)
indexes2 <- apply(ind2, 2, function(x)
                 c(which(test.run2[, "CategorySub"] == x[1]),
                   which(test.run2[, "CategorySub"] == x[2])))

interest3 <- test.run3[, "CategorySub"]
ind3 <- combn(interest3, 2)
indexes3 <- apply(ind3, 2, function(x)
                 c(which(test.run3[, "CategorySub"] == x[1]),
                   which(test.run3[, "CategorySub"] == x[2])))


######### Choose the range. Only correlations that satisfy the condition are
# inserted: range1 <= abs(cor) & abs(cor) <= range2
range1 <- 0.25
range2 <- 1

######### The cycle for inserting correlations. This is doing sequentially.
InsertCorr(data = test.run1, sql.base = sql.base[1], indexes = indexes1,
           con = con, range1 = range1, range2 = range2)
InsertCorr(data = test.run2, sql.base = sql.base[2], indexes = indexes2,
           con = con, range1 = range1, range2 = range2)
InsertCorr(data = test.run3, sql.base = sql.base[3], indexes = indexes3,
           con = con, range1 = range1, range2 = range2)

## graph, undirected weighted graph

# India
try <- dbGetQuery(con, "SELECT * from CorrCN_7")
from <- paste(try[, "Country1"], try[, "Category1"])
to <- paste(try[, "Country2"], try[, "Category2"])
W <-  try[, "Corr"]
ft <- cbind(from, to)
g <- ftM2graphNEL(ft, edgemode = "undirected", W = W)
g.mat1 <- as(g, "matrix")

# Canada
try <- dbGetQuery(con, "SELECT * from CorrCN_3")
from <- paste(try[, "Country1"], try[, "Category1"])
to <- paste(try[, "Country2"], try[, "Category2"])
W <-  try[, "Corr"]
ft <- cbind(from, to)
g <- ftM2graphNEL(ft, edgemode = "undirected", W = W)
g.mat2 <- as(g, "matrix")

# Australia
try <- dbGetQuery(con, "SELECT * from CorrCN_1")
from <- paste(try[, "Country1"], try[, "Category1"])
to <- paste(try[, "Country2"], try[, "Category2"])
W <-  try[, "Corr"]
ft <- cbind(from, to)
g <- ftM2graphNEL(ft, edgemode = "undirected", W = W)
g.mat3 <- as(g, "matrix")

## colors
col4 <- colorRampPalette(c("#7F0000","red","#FF7F00","yellow","#7FFF7F",
                           "cyan", "#007FFF", "blue","#00007F"))

## the angular order of the eigenvectors
a11 <- corrplot(g.mat1, method = "square", col = col4(200), order = "AOE")
pdf("output/notes/CorrMatrix/India_AOE.pdf")
levelplot(a11, main="Correlations India", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

a12 <- corrplot(g.mat2, method = "square", col = col4(200), order = "AOE")
pdf("output/notes/CorrMatrix/Canada_AOE.pdf")
levelplot(a12, main="Correlations Canada", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

a13 <- corrplot(g.mat3, method = "square", col = col4(200), order = "AOE")
pdf("output/notes/CorrMatrix/Australia_AOE.pdf")
levelplot(a13, main="Correlations Australia", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

## the first principal component order
a21 <- corrplot(g.mat1, method = "square", col = col4(200), order = "FPC")
pdf("output/notes/CorrMatrix/India_FPC.pdf")
levelplot(a21, main="Correlations India", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

a22 <- corrplot(g.mat2, method = "square", col = col4(200), order = "FPC")
pdf("output/notes/CorrMatrix/Canada_FPC.pdf")
levelplot(a22, main="Correlations Canada", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

a23 <- corrplot(g.mat3, method = "square", col = col4(200), order = "FPC")
pdf("output/notes/CorrMatrix/Australia_FPC.pdf")
levelplot(a23, main="Correlations Australia", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

a31 <- corrplot(g.mat1, method = "square", col = col4(200), order = "hclust",
         hclust.method = "ward")
pdf("output/notes/CorrMatrix/India_hclust_ward.pdf")
levelplot(a31, main="Correlations India", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

a32 <- corrplot(g.mat2, method = "square", col = col4(200), order = "hclust",
         hclust.method = "ward")
pdf("output/notes/CorrMatrix/Canada_hclust_ward.pdf")
levelplot(a32, main="Correlations Canada", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

a33 <- corrplot(g.mat3, method = "square", col = col4(200), order = "hclust",
         hclust.method = "ward")
pdf("output/notes/CorrMatrix/Australia_hclust_ward.pdf")
levelplot(a33, main="Correlations Australia", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

a41 <- corrplot(abs(g.mat1), method = "square", col = col4(200),
                order = "hclust", hclust.method = "centroid")
pdf("output/notes/CorrMatrix/India_hclust_centroid.pdf")
levelplot(a41, main="Correlations India", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

a42 <- corrplot(abs(g.mat2), method = "square", col = col4(200),
                order = "hclust", hclust.method = "centroid")
pdf("output/notes/CorrMatrix/Canada_hclust_centroid.pdf")
levelplot(a42, main="Correlations Canada", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()

a43 <- corrplot(abs(g.mat3), method = "square", col = col4(200),
                order = "hclust", hclust.method = "centroid")
pdf("output/notes/CorrMatrix/Australia_hclust_centroid.pdf")
levelplot(a43, main="Correlations Australia", xlab="", ylab="",
          col.regions=col4(200), cuts = 100)
dev.off()



