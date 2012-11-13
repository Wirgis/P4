library(foreach)
library(doParallel)
library(RSQLite)

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
data <- as.matrix(data[data$Hierarchy.Level == 4, ])

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

######### Check if appropriate table exists. If not, create it.
# unique table for unique data.frame: Corr.data.name.
id.table <- length(grep(data.name, table.names))
if(id.table == 0){
    sql1 <- "CREATE Table"
    sql2 <- paste("Corr" , data.name, sep = "_")
    sql3 <- "(Country1 SMALLINT, Category1 SMALLINT, Country2 SMALLINT,
Category2 SMALLINT, Corr NUMERIC(1,2))"
    sql <- paste(sql1, sql2, sql3)

    dbGetQuery(con, sql)
}

######### Form indexes
indexes <- combn(1:dim(data)[1], 2)

######### Form INSERT query
sql1 <- "INSERT INTO"
sql2 <- paste("Corr" , data.name, sep = "_")
sql3 <- "('Country1', 'Category1', 'Country2', 'Category2', 'Corr') VALUES "
sql.base <- paste(sql1, sql2, sql3)

######### Choose the range. Only correlations that satisfy the condition are
# inserted: range1 <= abs(cor) & abs(cor) <= range2
range1 <- 0.25
range2 <- 1

######### The cycle for inserting correlations. This is doing sequentially.
foreach(i = 1:dim(indexes)[2]) %do%{
    cor <- cor(data[indexes[, i][1], -c(1, 2)],
               data[indexes[, i][2], -c(1, 2)], method = "kendall",
               use = "complete.obs")
    if(range1 <= abs(cor) & abs(cor) <= range2) {
        sql <- paste(sql.base, "(",paste(data[indexes[, i][1], "Country"],
                                         data[indexes[, i][1],
                                                  "CategorySub"],
                                         data[indexes[, i][2], "Country"],
                                         data[indexes[, i][2],
                                                  "CategorySub"],
                                         Corr = round(cor, 2), sep = ","), ")",
                     sep = "")
        dbGetQuery(con, sql)
    }
}

dbDisconnect(con)

