library(XML)
library(foreach)
library(RSQLite)
library(reshape)
source("code/R.calc/10code.R")

drv <- SQLite()
con <- dbConnect(drv, dbname = "output/R/P4.sqlite")
sql <- "SELECT * FROM Correlations limit 10000"

######### Makes graph and saves it as .gexf file
###
### con - connection to database;
### sql - character indicating the query query for con
### output - where to print the GEXF file
system.time({Mygexf(con, sql, output = "bandom.gexf")})









