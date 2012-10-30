library(RSQLite)

## creat database
drv <- SQLite()
con <- dbConnect(drv, dbname = "output/R/P4.sqlite")

## create table1: Countries
cn <- read.csv("data/rawdata/EMI_private/Country.id.csv",
               stringsAsFactors = FALSE)
colnames(cn)[2] <- "id_cn"
## max(nchar(cn[, "Country"]))

dbGetQuery(con, "CREATE Table Countries (Country CHARACTER VARYING(15),
id_cn SMALLINT)")

sql <- "INSERT INTO Countries VALUES ($Country, $id_cn)"
dbBeginTransaction(con)
dbGetPreparedQuery(con, sql, bind.data = cn)
dbCommit(con)

## create table2: Categories
cat <- read.csv("data/rawdata/EMI_private/Category.id.csv",
                stringsAsFactors = FALSE)
cat[, "Subcategory"] <- gsub("^([ \t\n\r\f\v]+)", "", cat[, "Subcategory"])
##max(nchar(cat[, "Subcategory"]))
##max(nchar(cat[, "Category"]))

dbGetQuery(con, "CREATE Table Categories (id_cat SMALLINT,
Category CHARACTER VARYING(35), Subcategory CHARACTER VARYING(90))")


colnames(cat)[3] <- "id_cat"
sql <- "INSERT INTO Categories VALUES ($id_cat, $Category, $Subcategory)"
dbBeginTransaction(con)
dbGetPreparedQuery(con, sql, bind.data = cat)
dbCommit(con)

##
## dbListTables(con)
## dbListFields(con, "Categories")
## dbListFields(con, "Countries")

## dbGetQuery(con, "SELECT * FROM Categories WHERE id_cat = 1")
## dbGetQuery(con, "SELECT Country FROM Countries")
## dbGetQuery(con, "DROP TABLE Countries")

dbDisconnect(con)


