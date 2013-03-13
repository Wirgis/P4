source("01readNUM.R")

####    Original file POP17.xlsx
####    Population, 17 group.
pop17 <- read.csv("../../data/csv_format/POP17.csv")
pop17 <- pop17[-1, ]

## Unnecessary columns;
pop17 <- pop17[which(!colnames(pop17) %in%
                 c("Industry", "Edition", "Category", "ParentID", "ProductID",
                   "Lowest.Level", "Modelled", "Data.Type", "DataTypeID",
                   "Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion"))]

## colnames;
colnames(pop17) <- gsub("^X", "", colnames(pop17))

## subcategory;
pop17$Subcategory <- as.character(pop17$Subcategory)
pop17$Subcategory <- gsub("Pop17ulation Aged |: January 1st", "" ,
                        pop17$Subcategory)

pop17 <- Divide(pop17, pop, by = c("Region", "Country", "CountryID",
                            "Subcategory"))

rm(list = c("gdp", "gdp.constant.fix", "gdp.current.fix", "gdp.current.year",
   "gdp.ppp", "gdp.total", "hh", "pop"))