source("01readNUM.R")

####    Original file CE12.xlsx
####    Comsumer expenditure divided into 12 categories.
ce <- read.csv("../../data/csv_format/CE12.csv")
ce <- ce[-1, ]

## Unnecessary columns;
ce <- ce[which(!colnames(ce) %in%
                 c("Industry", "Edition", "Category", "ParentID", "ProductID",
                   "Lowest.Level", "Modelled", "Data.Type", "DataTypeID"))]

## Current Constant;
ce$Current.Constant <- as.character(ce$Current.Constant)
ce$Current.Constant <- gsub("Historic Current Prices, Forecast Constant 2012 Prices", "current", ce$Current.Constant)
ce$Current.Constant <- gsub("Historic Constant 2012 Prices, Forecast Constant 2012 Prices", "constant", ce$Current.Constant)

## Currency Conversion;
ce$Currency.Conversion <- as.character(ce$Currency.Conversion)
ce$Currency.Conversion <- gsub("Local Currency", "local",
                                ce$Currency.Conversion)
ce$Currency.Conversion <- gsub("Historic Year-on-Year Exchange Rates, Forecast Year-on-Year Exchange Rates", "year.on.year",
                                ce$Currency.Conversion)
ce$Currency.Conversion <- gsub("Historic Fixed 2012 Exchange Rates, Forecast Fixed 2012 Exchange Rates", "fixed",
                                ce$Currency.Conversion)

## colnames;
colnames(ce) <- gsub("^X", "", colnames(ce))

## subcategory;
ce$Subcategory <- as.character(ce$Subcategory)
ce$Subcategory <- gsub("Consumer Expenditure on ", "", ce$Subcategory)

## ce measured at current prices, year-on-year exchange rate;
ce.cur.year <- ce[(ce$Current.Constant == "current" & ce$Currency.Conversion == "year.on.year"), ]
ce.cur.year <- ce.cur.year[which(!colnames(ce.cur.year) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion"))]
ce.cur.y.hh <- Divide(ce.cur.year, hh, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))
ce.cur.y.pop <- Divide(ce.cur.year, pop, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))
ce.gdp <- Divide(ce.cur.year, gdp.current.year,
                 by = c("Region", "Country", "CountryID", "Subcategory"))


## ce measured at current prices, fixed exchange rate;
ce.cur.fix <- ce[(ce$Current.Constant == "current" & ce$Currency.Conversion == "fixed"), ]
ce.cur.fix <- ce.cur.fix[which(!colnames(ce.cur.fix) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion"))]
ce.cur.f.hh <- Divide(ce.cur.fix, hh, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))
ce.cur.f.pop <- Divide(ce.cur.fix, pop, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))

## ce measured at constant prices, fixed exchange rate;
ce.con.fix <- ce[(ce$Current.Constant == "constant" & ce$Currency.Conversion == "fixed"), ]
ce.con.fix <- ce.con.fix[which(!colnames(ce.con.fix) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion"))]
ce.con.f.hh <- Divide(ce.con.fix, hh, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))
ce.con.f.pop <- Divide(ce.con.fix, pop, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))

rm(list = c("gdp", "gdp.constant.fix", "gdp.current.fix", "gdp.current.year",
   "gdp.ppp", "gdp.total", "ce", "hh", "pop" ,"ce.con.fix", "ce.cur.fix",
   "ce.cur.year"))

