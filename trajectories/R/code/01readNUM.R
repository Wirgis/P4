library(reshape)

source("10code.R")
#### 1) Original file: NUM.xlsx;
####    Categories: Total GDP, GDP measured at PPP, number of HH, population;
gdp <- read.csv("../../data/csv_format/NUM.csv")
gdp <- gdp[-1, ]

## Unnecessary columns;
gdp <- gdp[which(!colnames(gdp) %in%
                 c("Industry", "Edition", "Category", "ParentID",
                   "ProductID", "Lowest.Level", "Modelled", "Data.Type",
                   "DataTypeID"))]

## Current Constant;
gdp$Current.Constant <- as.character(gdp$Current.Constant)
gdp$Current.Constant <- gsub("Historic Current Prices, Forecast Constant 2012 Prices", "current", gdp$Current.Constant)
gdp$Current.Constant <- gsub("Historic Constant 2012 Prices, Forecast Constant 2012 Prices", "constant", gdp$Current.Constant)

## Currency Conversion;
gdp$Currency.Conversion <- as.character(gdp$Currency.Conversion)
gdp$Currency.Conversion <- gsub("Local Currency", "local",
                                gdp$Currency.Conversion)
gdp$Currency.Conversion <- gsub("Historic Year-on-Year Exchange Rates, Forecast Year-on-Year Exchange Rates", "year.on.year",
                                gdp$Currency.Conversion)
gdp$Currency.Conversion <- gsub("Historic Fixed 2012 Exchange Rates, Forecast Fixed 2012 Exchange Rates", "fixed",
                                gdp$Currency.Conversion)

## colnames;
colnames(gdp) <- gsub("^X", "", colnames(gdp))

## gdp measured at PPP;
gdp$Subcategory <- as.character(gdp$Subcategory)
gdp.ppp <- gdp[gdp$Subcategory == "GDP Measured at Purchasing Power Parity", ]
gdp.ppp <- gdp.ppp[which(!colnames(gdp.ppp) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion", "Subcategory"))]

## gdp US$ millions;
gdp.total <- gdp[gdp$Subcategory == "Total GDP", ]
gdp.current.year <- gdp.total[(gdp.total$Current.Constant == "current" &
                               gdp.total$Currency.Conversion == "year.on.year"),]
gdp.current.year <- gdp.current.year[which(!colnames(gdp.current.year) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion", "Subcategory"))]

gdp.current.fix <- gdp.total[(gdp.total$Current.Constant == "current" &
                               gdp.total$Currency.Conversion == "fixed"),]
gdp.current.fix <- gdp.current.fix[which(!colnames(gdp.current.fix) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion", "Subcategory"))]

gdp.constant.fix <- gdp.total[(gdp.total$Current.Constant == "constant" &
                               gdp.total$Currency.Conversion == "fixed"),]
gdp.constant.fix <- gdp.constant.fix[which(!colnames(gdp.constant.fix) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion", "Subcategory"))]

## gdp US$ millions per 1000 of households;
hh <- gdp[gdp$Subcategory %in% "Number of Households", ]
hh <- hh[which(!colnames(hh) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion", "Subcategory"))]

pop <- gdp[gdp$Subcategory %in% "Population: National Estimates at January 1st", ]
pop <- pop[which(!colnames(pop) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion", "Subcategory"))]
