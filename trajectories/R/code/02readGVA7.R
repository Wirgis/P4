source("01readNUM.R")

#### 2) Original file GVA7.xlsx
####    Gross value added from 7 categories.
gva <- read.csv("../../data/csv_format/GVA7.csv")
gva <- gva[-1, ]

## Unnecessary columns;
gva <- gva[which(!colnames(gva) %in%
                 c("Industry", "Edition", "Category", "ParentID", "ProductID",
                   "Lowest.Level", "Modelled", "Data.Type", "DataTypeID"))]

## Current Constant;
gva$Current.Constant <- as.character(gva$Current.Constant)
gva$Current.Constant <- gsub("Historic Current Prices, Forecast Constant 2012 Prices", "current", gva$Current.Constant)
gva$Current.Constant <- gsub("Historic Constant 2012 Prices, Forecast Constant 2012 Prices", "constant", gva$Current.Constant)

## Currency Conversion;
gva$Currency.Conversion <- as.character(gva$Currency.Conversion)
gva$Currency.Conversion <- gsub("Local Currency", "local",
                                gva$Currency.Conversion)
gva$Currency.Conversion <- gsub("Historic Year-on-Year Exchange Rates, Forecast Year-on-Year Exchange Rates", "year.on.year",
                                gva$Currency.Conversion)
gva$Currency.Conversion <- gsub("Historic Fixed 2012 Exchange Rates, Forecast Fixed 2012 Exchange Rates", "fixed",
                                gva$Currency.Conversion)

## colnames;
colnames(gva) <- gsub("^X", "", colnames(gva))

## subcategory;
gva$Subcategory <- as.character(gva$Subcategory)
gva$Subcategory <- gsub("GVA from Agriculture, Hunting, Forestry and Fishing",
                        "Agri.Hunt.Forest.Fish", gva$Subcategory)
gva$Subcategory <- gsub("GVA from Manufacturing, Mining, Gas and Water Supply",
                        "Manu.Min.Gas.Water", gva$Subcategory)
gva$Subcategory <- gsub("GVA from Construction",
                        "Constructions", gva$Subcategory)
gva$Subcategory <- gsub("GVA from Transport, Communications, Trade, Hotels and Restaurants",
                        "Tran.Com.Tr.Hot.Res", gva$Subcategory)
gva$Subcategory <- gsub("GVA from Financial Intermediation, Real Estate, Renting and Business Activities",
                        "Finance.RealEst.Rent.Bus", gva$Subcategory)
gva$Subcategory <- gsub("GVA from Education, Health, Social Services, Public and Undefined Sectors",
                        "Edu.Health.Soc.Public", gva$Subcategory)
gva$Subcategory <- gsub("GVA from Activities of Households",
                        "Activities.of.hh", gva$Subcategory)

## gva measured at current prices, year-on-year exchange rate;
gva.cur.year <- gva[(gva$Current.Constant == "current" & gva$Currency.Conversion == "year.on.year"), ]
gva.cur.year <- gva.cur.year[which(!colnames(gva.cur.year) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion"))]
gva.cur.y.hh <- Divide(gva.cur.year, hh, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))
gva.cur.y.pop <- Divide(gva.cur.year, pop, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))
gva.gdp <- Divide(gva.cur.year, gdp.current.year,
                        by = c("Region", "Country", "CountryID", "Subcategory"))


## gva measured at current prices, fixed exchange rate;
gva.cur.fix <- gva[(gva$Current.Constant == "current" & gva$Currency.Conversion == "fixed"), ]
gva.cur.fix <- gva.cur.fix[which(!colnames(gva.cur.fix) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion"))]
gva.cur.f.hh <- Divide(gva.cur.fix, hh, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))
gva.cur.f.pop <- Divide(gva.cur.fix, pop, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))

## gva measured at constant prices, fixed exchange rate;
gva.con.fix <- gva[(gva$Current.Constant == "constant" & gva$Currency.Conversion == "fixed"), ]
gva.con.fix <- gva.con.fix[which(!colnames(gva.con.fix) %in%
                 c("Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion"))]
gva.con.f.hh <- Divide(gva.con.fix, hh, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))
gva.con.f.pop <- Divide(gva.con.fix, pop, by = c("Region", "Country",
                                         "CountryID", "Subcategory"))

rm(list = c("gdp", "gdp.constant.fix", "gdp.current.fix", "gdp.current.year",
   "gdp.ppp", "gdp.total", "gva", "hh", "pop" ,"gva.con.fix", "gva.cur.fix",
   "gva.cur.year"))

