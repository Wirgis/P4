rm(list=ls())
library(reshape)
library(foreach)

source("Code/R.calc/10code.R")
## PIUSD.xlsx: US dollars, US dollars per capita, US dollars per household;
## currency conversion: fixed 2011 exchange rate, year-on-year exchange rate;
pathPIUSD <- "data/rawdata/PIUSD.csv"
data.raw <- read.csv(pathPIUSD)

## colnames
id.year <- grep("X", colnames(data.raw))
colnames(data.raw)[id.year] <- gsub("X", "", colnames(data.raw)[id.year])
colnames(data.raw) <- gsub("PC.", "", colnames(data.raw))
colnames(data.raw) <- gsub("\\.\\.", ".", colnames(data.raw))
colnames(data.raw) <- gsub("\\.$", "", colnames(data.raw))
colnames(data.raw) <- gsub("Per.Capita", "PerCap", colnames(data.raw))
colnames(data.raw) <- gsub("Per.Household", "PerHH", colnames(data.raw))

## Data Type: one option
data.raw$Data.Type <- NULL

## Unit: one option - US million
data.raw$Unit <- NULL

## Current Constant: one option - Historic Current Prices,
## Forecast Current Prices
data.raw$Current.Constant <- NULL

## Currency Conversion
data.raw$Currency.Conversion <- as.character(data.raw$Currency.Conversion)
data.raw$Currency.Conversion <- gsub(" ", ".", data.raw$Currency.Conversion)
data.raw$Currency.Conversion <- gsub(unique(data.raw$Currency.Conversion)[1],
                                     "year.on.year.exch",
                                     data.raw$Currency.Conversion)
data.raw$Currency.Conversion <- gsub(unique(data.raw$Currency.Conversion)[2],
                                     "fixed.2011",
                                     data.raw$Currency.Conversion)

## Per Capita Units: one option - US dollars Per Capita
data.raw$PerCap.Unit.PerCap <- NULL

## Per Household Units: one option - US dollars Per Household
data.raw$PerCap.Unit.PerHH <- NULL


## three options by measurement type: US millions, US Per Capita,
## US Per Household

## two options by type of currency conversion: year on yeat exchange rate and
##fixed 2011 excahnge rate

## six data sets:
## 1: US million, year on year exchange rate;
## 2: US million, fixed 2011 exchange rate;
## 3: US per capita, year on year exchange rate;
## 4: US per capita, fixed 2011 exchange rate;
## 5: US per household, year on year exchange rate;
## 6: US per household, fixed 2011 exchange rate;

columns <- c("Country", "CategorySub", "Hierarchy.Level", "Currency.Conversion")


data.raw.million <- data.raw[, colnames(data.raw) %in% c(columns, 1997:2011)]
data.raw.flow <- data.raw.million[data.raw.million$Currency.Conversion ==
                                  "year.on.year.exch", ]
data.raw.flow$Currency.Conversion <- NULL
data.raw.fix <- data.raw.million[data.raw.million$Currency.Conversion ==
                                 "fixed.2011", ]
data.raw.fix$Currency.Conversion <- NULL


data.raw.percap <- data.raw[, colnames(data.raw) %in%
                            c(columns, paste(1997:2011, "PerCap", sep = "."))]
data.raw.PC.flow <- data.raw.percap[data.raw.percap$Currency.Conversion ==
                                    "year.on.year.exch", ]
data.raw.PC.flow$Currency.Conversion <- NULL
colnames(data.raw.PC.flow) [length(columns):length(colnames(data.raw.PC.flow))]<- 1997:2011
data.raw.PC.fix <- data.raw.percap[data.raw.percap$Currency.Conversion ==
                                    "fixed.2011", ]
data.raw.PC.fix$Currency.Conversion <- NULL
colnames(data.raw.PC.fix) [length(columns):length(colnames(data.raw.PC.fix))]<- 1997:2011


data.raw.perHH <- data.raw[, colnames(data.raw) %in%
                           c(columns, paste(1997:2011, "PerHH", sep = "."))]
data.raw.PHH.flow <- data.raw.perHH[data.raw.perHH$Currency.Conversion ==
                                    "year.on.year.exch", ]
data.raw.PHH.flow$Currency.Conversion <- NULL
colnames(data.raw.PHH.flow) [length(columns):length(colnames(data.raw.PHH.flow))]<- 1997:2011
data.raw.PHH.fix <- data.raw.perHH[data.raw.perHH$Currency.Conversion ==
                                   "fixed.2011", ]
data.raw.PHH.fix$Currency.Conversion <- NULL
colnames(data.raw.PHH.fix) [length(columns):length(colnames(data.raw.PHH.fix))]<- 1997:2011

# remove rows which have all NA
data.raw.flow <- RemoveNA(data.raw.flow)
data.raw.fix <- RemoveNA(data.raw.fix)
data.raw.PC.flow <- RemoveNA(data.raw.PC.flow)
data.raw.PC.fix <- RemoveNA(data.raw.PC.fix)
data.raw.PHH.flow <- RemoveNA(data.raw.PHH.flow)
data.raw.PHH.fix <- RemoveNA(data.raw.PHH.fix)


## some strange cases
check.na <- unique(which(is.na(data.raw.flow), arr.ind = T)[, 1])
## data.raw.flow[check.na, ]
data.raw.flow <- data.raw.flow[-check.na, ]

check.na <- unique(which(is.na(data.raw.fix), arr.ind = T)[, 1])
## data.raw.fix[check.na, ]
data.raw.fix <- data.raw.fix[-check.na, ]

check.na <- unique(which(is.na(data.raw.PC.flow), arr.ind = T)[, 1])
##data.raw.PC.flow[check.na, ]
data.raw.PC.flow <- data.raw.PC.flow[-check.na, ]

check.na <- unique(which(is.na(data.raw.PC.fix), arr.ind = T)[, 1])
##data.raw.PC.fix[check.na, ]
data.raw.PC.fix <- data.raw.PC.fix[-check.na, ]

check.na <- unique(which(is.na(data.raw.PHH.flow), arr.ind = T)[, 1])
##data.raw.PHH.flow[check.na, ]
data.raw.PHH.flow <- data.raw.PHH.flow[-check.na, ]

check.na <- unique(which(is.na(data.raw.PHH.fix), arr.ind = T)[, 1])
##data.raw.PHH.fix[check.na, ]
data.raw.PHH.fix <- data.raw.PHH.fix[-check.na, ]

## remove row with all zeros
data.raw.flow <- RemoveZero(data.raw.flow)
data.raw.fix <- RemoveZero(data.raw.fix)
data.raw.PC.flow <- RemoveZero(data.raw.PC.flow)
data.raw.PC.fix <- RemoveZero(data.raw.PC.fix)
data.raw.PHH.flow <- RemoveZero(data.raw.PHH.flow)
data.raw.PHH.fix <- RemoveZero(data.raw.PHH.fix)

# make growth rates
ptime <- proc.time()[3]
data.flow <- MakeGrowthRates(data.raw.flow, as.character(1997:2011))
data.fix <- MakeGrowthRates(data.raw.fix, as.character(1997:2011))
data.PC.flow <- MakeGrowthRates(data.raw.PC.flow, as.character(1997:2011))
data.PC.fix <- MakeGrowthRates(data.raw.PC.fix, as.character(1997:2011))
data.PHH.flow <- MakeGrowthRates(data.raw.PHH.flow, as.character(1997:2011))
data.PHH.fix <- MakeGrowthRates(data.raw.PHH.fix, as.character(1997:2011))
ptime1 <- proc.time()[3] - ptime
ptime1

## replace Inf with NA
data.flow <- ChangeInf(data.flow)
data.fix <- ChangeInf(data.fix)
data.PC.flow <- ChangeInf(data.PC.flow)
data.PC.fix <- ChangeInf(data.PC.fix)
data.PHH.flow <- ChangeInf(data.PHH.flow)
data.PHH.fix <- ChangeInf(data.PHH.fix)

# remove rows which have all NA
data.flow <- RemoveNA(data.flow)
data.fix <- RemoveNA(data.fix)
