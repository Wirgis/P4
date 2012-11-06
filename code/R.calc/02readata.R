library(reshape)
library(foreach)
## PIlocal.xlsx: local currency, local currency per capita,
## local currency per household;
pathPIlocal <- "data/rawdata/PIlocal.csv"
data.raw <- read.csv(pathPIlocal)

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

## Unit: doesn't matter as we deal with growth rates
data.raw$Unit <- NULL

## Current Constant: one option - Historic Constant Prices,
## Forecast Constant Prices
data.raw$Current.Constant <- NULL

## Currency Conversion: one option - Local Currency
data.raw$Currency.Conversion <- NULL

## doesn't matter as we deal with growth rates
data.raw$PerCap.Unit.PerCap <- NULL

## doesn't matter as we deal with growth rates
data.raw$PerCap.Unit.PerHH <- NULL

## three options by measurement type: local, local Per Capita,
## local Per Household

## three data sets:
## 1: local;
## 2: local per capita;
## 3: local per household;

columns <- c("Country", "CategorySub")

data.raw.loc <- data.raw[, colnames(data.raw) %in% c(columns, 1997:2011)]

data.raw.PC <- data.raw[, colnames(data.raw) %in%
                        c(columns, paste(1997:2011, "PerCap", sep = "."))]
colnames(data.raw.PC) [3:length(colnames(data.raw.PC))]<- 1997:2011

data.raw.PHH <- data.raw[, colnames(data.raw) %in%
                        c(columns, paste(1997:2011, "PerHH", sep = "."))]
colnames(data.raw.PHH) [3:length(colnames(data.raw.PHH))]<- 1997:2011

# remove rows which have all NA
data.raw.loc <- ChangeNA(data.raw.loc)
data.raw.PC <- ChangeNA(data.raw.PC)
data.raw.PHH <- ChangeNA(data.raw.PHH)

## some strange cases
check.na <- unique(which(is.na(data.raw.loc), arr.ind = T)[, 1])
## data.raw.loc[check.na, ]
data.raw.loc <- data.raw.loc[-check.na, ]

check.na <- unique(which(is.na(data.raw.PC), arr.ind = T)[, 1])
## data.raw.PC[check.na, ]
data.raw.PC <- data.raw.PC[-check.na, ]

check.na <- unique(which(is.na(data.raw.PHH), arr.ind = T)[, 1])
## data.raw.PHH[check.na, ]
data.raw.PHH <- data.raw.PHH[-check.na, ]

## remove all 0
data.raw.loc <- ChangeZero(data.raw.loc)
data.raw.PC <- ChangeZero(data.raw.PC)
data.raw.PHH <- ChangeZero(data.raw.PHH)

# make growth rates
data.loc <- MakeGrowthRates(data.raw.loc, as.character(1997:2011))
data.PC <- MakeGrowthRates(data.raw.PC, as.character(1997:2011))
data.PHH <- MakeGrowthRates(data.raw.PHH, as.character(1997:2011))

## replace Inf with NA
data.loc <- ChangeInf(data.loc)
data.PC <- ChangeInf(data.PC)
data.PHH <- ChangeInf(data.PHH)
