library(ggplot2)
library(grid)
library(stats)
library(MASS)
library(kernlab)
library(tsne)
library(spe)
library(lle)

## short names of countries
cn <- read.csv("../../data/csv_format/countries.csv")

## source the data
source("02readGVA7.R")
ls()
# the explanation of subscripts:
# 1) con - constant prices(2012);
# 2) cur - current prices;
# 3) y - year-on-year exchange rate;
# 4) f - fixed exchange rate(2012);
# 5) pop - population;
# 6) gdp - gross domestic product;
# 7) hh - households;

####################### PER 1000 OF HOUSEHOLDS
#### CURRENT PRICES, FIXED EXCHANGE RATE, PER 1000 OF HOUSEHOLDS

## how many NA's in each column
apply(gva.cur.f.hh[, -c(1:4)], 2, function(x) length(which(is.na(x))))

## period
from <- 2000
data <- gva.cur.f.hh[!colnames(gva.cur.f.hh) %in%
                     as.character(c(1977:(from - 1)))]
data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)

## some countries have no observations of the first category
temp <- ddply(data, .(Country), function(x)
              apply(x[, -c(1:3)], 2, function(y) length(which(is.na(y)))))
rows <- which(temp[, -1] > 0, arr.ind = TRUE)[, "row"]
cn.no <- as.character(temp[rows, "Country"])
data <- data[!data$Country %in% cn.no, ]

data$Country <- factor(as.character(data$Country))
data$Region <- factor(as.character(data$Region))

Y.hh <- DoAll(X = data[, -c(1:3)], cn.year = data[, c("Country", "Year")],
              k = 2)

pdf("../results/period/GVA7/short_2000_hh.pdf", height = 8, width = 17)
 myplot2(data = Y.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.hh$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

################### SHARE OF total GDP

## how many NA's in each column
apply(gva.gdp[, -c(1:4)], 2, function(x) length(which(is.na(x))))

## period
from <- 2000
data <- gva.gdp[!colnames(gva.gdp) %in%
                     as.character(c(1977:(from - 1)))]
data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)

## some countries have no observations of the first category
temp <- ddply(data, .(Country), function(x)
              apply(x[, -c(1:3)], 2, function(y) length(which(is.na(y)))))
rows <- which(temp[, -1] > 0, arr.ind = TRUE)[, "row"]
cn.no <- as.character(temp[rows, "Country"])
data <- data[!data$Country %in% cn.no, ]

data$Country <- factor(as.character(data$Country))
data$Region <- factor(as.character(data$Region))

Y.gdp <- DoAll(X = data[, -c(1:3)], cn.year = data[, c("Country", "Year")],
               k = 2)

pdf("../results/period/GVA7/short_2000_gdp.pdf", height = 8, width = 17)
 myplot2(data = Y.gdp$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.gdp$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.gdp$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.gdp$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.gdp$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.gdp$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

################### PER 1000 of POPULATION

#### CURRENT PRICES, FIXED EXCHANGE RATE, PER 1000 OF POPULATION
## how many NA's in each column
apply(gva.cur.f.pop[, -c(1:4)], 2, function(x) length(which(is.na(x))))

## period
from <- 2000
data <- gva.cur.f.pop[!colnames(gva.cur.f.pop) %in%
                      as.character(c(1977:(from - 1)))]
data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)

## some countries have no observations of the first category
temp <- ddply(data, .(Country), function(x)
              apply(x[, -c(1:3)], 2, function(y) length(which(is.na(y)))))
rows <- which(temp[, -1] > 0, arr.ind = TRUE)[, "row"]
cn.no <- as.character(temp[rows, "Country"])
data <- data[!data$Country %in% cn.no, ]

data$Country <- factor(as.character(data$Country))
data$Region <- factor(as.character(data$Region))

Y.pop <- DoAll(X = data[, -c(1:3)],
               cn.year = data[, c("Country", "Year")], k = 2)

pdf("../results/period/GVA7/short_2000_pop.pdf", height = 8, width = 17)
 myplot2(data = Y.pop$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.pop$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.pop$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.pop$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.pop$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.pop$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

## Countries
Hong Kong, China     India                Malaysia
Singapore            Taiwan               Thailand
Vietnam              Czech Republic       Poland
Slovakia             Ukraine              Argentina
Colombia             Mexico               Peru
Venezuela            Morocco              South Africa
United Arab Emirates USA                  Austria
Belgium              Denmark              Finland
France               Germany              Greece
Ireland              Italy                Netherlands
Norway               Portugal             Spain
Sweden               Switzerland          Turkey
United Kingdom


