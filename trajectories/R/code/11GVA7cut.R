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

#### CURRENT PRICES, FIXED EXCHANGE RATE, PER 1000 OF HOUSEHOLDS
## how many NA's in each column
apply(gva.cur.f.hh[, -c(1:4)], 2, function(x) length(which(is.na(x))))

## period
data <- Cut(data = gva.cur.f.hh[, -3], from = 2000)
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

pdf("../results/period/GVA7/short_2000_hh_mds.pdf", height = 8, width = 17)
 myplot2(data = Y.hh$Y.mds, point = "2012", cn, title = "MDS")
dev.off()

pdf("../results/period/GVA7/short_2000_hh_SPE.pdf", height = 8, width = 17)
 myplot2(data = Y.hh$Y.spe, point = "2012", cn, title = "SPE")
dev.off()

################### SHARE OF total GDP
## how many NA's in each column
apply(gva.gdp[, -c(1:4)], 2, function(x) length(which(is.na(x))))

## period
data <- Cut(data = gva.gdp[, -3], from = 2000)
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

#### CURRENT PRICES, FIXED EXCHANGE RATE, PER 1000 OF POPULATION
## how many NA's in each column
apply(gva.cur.f.pop[, -c(1:4)], 2, function(x) length(which(is.na(x))))

## period
data <- Cut(data = gva.cur.f.pop[, -3], from = 2000)
Y.pop <- DoAll(X = data[, -c(1:3)], cn.year = data[, c("Country", "Year")],
               k = 2)

pdf("../results/period/GVA7/short_2000_pop.pdf", height = 8, width = 17)
 myplot2(data = Y.pop$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.pop$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.pop$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.pop$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.pop$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.pop$Y.lle, point = "2012", cn, title = "LLE")
dev.off()


