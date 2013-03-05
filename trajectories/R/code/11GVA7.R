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

#### CONSTANT PRICES, FIXED EXCHANGE RATE, PER 1000 OF HOUSEHOLDS
data <- gva.con.f.hh
data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)
data.omit <- na.omit(data)
data.omit$Country <- factor(as.character(data.omit$Country))
data.omit$Region <- factor(as.character(data.omit$Region))

Y.con.f.hh <- DoAll(X = data.omit[, -c(1:3)],
                    cn.year = data.omit[, c("Country", "Year")], k = 2)

pdf("../results/all/GVA7/con_f_hh.pdf", height = 8, width = 17)
 myplot2(data = Y.con.f.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.con.f.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.con.f.hh$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.con.f.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.con.f.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.con.f.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()


#### CURRENT PRICES, FIXED EXCHANGE RATE, PER 1000 OF HOUSEHOLDS
data <- gva.cur.f.hh
data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)
data.omit <- na.omit(data)
data.omit$Country <- factor(as.character(data.omit$Country))
data.omit$Region <- factor(as.character(data.omit$Region))

Y.cur.f.hh <- DoAll(X = data.omit[, -c(1:3)],
                    cn.year = data.omit[, c("Country", "Year")], k = 2)

pdf("../results/all/GVA7/cur_f_hh.pdf", height = 8, width = 17)
 myplot2(data = Y.cur.f.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.cur.f.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.cur.f.hh$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.cur.f.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.cur.f.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.cur.f.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

#### CURRENT PRICES, YEAR-ON-YEAR EXCHANGE RATE, PER 1000 OF HOUSEHOLDS
data <- gva.cur.y.hh
data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)
data.omit <- na.omit(data)
data.omit$Country <- factor(as.character(data.omit$Country))
data.omit$Region <- factor(as.character(data.omit$Region))

Y.cur.y.hh <- DoAll(X = data.omit[, -c(1:3)],
                    cn.year = data.omit[, c("Country", "Year")], k = 2)

pdf("../results/all/GVA7/cur_y_hh.pdf", height = 8, width = 17)
 myplot2(data = Y.cur.y.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.cur.y.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.cur.y.hh$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.cur.y.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.cur.y.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.cur.y.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

################### SHARE OF total GDP
data <- gva.gdp
data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)
data.omit <- na.omit(data)
data.omit$Country <- factor(as.character(data.omit$Country))
data.omit$Region <- factor(as.character(data.omit$Region))

Y.gdp <- DoAll(X = data.omit[, -c(1:3)],
                     cn.year = data.omit[, c("Country", "Year")], k = 2)

pdf("../results/all/GVA7/share_gdp.pdf", height = 8, width = 17)
 myplot2(data = Y.gdp$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.gdp$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.gdp$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.gdp$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.gdp$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.gdp$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

################### PER 1000 of POPULATION

#### CONSTANT PRICES, FIXED EXCHANGE RATE, PER 1000 OF POPULATION
data <- gva.con.f.pop
data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)
data.omit <- na.omit(data)
data.omit$Country <- factor(as.character(data.omit$Country))
data.omit$Region <- factor(as.character(data.omit$Region))

Y.con.f.pop <- DoAll(X = data.omit[, -c(1:3)],
                    cn.year = data.omit[, c("Country", "Year")], k = 2)

pdf("../results/all/GVA7/con_f_pop.pdf", height = 8, width = 17)
 myplot2(data = Y.con.f.pop$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.con.f.pop$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.con.f.pop$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.con.f.pop$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.con.f.pop$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.con.f.pop$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

#### CURRENT PRICES, FIXED EXCHANGE RATE, PER 1000 OF POPULATION
data <- gva.cur.f.pop
data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)
data.omit <- na.omit(data)
data.omit$Country <- factor(as.character(data.omit$Country))
data.omit$Region <- factor(as.character(data.omit$Region))

Y.cur.f.pop <- DoAll(X = data.omit[, -c(1:3)],
                    cn.year = data.omit[, c("Country", "Year")], k = 2)

pdf("../results/all/GVA7/cur_f_pop.pdf", height = 8, width = 17)
 myplot2(data = Y.cur.f.pop$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.cur.f.pop$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.cur.f.pop$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.cur.f.pop$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.cur.f.pop$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.cur.f.pop$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

#### CURRENT PRICES, YEAR-ON-YEAR EXCHANGE RATE, PER 1000 OF POPULATION
data <- gva.cur.y.pop
data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)
data.omit <- na.omit(data)
data.omit$Country <- factor(as.character(data.omit$Country))
data.omit$Region <- factor(as.character(data.omit$Region))

Y.cur.y.pop <- DoAll(X = data.omit[, -c(1:3)],
                    cn.year = data.omit[, c("Country", "Year")], k = 2)

pdf("../results/all/GVA7/cur_y_pop.pdf", height = 8, width = 17)
 myplot2(data = Y.cur.y.pop$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.cur.y.pop$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.cur.y.pop$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.cur.y.pop$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.cur.y.pop$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.cur.y.pop$Y.lle, point = "2012", cn, title = "LLE")
dev.off()
