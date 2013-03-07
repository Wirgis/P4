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
## Asia Pacific + Australasia + Middle East and Africa + Latin America
data <- gva.cur.f.hh[gva.cur.f.hh$Region %in%
                     c("Asia Pacific", "Australasia", "Middle East and Africa",
                       "Latin America"), ]
data <- Sample(data, years = seq(1979, 2012, by = 3))
Y.other.3.hh <- DoAll(X = data[, -c(1:3)],
                      cn.year = data[, c("Country", "Year")], k = 2)

pdf("../results/region/sample/GVA7/other_3_hh.pdf", height = 8, width = 17)
 myplot2(data = Y.other.3.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.other.3.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.other.3.hh$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.other.3.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.other.3.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.other.3.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

pdf("../results/region/sample/GVA7/other_3_hh_pca.pdf", height = 8, width = 17)
 myplot2(data = Y.other.3.hh$Y.pca, point = "2012", cn, title = "PCA")
dev.off()

## Europe + North America
data <- gva.cur.f.hh[gva.cur.f.hh$Region %in%
                     c("Eastern Europe", "Western Europe", "North America"), ]
data <- Sample(data, years = seq(1979, 2012, by = 3))
Y.america.euro.3.hh <- DoAll(X = data[, -c(1:3)],
                             cn.year = data[, c("Country", "Year")], k = 2)

pdf("../results/region/sample/GVA7/america_euro_3_hh.pdf", height = 8,
    width = 17)
 myplot2(data = Y.america.euro.3.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.america.euro.3.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.america.euro.3.hh$Y.isoMDS, point = "2012", cn,
         title = "isoMDS")
 myplot2(data = Y.america.euro.3.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.america.euro.3.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.america.euro.3.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

pdf("../results/region/sample/GVA7/america_euro_3_hh_mds.pdf", height = 8,
    width = 17)
 myplot2(data = Y.america.euro.3.hh$Y.mds, point = "2012", cn, title = "MDS")
dev.off()
