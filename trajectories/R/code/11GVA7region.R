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
data <- GetDataOmit(data = gva.cur.f.hh[gva.cur.f.hh$Region %in%
                    c("Asia Pacific", "Australasia", "Middle East and Africa",
                      "Latin America"), -3])
Y.other.hh <- DoAll(X = data[, -c(1:3)],
                    cn.year = data[, c("Country", "Year")], k = 2)

pdf("../results/region/GVA7/other_hh.pdf", height = 8, width = 17)
 myplot2(data = Y.other.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.other.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.other.hh$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.other.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.other.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.other.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

pdf("../results/region/GVA7/other_hh_pca.pdf", height = 8, width = 17)
 myplot2(data = Y.other.hh$Y.pca, point = "2012", cn, title = "PCA")
dev.off()


data <- GetDataOmit(data = gva.cur.f.hh[gva.cur.f.hh$Region %in%
                    c("Eastern Europe", "Western Europe", "North America"), -3])

Y.america.euro.hh <- DoAll(X = data[, -c(1:3)],
                           cn.year = data[, c("Country", "Year")], k = 2)

pdf("../results/region/GVA7/america_euro_hh.pdf", height = 8, width = 17)
 myplot2(data = Y.america.euro.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.america.euro.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.america.euro.hh$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.america.euro.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.america.euro.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.america.euro.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

pdf("../results/region/GVA7/america_euro_hh_mds.pdf", height = 8, width = 17)
 myplot2(data = Y.america.euro.hh$Y.mds, point = "2012", cn, title = "MDS")
dev.off()

#### CURRENT PRICES, FIXED EXCHANGE RATE, PER 1000 OF POPULATION
data <- GetDataOmit(data = gva.cur.f.pop[gva.cur.f.pop$Region %in%
                    c("Asia Pacific", "Australasia", "Middle East and Africa",
                      "Latin America"), -3])

Y.other.hh <- DoAll(X = data[, -c(1:3)],
                    cn.year = data[, c("Country", "Year")], k = 2)

pdf("../results/region/GVA7/other_pop.pdf", height = 8, width = 17)
 myplot2(data = Y.other.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.other.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.other.hh$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.other.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.other.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.other.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()

data <- GetDataOmit(data = gva.cur.f.pop[gva.cur.f.pop$Region %in%
                    c("Eastern Europe", "Western Europe", "North America"), -3])

Y.america.euro.hh <- DoAll(X = data[, -c(1:3)],
                           cn.year = data[, c("Country", "Year")], k = 2)

pdf("../results/region/GVA7/america_euro_pop.pdf", height = 8, width = 17)
 myplot2(data = Y.america.euro.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.america.euro.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.america.euro.hh$Y.isoMDS, point = "2012", cn,
         title = "isoMDS")
 myplot2(data = Y.america.euro.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.america.euro.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.america.euro.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()








