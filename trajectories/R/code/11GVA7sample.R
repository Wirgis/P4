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
data <- gva.cur.f.hh
sample <- seq(1979, 2012, by = 3)

data <- data[colnames(data) %in% c("Region", "Country", "Subcategory",
                                   as.character(sample))]
data <- melt(data, id = c("Region", "Country", "Subcategory"))
colnames(data)[4] <- "Year"
data <- cast(data, Region + Country + Year ~ Subcategory)
data.omit <- na.omit(data)
data.omit$Country <- factor(as.character(data.omit$Country))
data.omit$Region <- factor(as.character(data.omit$Region))

Y.hh <- DoAll(X = data.omit[, -c(1:3)],
              cn.year = data.omit[, c("Country", "Year")], k = 2)

pdf("../results/sample/GVA7/3_hh.pdf", height = 8, width = 17)
 myplot2(data = Y.hh$Y.pca, point = "2012", cn, title = "PCA")
 myplot2(data = Y.hh$Y.mds, point = "2012", cn, title = "MDS")
 myplot2(data = Y.hh$Y.isoMDS, point = "2012", cn, title = "isoMDS")
 myplot2(data = Y.hh$Y.kpca, point = "2012", cn, title = "KPCA")
 myplot2(data = Y.hh$Y.spe, point = "2012", cn, title = "SPE")
 myplot2(data = Y.hh$Y.lle, point = "2012", cn, title = "LLE")
dev.off()
