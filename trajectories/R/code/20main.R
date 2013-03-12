## libraries
library(ggplot2)
library(grid)
library(stats)
library(MASS)
library(kernlab)
library(tsne)
library(spe)
library(lle)

## short names of countries, labels of trajectories
cn <- read.csv("../../data/csv_format/countries.csv")

######################### load the data
## use one of the scripts:
##  1) "02readGVA7.R" - Gross Value Added. Available data sets:
##    a) gva.con.f.hh - constant prices, fixed exchange rate (2012),
##                      per 100 of households;
##    b) "gva.con.f.pop" - constant prices, fixed exchange rate (2012),
##                         per 1000 of population;
##    c) "gva.cur.f.hh" -  current prices, fixed exchange rate (2012),
##                         per 100 of households;
##    d) "gva.cur.f.pop" -  current prices, fixed exchange rate (2012),
##                          per 1000 of population;
##    e) "gva.cur.y.hh" -  current prices, year-on-year exchange rate,
##                         per 1000 of households;
##    f) "gva.cur.y.pop" -  current prices, year-on-year exchange rate,
##                          per 1000 of population;
##    g) "gva.gdp" - per one GDP unit;
##
##  2) "03readCE12.R" - Consumer Expenditure. Available data sets:
##    a) ce.con.f.hh - constant prices, fixed exchange rate (2012),
##                      per 100 of households;
##    b) "ce.con.f.pop" - constant prices, fixed exchange rate (2012),
##                         per 1000 of population;
##    c) "ce.cur.f.hh" -  current prices, fixed exchange rate (2012),
##                         per 100 of households;
##    d) "ce.cur.f.pop" -  current prices, fixed exchange rate (2012),
##                          per 1000 of population;
##    e) "ce.cur.y.hh" -  current prices, year-on-year exchange rate,
##                         per 1000 of households;
##    f) "ce.cur.y.pop" -  current prices, year-on-year exchange rate,
##                          per 1000 of population;
##    g) "ce.gdp" - per one GDP unit;
##
##  3) "04readHHI21.R" - Distribution of Housefolds according to the annual
##                       disposable income level. The data set - hhi.
##  4) "05readPOP17.R" - Population by age groups. The data set - pop17.

source("02readGVA7.R")

## chose the data frame
data <- gva.cur.f.hh

############### prepare the data for dimensionality reduction methods
## Usefull functions:
##                   1) GetDataOmit(data). Returns the data frame which is
##                      used as an input of the DoAll() function. na.omit()
##                      method for dealing with NA's is used.
##                   2) Cut(data, from). Returns the data frame of the period -
##                      'from' - 2012 which is used as an input of the DoAll()
##                      function.
##                   3) Sample(data, years). Returns the data frame of years
##                     suplied the argument 'years' and suitable for the DoAll().

data.full <- GetDataOmit(data)
data.cut <- Cut(data, from = 2000)
data.sample <- Sample(data, years = seq(1979, 2012, by = 3))

############## run dimensionality reduction methods
## Usefull function:
##                  1) DoAll(X, cn.year, k, tsne, max.iter.tsne)
##                      Arguments:
##                           'X' - a data.frame of the dimension (n x D),
##                              where n - samples, D - features.
##                           'cn.years' - a data frame of the dimension (n x 2).
##                              The first is the Country column, the second -
##                              the Year column.
##                           'k' - an integer indicating the number of features
##                              to return.
##                           'tsne' - logical. If TRUE t-SNE method is carried
##                              out. The default is FALSE.
##                           'max.iter.tsne' - an integer. If tsne = TRUE, then
##                              it sets maximum number of iterations to perform
##                              for a t-SNE method.
##                      Returns the list of objects of dimensions (n x [2+k])
##                           1) Y.pca - after Principal Componenst Analysis;
##                           2) Y.mds - after the classical Multidimensional
##                                      scaling;
##                           3) Y.isoMDS - after the non-metric Multidimensional
##                                         scaling;
##                           4) Y.kpca - after kernel PCA;
##                           5) Y.spe - after Stochastic Proximity Embedding;
##                           6) Y.lle - after Local Linear Embedding;
##                           7) Y.tsne - after t-Distributed Stochastic
##                                       Neighbour Embedding.

Y <- DoAll(X = data.full[, -c(1:3)], cn.year = data.full[, c("Country", "Year")],
           k = 2, tsne = FALSE)

################### make a graph
## Usefull function:
##          myplot2(data, point, cn, title)
##           Arguments:
##                  data - a data.frame with columns 'Country', 'Year', 'Comp1',
##                         'Comp2';
##                  point - a character indicating the point where short country
##                          names should be;
##                  cn - a data frame with two columns 'Code' - short names of
##                       countries and 'Country' - full names of countries.
##                  title - a character indicating the title of the graph.

cn <- read.csv("../../data/csv_format/countries.csv")
myplot2(data = Y$Y.pca, point = "2012", cn = cn, title = "PCA")

