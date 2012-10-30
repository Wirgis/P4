## PIUSD.xlsx: US dollars, US dollars per capita, US dollars per household;
## currency conversion: fixed 2011 exchange rate, year-on-year exchange rate;
pathPIlocal <- "data/rawdata/EMI_private/PIlocal_private.csv"
data.local <- read.csv(pathPIlocal)

pathPIUSD <- "data/rawdata/EMI_private/PIUSD_private.csv"
data.usd <- read.csv(pathPIUSD)

## countries
CN <- as.character(unique(data.local$Country))
CN <- sort(CN)
CN.encoding <- data.frame(Country = CN, id = c(1:length(CN)))
write.csv(CN.encoding, "data/rawdata/EMI_private/Country.id.csv",
          row.names = FALSE)

## categories and subcategories
Sub.encoding <- data.local[, c("Category", "Subcategory")]
Sub.encoding <- unique(Sub.encoding)
Sub.encoding$id <- 1:dim(Sub.encoding)[1]
write.csv(Sub.encoding, "data/rawdata/EMI_private/Category.id.csv",
          row.names = FALSE)

## encode PIlocal
data.local <- merge(data.local, CN.encoding, by = "Country")
data.local$Country <- data.local$id
data.local$id <- NULL

data.local <- merge(data.local, Sub.encoding, by = c("Category", "Subcategory"))
data.local$CategorySub <- data.local$id
data.local$id <- NULL
data.local$Category <- NULL
data.local$Subcategory <- NULL
data.local$Hierarchy.Level <- NULL
data.local <- data.local[, c("Country", "CategorySub",
                             colnames(data.local)[-c(1, dim(data.local)[2])])]
write.csv(data.local, "data/rawdata/PIlocal.csv", row.names = FALSE)

## encode PIUSD
data.usd <- merge(data.usd, CN.encoding, by = "Country")
data.usd$Country <- data.usd$id
data.usd$id <- NULL

data.usd <- merge(data.usd, Sub.encoding, by = c("Category", "Subcategory"))
data.usd$CategorySub <- data.usd$id
data.usd$id <- NULL
data.usd$Category <- NULL
data.usd$Subcategory <- NULL
data.usd$Hierarchy.Level <- NULL
data.usd <- data.usd[, c("Country", "CategorySub",
                             colnames(data.usd)[-c(1, dim(data.usd)[2])])]
write.csv(data.usd, "data/rawdata/PIUSD.csv", row.names = FALSE)





