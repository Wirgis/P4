####    Original file HHI21.xlsx
####    The distribution of households income.
hhi <- read.csv("../../data/csv_format/HHI21.csv")
hhi <- hhi[-1, ]

## Unnecessary columns;
hhi <- hhi[which(!colnames(hhi) %in%
                 c("Industry", "Edition", "Category", "ParentID", "ProductID",
                   "Lowest.Level", "Modelled", "Data.Type", "DataTypeID",
                   "Unit", "Unit.Multiplier", "Current.Constant",
                   "Currency.Conversion"))]

## colnames;
colnames(hhi) <- gsub("^X", "", colnames(hhi))

## subcategory;
hhi$Subcategory <- as.character(hhi$Subcategory)
hhi$Subcategory <- gsub("Households \\(\\% of Total\\) with an Annual Disposable Income Over US\\$","", hhi$Subcategory)
hhi$Subcategory <- gsub(" \\(PPP\\)","", hhi$Subcategory)
hhi$Subcategory <- gsub(", ","", hhi$Subcategory)
