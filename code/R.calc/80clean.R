rm(list = ls())
library(RSQLite)
library(XML)
source("code/R.calc/10code.R")

######### Load your database P4.sqlite
drv <- SQLite()
con <- dbConnect(drv, dbname = "output/R/P4.sqlite")
output <- "USA_177.gexf"

# sql <- "SELECT * FROM Countries"
# cntrs <- dbGetQuery(con, sql)
# 
# for(i in 1:nrow(cntrs)){
#     sql <- paste("SELECT *
#         FROM Corr_data_PC
#         WHERE Country1 = ", i, " AND Country2 = ", i,
#         sep = "")
#     data <- dbGetQuery(con, sql)
#     tmp <- data.frame(fromto = paste(data[,"Category1"], data[,"Category2"]), Corr = data[,"Corr"])
#     if(i == 1) alldata <- tmp
#     else {
#         alldata <- merge(alldata, tmp, by = "fromto", all = TRUE, sort = FALSE)
#         names(alldata) <- c("fromto", paste("Corr", 1:i, sep = "."))
#     }
# }

i <- 18
# count <- apply(alldata[, -1], 1, function(z){
#                                     val <- z[!is.na(z)]
#                                     length(which((!is.na(z)) & (length(unique(sign(val))) == 1)))})
# thr <- 6
# subdata <- alldata[count > thr,]
# tmp <- strsplit(as.character(subdata[, "fromto"]), " ")
# from <- unlist(lapply(tmp, function(z)z[1]))
# to <- unlist(lapply(tmp, function(z)z[2]))
# 
# sql <- paste("SELECT cn1.Country AS Country1,
#                 cn2.Country AS Country2,
#                 cat1.Subcategory AS Category1,
#                 cat2.Subcategory AS Category2,
#                 Corr
#         FROM Corr_data_PC as cor
#         LEFT JOIN Countries AS cn1 ON cor.Country1 = cn1.id_cn
#         LEFT JOIN Categories AS cat1 ON cor.Category1 = cat1.id_cat
#         LEFT JOIN Countries AS cn2 ON cor.Country2 = cn2.id_cn
#         LEFT JOIN Categories AS cat2 ON cor.Category2 = cat2.id_cat
#         WHERE Country1 = ", i, " AND Country2 = ", i,
#         " AND FromTo IN (\"", paste(from, to, collapse = "\",\""),"\")",
#         " AND (cat1.Hierarchy = 3 AND cat2.Hierarchy = 3)",
#         sep = "")

codes <- read.table("data/rawdata/EMI_private/Category.id.csv", header = TRUE, sep = ",")
codes <- codes[!is.na(codes[, "IndustryCode"]), "id"]

sql <- paste("SELECT cn1.Country AS Country1,
                cn2.Country AS Country2,
                cat1.Subcategory AS Category1,
                cat2.Subcategory AS Category2,
                Corr
        FROM Corr_data_PC as cor
        LEFT JOIN Countries AS cn1 ON cor.Country1 = cn1.id_cn
        LEFT JOIN Categories AS cat1 ON cor.Category1 = cat1.id_cat
        LEFT JOIN Countries AS cn2 ON cor.Country2 = cn2.id_cn
        LEFT JOIN Categories AS cat2 ON cor.Category2 = cat2.id_cat
        WHERE Country1 = ", i, " AND Country2 = ", i,
        " AND Category1 IN (\"", paste(codes, collapse = "\",\""),"\")",
        " AND Category2 IN (\"", paste(codes, collapse = "\",\""),"\")",
        sep = "")

Mygexf(con = con, sql = sql, output = output)

dbDisconnect(con)

#########################################

rm(list = ls())

thr <- 0.1
top <- 5
codes <- read.table("data/rawdata/EMI_private/IndustryCodes.csv", header = TRUE, sep = ",")
snot <- c(146, 157)
sids <- setdiff(codes[, "Id"], snot)
bnot <- c(12, 157, 133, 9, 162, 139, 109, 13)
bids <- setdiff(codes[, "Id"], bnot)

data <- read.table("data/rawdata/EMI_private/US_IO_2012.csv", header = TRUE, sep = ",")
output1 <- "USA_IO_2012_edges"
#output2 <- "USA_IO_2012_nodes"

suppliers <- c()
for(si in sids){
    tmp <- data[data[, "SupplierCode"] == si & data[, "BuyerCode"] %in% sids,]
    tmp$Share <- tmp[, "MatrixValue"] / sum(tmp[, "MatrixValue"])
    tmp$Share[tmp$Share < tail(sort(tmp$Share), top)[1]] <- 0
    suppliers <- rbind(suppliers, tmp)
}
#suppliers <- suppliers[is.finite(suppliers[, "Share"]) & suppliers[, "Share"] > thr,]
suppliers <- suppliers[is.finite(suppliers[, "Share"]) & suppliers[, "Share"] > 0,]
suppliers$MatrixValue <- NULL
names(suppliers) <- c("Source", "Target", "Weight")
write.table(suppliers, paste("output/R/", output1,"_supp_top", top, ".csv", sep = ""), row.names = FALSE, sep = ",")
#write.table(codes, paste("output/R/", output2,"_supp.csv", sep = ""), row.names = FALSE, sep = ",")

buyers <- c()
for(bi in bids){
    tmp <- data[data[, "BuyerCode"] == bi & data[, "SupplierCode"] %in% bids, ]
    tmp$Share <- tmp[, "MatrixValue"] / sum(tmp[, "MatrixValue"])
    tmp$Share[tmp$Share < tail(sort(tmp$Share), top)[1]] <- 0
    buyers <- rbind(buyers, tmp)
}
#buyers <- buyers[is.finite(buyers[, "Share"]) & buyers[, "Share"] > thr,]
buyers <- buyers[is.finite(buyers[, "Share"]) & buyers[, "Share"] > 0,]
buyers$MatrixValue <- NULL
names(buyers) <- c("Source", "Target", "Weight")
write.table(buyers, paste("output/R/", output1,"_buy_top", top, ".csv", sep = ""), row.names = FALSE, sep = ",")
#write.table(codes, paste("output/R/", output2,"_buy.csv", sep = ""), row.names = FALSE, sep = ",")

######################################

rm(list = ls())

library(igraph)

df_node <- read.table("output/R/USA_IO_2012_nodes.csv", header = TRUE, sep = ",")
top <- 5
output1 <- "USA_IO_2012_edges"

df_edge <- read.table(paste("output/R/", output1,"_buy_top", top, ".csv", sep = ""), header = TRUE, sep = ",")
gr_IO <- graph.data.frame(df_edge, directed = TRUE, vertices = df_node)

dgr <- degree(gr_IO, mode = "out")
id <- names(sort(dgr, dec = FALSE))
sapply(id, function(z)df_node[df_node[, "Id"] == z, "Label"])[1:10]

#id <- c("4", "25", "128", "62", "101", "86")
#id <- c("4", "55", "50", "53", "104", "195", "172")
id <- c("160", "3", "38", "137", "134", "135", "161", "16")
gr_sub <- induced.subgraph(gr_IO, id)
pdf("output/R/USA_IO_2012_edges_buy_top5.pdf", width = 16, height = 9)
plot(gr_sub,
     layout = layout.fruchterman.reingold,
     vertex.label = V(gr_sub)$Label,
     edge.arrow.size = 0.5,
     edge.label = format(E(gr_sub)$Weight, digits = 2),
     edge.label.cex = 1.5)
dev.off()


df_edge <- read.table(paste("output/R/", output1,"_supp_top", top, ".csv", sep = ""), header = TRUE, sep = ",")
gr_IO <- graph.data.frame(df_edge, directed = TRUE, vertices = df_node)

dgr <- degree(gr_IO, mode = "in")
id <- names(sort(dgr, dec = TRUE))
sapply(id, function(z)df_node[df_node[, "Id"] == z, "Label"])[1:10]

#id <- c("4", "25", "52", "128", "106", "101", "110", "42")
#id <- c("4", "55", "50", "173", "172", "18", "33")
id <- c("160", "170", "172", "3", "134", "135", "161")
gr_sub <- induced.subgraph(gr_IO, id)
pdf("output/R/USA_IO_2012_edges_supp_top5.pdf", width = 16, height = 9)
plot(gr_sub,
     layout = layout.fruchterman.reingold,
     vertex.label = V(gr_sub)$Label,
     edge.arrow.size = 0.5,
     edge.label = format(E(gr_sub)$Weight, digits = 2),
     edge.label.cex = 1.5)
dev.off()


df_node <- read.table("output/R/USA_177_nodes.csv", header = TRUE, sep = ",")
df_edge <- read.table("output/R/USA_177_edges.csv", header = TRUE, sep = ",")
#df_edge <- df_edge[abs(df_edge[, "Weight"]) >= 0.65, ]

gr_cor <- graph.data.frame(df_edge, directed = FALSE, vertices = df_node)
id <- c("71", "89", "115", "149")
#id <- c("37", "40", "43", "46", "72", "125", "143", "144")
#id <- c("103", "105", "130")
gr_sub <- induced.subgraph(gr_cor, id)
pdf("output/R/USA_177.pdf", width = 16, height = 9)
plot(gr_sub,
     layout = layout.fruchterman.reingold,
     vertex.label = V(gr_sub)$Label,
     edge.arrow.size = 0.5,
     edge.label = format(E(gr_sub)$Weight, digits = 2),
     edge.label.cex = 1.5)
dev.off()
