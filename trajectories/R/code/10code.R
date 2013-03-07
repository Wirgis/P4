Divide <- function(x, y, by = c("Country", "CountryID")){
    x.melt <- melt(x, id = by)
    y.melt <- melt(y, id = by[1:3])
    xy <- merge(x.melt, y.melt, by = c(by[1:3], "variable"))
    xy$value <-xy$value.x/xy$value.y
    if(length(by) != 4)
        as.data.frame(cast(xy[!colnames(xy) %in% c("value.x", "value.y")],
                           Region + Country + CountryID ~ variable))
    else
        as.data.frame(cast(xy[!colnames(xy) %in% c("value.x", "value.y")],
                           Region + Country + CountryID + Subcategory ~ variable))
}

myplot <- function(data){
    #ggplot(data, aes(x = data[, 3], y = data[, 4],
    ggplot(data, aes(x = Comp1, y = Comp2,
                     col = Country,
                     linetype = Country)) +
                         geom_path(lwd = 0.8) +
            scale_linetype_manual(values = c(rep("solid", 7), rep("dashed", 7),
                                  rep("dotted", 7), rep("dotdash", 8),
                                  rep("longdash", 8))) +
             scale_color_manual(values = c(brewer.pal(7, "Set3"),
                                brewer.pal(7, "Set3"),
                                brewer.pal(7, "Set3"), brewer.pal(8, "Set3"),
                                brewer.pal(8, "Set3"))) +
             theme(legend.key.width = unit(1.5, "cm"),
                   legend.key.size = unit(0.48, "cm"),
                   legend.text = element_text(size = 9),
                   legend.title = element_text(size = 9),
                  ##panel.background =  theme_rect(fill = "white", colour = NA),
                   panel.border = element_rect(fill = NA, colour="grey50"),
                   panel.grid.major = element_line(colour = "grey90",
                   size = 0.2),
                   panel.grid.minor = element_line(colour = "grey98",
                   size = 0.5),
                   panel.margin = unit(0.25, "lines")
                   ) +
                   xlab("Comp1") + ylab("Comp2")
}

myplot2 <- function(data, point = "2012", cn, title){
    data <- merge(data, cn, by = "Country")
    q <- ggplot(data, aes(x = Comp1, y = Comp2, col = Country)) +
        geom_path() +
            geom_text(data = data[data$Year == point, ], aes(label = Code,
                                  x = Comp1, y = Comp2, size = 3.5),
                      hjust = 0, vjust = 0) +
                          theme(legend.position = "none") + labs(title = title)

    data.table <- unique(data[, c("Code", "Country")])
    data.table$y <- 1:dim(data.table)[1]
    data.table <- melt(data.table, id = "y")
    table <- ggplot(data.table, aes(x = variable, y = y, label = value)) +
        geom_text(size = 2.5) + xlab(NULL) + ylab(NULL) +
            theme(panel.background = element_rect(fill = "white", colour = NA),
                  axis.ticks = element_blank(), axis.text = element_blank(),
                  panel.grid.minor.y = element_blank())
    SetLayout(nrow = 1, ncol = 2)
    print(q, vp = subplot(1, 1))
    print(table, vp = subplot(1, 2))
}

SetLayout <- function(nrow, ncol){
  Layout <- grid.layout(nrow = nrow, ncol = ncol,
                        widths = unit(c(7, 1), "null"))
  grid.newpage()
  pushViewport(viewport(layout = Layout))
}

subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

DoAll <- function(X, cn.year, k = 2){
    ## Principal component analysis;
    res.pca <- prcomp(X, scale = TRUE)
    Y.pca <- cbind(cn.year, res.pca$x[, c(1:k)])
    colnames(Y.pca)[c(3, 4)] <- c("Comp1", "Comp2")

    ## Multidimensional scaling;
    dd <- dist(X)
    res.mds <- cmdscale(dd, k = k)
    Y.mds <- cbind(cn.year, res.mds)
    colnames(Y.mds)[c(3, 4)] <- c("Comp1", "Comp2")

    ## Non-metrical MDS;
    res.isoMDS <- isoMDS(dd, k = k)
    Y.isoMDS <- cbind(cn.year, res.isoMDS)
    colnames(Y.isoMDS)[c(3, 4)] <- c("Comp1", "Comp2")

    ## Kernel PCA;
    kpc <- kpca(~., data = X, kernel = "rbfdot", features = k)
    res.kpca <- rotated(kpc)
    Y.kpca <- cbind(cn.year, res.kpca)
    colnames(Y.kpca)[c(3, 4)] <- c("Comp1", "Comp2")

    ## t-SNE;
    ## res.tsne <- tsne(X, k = k, max_iter = 5000)
    ## Y.tsne <- cbind(cn.year, res.tsne)
    ## colnames(Y.tsne)[c(3, 4)] <- c("Comp1", "Comp2")

    ## Stochastic proximity embedding;
    res.spe <- spe(X, edim = k)
    Y.spe <- cbind(cn.year, res.spe$x)
    colnames(Y.spe)[c(3, 4)] <- c("Comp1", "Comp2")

    ## Local linear embedding;
    k.info <- calc_k(X, m = 2, plotres = FALSE)
    k.opt <- k.info[which.min(k.info$rho), "k"]
    res.lle <- lle(X, m = k, k = k.opt)
    Y.lle <- cbind(cn.year, res.lle$Y)
    colnames(Y.lle)[c(3, 4)] <- c("Comp1", "Comp2")

    list(Y.pca = Y.pca, Y.mds = Y.mds, Y.isoMDS = Y.isoMDS, Y.kpca = Y.kpca,
         Y.spe = Y.spe, Y.lle = Y.lle)
}

GetDataOmit <- function(data){
    #data <- melt(data[, -3], id = c("Region", "Country", "Subcategory"))
    data <- melt(data, id = c("Region", "Country", "Subcategory"))
    colnames(data)[4] <- "Year"
    data <- cast(data, Region + Country + Year ~ Subcategory)
    data.omit <- na.omit(data)
    data.omit$Country <- factor(as.character(data.omit$Country))
    data.omit$Region <- factor(as.character(data.omit$Region))
    data.omit
}

Cut <- function(data, first = 1977, from = 2000){
    data <- data[!colnames(data) %in% as.character(c(first:(from - 1)))]
    GetDataOmit(data)
}

Sample <- function(data, years = seq(1979, 2012, by = 3)){
    data <- data[colnames(data) %in% c("Region", "Country", "Subcategory",
                                       as.character(years))]
    GetDataOmit(data)
}


