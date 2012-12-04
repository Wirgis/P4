MakeGrowthRates <- function(data, years){
    len <- length(years)
    res <- foreach(ind = 2:len, .combine = cbind) %do% {
        (data[, years[ind]] - data[, years[(ind - 1)]])/data[, years[(ind - 1)]]
    }
    data[, years[-1]] <- res
    data[, years[1]] <- NULL
    data
}

ChangeInf <- function(data){
    data.Inf <- which(data == Inf, arr.ind = TRUE)
    for(i in 1:dim(data.Inf)[1])
        data[data.Inf[i, ][1], data.Inf[i, ][2]] = NA
    data
}

RemoveZero <-  function(data){
    data.zero <- which(apply(data[, -c(1, 2, 3)], 1,
                                      function(x) all(x == 0)))
    data <- data[-data.zero, ]
    data
}

RemoveNA <-  function(data){
    data.na <- which(apply(data[, -c(1, 2, 3)], 1,
                           function(x) all(is.na(x))))
    data[-data.na, ]
}

SetLayout <- function(ncol, nrow){
  Layout <- grid.layout(nrow = nrow, ncol = ncol,
                        widths = unit(rep(1, ncol),
                                      rep("null", (ncol + 1))))
  grid.newpage()
  pushViewport(viewport(layout = Layout))
}

subplot <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

InsertCorr <- function(data, sql.base, indexes, con, range1, range2){
    foreach(i = 1:dim(indexes)[2]) %do%{
        print(i)
        cor <- cor(data[indexes[, i][1], -c(1:2)],
                   data[indexes[, i][2], -c(1:2)], method = "kendall",
                   use = "complete.obs")
        if(range1 <= abs(cor) & abs(cor) <= range2) {
            sql <- paste(sql.base, "(",paste(data[indexes[, i][1], "Country"],
                                             data[indexes[, i][1],
                                                  "CategorySub"],
                                             data[indexes[, i][2], "Country"],
                                             data[indexes[, i][2],
                                                  "CategorySub"],
                                             Corr = round(cor, 2), sep = ","),
                         ")", sep = "")
            dbGetQuery(con, sql)
        }
    }
}


InsertCorr2 <- function(data1, data2, sql.base, con, range1, range2){
    for(i in 1:dim(data1)[1]){
        for(j in 1:dim(data2)[1]){
            print(paste(i, j))
            cor <- cor(data1[i, -c(1, 2)], data2[j, -c(1, 2)],
                       method = "kendall", use = "complete.obs")
            if(range1 <= abs(cor) & abs(cor) <= range2) {
                sql <- paste(sql.base, "(",paste(data1[i, "Country"],
                                                 data1[i, "CategorySub"],
                                                 data2[j, "Country"],
                                                 data2[j, "CategorySub"],
                                                 Corr = round(cor, 2),
                                                 sep = ","),
                             ")", sep = "")
                dbGetQuery(con, sql)
            }
        }
    }
}

CorrMatWithin <- function(con, cn, table, level, hierarchy, range1, range2,
                          RBGL = NULL, corrplot = NULL, hclust.method = NULL){
    ## categories which match the given hierarchy level
    cat <- hierarchy[hierarchy$Hierarchy.Level == level, "CategorySub"]

    ## form a query to select appropriate rows according to the specified
    ## hierarchy level and a country
    sql.cat <- paste(cat, collapse = ",")
    sql <- paste("SELECT * FROM", table, "WHERE Category1 IN","(", sql.cat, ")",
                 "AND Category2 IN ", "(", sql.cat, ")", "AND Country1 = ", cn,
                 "AND Country2 = ", cn)

    ## take the data
    data <- dbGetQuery(con, sql)

    ## assign zero for correlations in [range1, range2]
    data[range1 <= abs(data$Corr) & abs(data$Corr) <= range2, "Corr"] <- 0

    ## form a matrix and a graph
    from <- paste(data[, "Country1"], data[, "Category1"])
    to <- paste(data[, "Country2"], data[, "Category2"])
    weights <-  data[, "Corr"]

    g <- ftM2graphNEL(cbind(from, to), edgemode = "undirected", W = weights)
    g.matrix <- as(g, "matrix")

    ord <- if(!is.null(RBGL))
        switch(RBGL,
               cuthill.mckee.ordering = cuthill.mckee.ordering(g)$`reverse cuthill.mckee.ordering`,
               sloan.ordering = minDegreeOrdering(g)$inverse_permutation,
               minDegreeOrdering = sloan.ordering(g)$sloan.ordering)

    else
        switch(corrplot,
               AOE = corrMatOrder(g.matrix, order = "AOE"),
               FPC = corrMatOrder(g.matrix, order = "FPC"),
               hclust = corrMatOrder(g.matrix, order = "hclust",
               hclust.method = hclust.method))
    matrix.ord <- g.matrix[ord, ord]

    ## colours
    pal <- colorRampPalette(c("red", "yellow"), space = "rgb")

    levelplot(matrix.ord, main = paste("Country", cn), xlab="", ylab="",
              col.regions=pal(100), cuts = 99)
}

CorrMatBetween <- function(con, cn1, cn2, table, level, hierarchy,
                           range1, range2, RBGL = c("cuthill.mckee.ordering",
                                           "sloan.ordering",
                                           "minDegreeOrdering")){
    ## categories which match the given hierarchy level
    cat <- hierarchy[hierarchy$Hierarchy.Level == level, "CategorySub"]

    ## form a query to select appropriate rows according to the specified
    ## hierarchy level and a country
    sql.cat <- paste(cat, collapse = ",")
    sql <- paste("SELECT * FROM", table, "WHERE Category1 IN","(", sql.cat, ")",
                 "AND Category2 IN ", "(", sql.cat, ")", "AND Country1 = ", cn1,
                 "AND Country2 = ", cn2)

    ## take the data
    data <- dbGetQuery(con, sql)

    ## assign zero for correlations in [range1, range2]
    data[range1 <= abs(data$Corr) & abs(data$Corr) <= range2, "Corr"] <- 0

    ## form a matrix and a graph
    from <- paste(data[, "Country1"], data[, "Category1"])
    to <- paste(data[, "Country2"], data[, "Category2"])
    weights <-  data[, "Corr"]

    g <- ftM2graphNEL(cbind(from, to), edgemode = "directed", W = weights)
    g.matrix <- as(g, "matrix")

    ord <- switch(RBGL,
                  cuthill.mckee.ordering = cuthill.mckee.ordering(g)$`reverse cuthill.mckee.ordering`,
                  sloan.ordering = minDegreeOrdering(g)$inverse_permutation,
                  minDegreeOrdering = sloan.ordering(g)$sloan.ordering)

    matrix.ord <- g.matrix[ord, ord]

    ## colours
    pal <- colorRampPalette(c("red", "yellow"), space = "rgb")

    levelplot(matrix.ord, main = paste("Country", cn), xlab="", ylab="",
              col.regions=pal(100), cuts = 99)
}

MaxSelect <- function(con, node = c("Country" = 1, "Category" = 1), n = 10,
                      table = "Correlations", same.country = TRUE){

    if(!same.country){
        sql1 <- paste("SELECT * FROM", table , "WHERE Country1 = ",
                     node["Country"], "AND Category1 = ", node["Category"],
                     "OR Country2 != ", node["Country"], "AND Category2 = ",
                     node["Category"], "ORDER BY abs(Corr) Desc limit", n)
        which1 <- dbGetQuery(con, sql1)

        sql2 <- paste("SELECT * FROM", table , "WHERE Country1 != ",
                     node["Country"], "AND Category1 = ", node["Category"],
                     "OR Country2 = ", node["Country"], "AND Category2 = ",
                     node["Category"], "ORDER BY abs(Corr) Desc limit", n)
        which2 <- dbGetQuery(con, sql2)

        which <- rbind(which1, which2)
        which <- tail(sort_df(which, vars = "Corr"), n)

    } else {
        sql <- paste("SELECT * FROM", table , "WHERE Country1 = ",
                     node["Country"], "AND Category1 = ", node["Category"],
                     "OR Country2 = ", node["Country"], "AND Category2 = ",
                     node["Category"], "ORDER BY abs(Corr) Desc limit", n)
        which <- dbGetQuery(con, sql)
    }

    cat1 <- which[, "Category1"]
    cat2 <- which[, "Category2"]
    cn1 <- which[, "Country1"]
    cn2 <- which[, "Country2"]

    data <- c()
    for(i in 1:dim(which)[1]){
        sql <- paste("SELECT * FROM", table, "WHERE Category1 = ", cat1[i],
                     "AND Category2 = ", cat2[i], "AND Country1 = ", cn1[i],
                     "AND Country2 = ", cn2[i])
        data <- rbind(data, dbGetQuery(con, sql))
    }
    data
}

GraphOne <- function(con, node, n = 10, table, edge.labels = FALSE,
                     minimum = 0, center = TRUE, same.country = TRUE){
    data <- MaxSelect(con = con, node = node, n = n, table = table,
                      same.country = same.country)

    from <- paste(data[, "Country1"], data[, "Category1"])
    to <- paste(data[, "Country2"], data[, "Category2"])

    data.new <- data.frame(from, to, Corr = data[, "Corr"])

    if(center){
        target <- paste(node, collapse = " ")
        other <- unique(c(from, to))
        id <- which(target == other)
        groups <- list(target = id,
                       other = c(1:(n + 1))[-id])
        qgraph(data.new, directed = FALSE, edge.labels = edge.labels,
               groups = groups, minimum = minimum, layout = "spring")
    } else
        qgraph(data.new, directed = FALSE, edge.labels = edge.labels,
               minimum = minimum, layout = "circular", groups = groups)
}

Mygexf <- function(con, sql, output){
    ## extract info from database
    data <- dbGetQuery(con, sql)

    ## nodes
    from <- paste(data[, "Country1"], data[, "Category1"])
    to <- paste(data[, "Country2"], data[, "Category2"])
    unique.nodes <- unique(c(from, to))
    n.nodes <- length(unique.nodes)
    nodes <- matrix(c(1:n.nodes, unique.nodes), ncol = 2)

    ## ## edges
    edges <- matrix(c(sapply(from, function(x) which(x == nodes[, 2])),
                      sapply(to, function(x) which(x == nodes[, 2]))),
                    ncol = 2)

    ## ## add weights
    weights <- matrix(data[, "Corr"], ncol = 1)

    ## graph
    g <- xmlNode("graph", attrs = c(defaultedgetype="undirected",
                          idtype="string", type="static"))

    ## nodes
    g$children[[1]] <- xmlNode("nodes")

    for(i in 1:dim(nodes)[1]){
        g$children[[1]][[i]] <- xmlNode("node", attrs = c(id = nodes[i, 1],
                                                label = nodes[i, 2]))
    }

    ## edges
    g$children[[2]] <- xmlNode("edges")
    for(i in 1:dim(edges)[1]){
        g$children[[2]][[i]] <- xmlNode("edge", attrs = c(source = edges[i, 1],
                                                target = edges[i, 2],
                                                weight = weights[i]))
    }

    sink(output)
    print(g)
    sink()
}


