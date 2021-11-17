
# simple VlnPlot: From Seurat::VlnPlot
# Usage: 
# exprs: expression data.frame
# idents: clusters
# features: gene

vlnplot <- function(exprs, 
                    idents, 
                    features, 
                    pt.size = 1,
                    adjust = 1,
                    cols = NULL,
                    combine = TRUE, ...){
  

  #print(features)
  data <- data.frame(exprs[features,])
  colnames(data) <- features
  cells <- names(idents)
  
  # set the parameters
  type = "violin"
  data <- data[cells, ,drop=FALSE] # filter cells that not in identity
  split <- NULL
  sort <- FALSE
  y.max <- max(data)
  adjust <- adjust 
  log <- FALSE
  
  plots <- lapply(X = features, FUN = function(x) {
    return(SingleExIPlot(type = type, data = data[, x, drop = FALSE], 
                         idents = idents, split = split, sort = sort, y.max = y.max, 
                         adjust = adjust, cols = cols, pt.size = pt.size, 
                         log = log))
  })
  if (combine) {
    plots <- CombinePlots(plots = plots, ncol = ncol, legend = "none")
  }
  return(plots)
  
}

SingleExIPlot <- function(data, idents, split = NULL, type = "violin", sort = FALSE, 
          y.max = NULL, adjust = 1, pt.size = 0, cols = NULL, log = FALSE) 
{
  set.seed(seed = 42)
  if (!is.data.frame(x = data) || ncol(x = data) != 1) {
    stop("'SingleExIPlot requires a data frame with 1 column")
  }
  feature <- colnames(x = data)
  data$ident <- idents
  if ((is.character(x = sort) && nchar(x = sort) > 0) || sort) {
    data$ident <- factor(x = data$ident, levels = names(x = rev(x = sort(x = tapply(X = data[, 
                                                                                             feature], INDEX = data$ident, FUN = mean), decreasing = grepl(pattern = paste0("^", 
                                                                                                                                                                            tolower(x = sort)), x = "decreasing")))))
  }
  if (log) {
    noise <- rnorm(n = length(x = data[, feature]))/200
    data[, feature] <- data[, feature] + 1
  }
  else {
    noise <- rnorm(n = length(x = data[, feature]))/1e+05
  }
  if (all(data[, feature] == data[, feature][1])) {
    warning(paste0("All cells have the same value of ", feature, 
                   "."))
  }
  else {
    data[, feature] <- data[, feature] + noise
  }
  axis.label <- ifelse(test = log, yes = "Log Expression Level", 
                       no = "Expression Level")
  y.max <- y.max %||% max(data[, feature])
  if (is.null(x = split) || type != "violin") {
    vln.geom <- geom_violin
    fill <- "ident"
  }
  else {
    data$split <- split
    vln.geom <- geom_split_violin
    fill <- "split"
  }
  switch(EXPR = type, violin = {
    x <- "ident"
    y <- paste0("`", feature, "`")
    xlab <- "Identity"
    ylab <- axis.label
    geom <- list(vln.geom(scale = "width", adjust = adjust, 
                          trim = TRUE), theme(axis.text.x = element_text(angle = 45, 
                                                                         hjust = 1)))
    jitter <- geom_jitter(height = 0, size = pt.size)
    log.scale <- scale_y_log10()
    axis.scale <- ylim
  }, ridge = {
    x <- paste0("`", feature, "`")
    y <- "ident"
    xlab <- axis.label
    ylab <- "Identity"
    geom <- list(geom_density_ridges(scale = 4), theme_ridges(), 
                 scale_y_discrete(expand = c(0.01, 0)), scale_x_continuous(expand = c(0, 
                                                                                      0)))
    jitter <- geom_jitter(width = 0, size = pt.size)
    log.scale <- scale_x_log10()
    axis.scale <- function(...) {
      invisible(x = NULL)
    }
  }, stop("Unknown plot type: ", type))
  plot <- ggplot(data = data, mapping = aes_string(x = x, y = y, 
                                                   fill = fill)[c(2, 3, 1)]) + labs(x = xlab, y = ylab, 
                                                                                    title = feature, fill = NULL) + theme_cowplot()
  plot <- do.call(what = "+", args = list(plot, geom))
  plot <- plot + if (log) {
    log.scale
  }
  else {
    axis.scale(min(data[, feature]), y.max)
  }
  if (pt.size > 0) {
    plot <- plot + jitter
  }
  if (!is.null(x = cols)) {
    if (!is.null(x = split)) {
      idents <- unique(x = as.vector(x = data$ident))
      splits <- unique(x = as.vector(x = data$split))
      labels <- if (length(x = splits) == 2) {
        splits
      }
      else {
        unlist(x = lapply(X = idents, FUN = function(pattern, 
                                                     x) {
          x.mod <- gsub(pattern = paste0(pattern, "."), 
                        replacement = paste0(pattern, ": "), x = x, 
                        fixed = TRUE)
          x.keep <- grep(pattern = ": ", x = x.mod, fixed = TRUE)
          x.return <- x.mod[x.keep]
          names(x = x.return) <- x[x.keep]
          return(x.return)
        }, x = unique(x = as.vector(x = data$split))))
      }
      if (is.null(x = names(x = labels))) {
        names(x = labels) <- labels
      }
    }
    else {
      labels <- unique(x = as.vector(x = data$ident))
    }
    plot <- plot + scale_fill_manual(values = cols, labels = labels)
  }
  return(plot)
}


CombinePlots <- function (plots, ncol = NULL, legend = NULL, ...) 
{
  plots.combined <- if (length(x = plots) > 1) {
    if (!is.null(x = legend)) {
      if (legend != "none") {
        plot.legend <- get_legend(plot = plots[[1]] + 
                                    theme(legend.position = legend))
      }
      plots <- lapply(X = plots, FUN = function(x) {
        return(x + NoLegend())
      })
    }
    plots.combined <- plot_grid(plotlist = plots, ncol = ncol, 
                                align = "hv", ...)
    if (!is.null(x = legend)) {
      plots.combined <- switch(EXPR = legend, bottom = plot_grid(plots.combined, 
                                                                 plot.legend, ncol = 1, rel_heights = c(1, 0.2)), 
                               right = plot_grid(plots.combined, plot.legend, 
                                                 rel_widths = c(3, 0.3)), plots.combined)
    }
    plots.combined
  }
  else {
    plots[[1]]
  }
  return(plots.combined)
}