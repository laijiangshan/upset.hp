#' @title Visualization of VP and HP Using UpSet Diagram
#'
#' @description Visualization of variation partitioning (VP) and hierarchical partitioning (HP) with unlimited number of predictor variables (or matrices of predictors) using UpSet matrix layout.
#'
#' @param vp A matrix, which contains the output of variation partitioning (i.e. commonality analysis) from \code{rdacca.hp},\code{glmm.hp},\code{gam.hp},and \code{phylolm.hp}.
#' @param hp A matrix, which contains the output of hierarchical partitioning from \code{rdacca.hp},\code{glmm.hp},\code{gam.hp},and \code{phylolm.hp}.
#' @param plot.hp The default is \code{TRUE}, which plots the individual effect for each predictor on left column diagram. If \code{FALSE}, compute and plot the sum of unique effect and common effect for each predictor.
#' @param order.part How the VP components in matrix layout should be ordered. Options include \code{"effect"} (order the intersections by their effects) or \code{"degree"} (sort by the number of predictors involved in the intersection), default is \code{"effect"}.
#' @param decreasing.part How the intersections in \code{order.part} should be ordered. Default is \code{TRUE}, \code{"effect"} is decreasing (from greatest to least) or \code{"degree"} is increasing (from least to greatest).
#' @param order.var The predictors in the matrix layout should be ordered by. Default is \code{TRUE}, which orders the predictors by their effects. IF \code{FALSE}, sort by the order of predictors in input data.
#' @param decreasing.var If \code{order.var=TRUE}, how the predictors should be ordered. Default is \code{TRUE}, from greatest to least.
#' @param cutoff Effects below \code{cutoff} will not be displayed, default is \code{-1}. Note: Negative effects due to adjustment of R-squared mean negligible contributions, but they are included in the computation of the total contribution of each predictor category.
#' @param nVar Number of components in VP to plot, default is \code{30}.
#' @param col.width Width of bars in column diagram, default is \code{0.6}.
#' @param pch.size Size of points in matrix diagram, default is \code{3}.
#' @param line.lwd Width of lines in matrix diagram, default is \code{0.5}.
#' @param show.effect Show the relative importance of predictors (unique, common, or individual effects) above bars, default is \code{TRUE}.
#' @param effect.cex Font size of the effects, default is \code{2.7}.
#' @param title.cex Font size of axis titles, default is \code{10}.
#' @param axis.cex Font size of axis labels, default is \code{8}.
#' @param height.ratio Ratio between matrix and top column diagram, default is \code{c(2, 1)}.
#' @param width.ratio Ratio between matrix and left column diagram, default is \code{c(1, 3)}.
#' @param col Character. Color palette name: "nature" (default), "science", "cell", "bw" (black-white), or "cvd" (color-blind friendly).
#' @param digits Integer. Number of decimal places to display for effect values, default is \code{2}.
#'
#'
#' @details upset.hp diagram is an extension of UpSet technique to  and is used to visualize the object of \code{rdacca.hp},\code{glmm.hp},\code{gam.hp},and \code{phylolm.hp} (Lai et al. 2022a,2022b,2023,2024; Liu et al. 2023). The matrix layout enables the effective representation of relative importance of predictors, such as the unique effects and common effects in VP, as well as additional summary statistics or individual effects in HP. upset.hp diagram could, in principle, allow visualization of any number of predictor variables or groups of predictor variables. But considering the interpretability of data, we would like to recommend that the number of predictors (or groups of predictors) no more than 7.
#'
#' @return \itemize{Returns a ggplot2.}
#'
#' @references Lai J., Zou Y., Zhang J., Peres-Neto P. (2022) Generalizing hierarchical and variation partitioning in multiple regression and canonical analyses using the rdacca.hp R package. Methods in Ecology and Evolution, 13:782-788.
#' @references Lai J.,Zou Y., Zhang S.,Zhang X.,Mao L.(2022)glmm.hp: an R package for computing individual effect of predictors in generalized linear mixed models.Journal of Plant Ecology,15(6):1302-1307<DOI:10.1093/jpe/rtac096>
#' @references Lai J.,Zhu W., Cui D.,Mao L.(2023)Extension of the glmm.hp package to Zero-Inflated generalized linear mixed models and multiple regression.Journal of Plant Ecology,16(6):rtad038<DOI:10.1093/jpe/rtad038>
#' @references Liu Y., Yu X., Yu Y., et al. (2023) Application of "rdacca. hp" R package in ecological data analysis: case and progress. Chinese Journal of Plant Ecology, 27:134-144.
#' @references Lai J.,Tang J., Li T., Zhang A.,Mao L.(2024)Evaluating the relative importance of predictors in Generalized Additive Models using the gam.hp R package.Plant Diversity,46(4):542-546<DOI:10.1016/j.pld.2024.06.002>

#' @export
#' @examples
#' library(glmm.hp)
#' #upset for glmm.hp() in lm()
#' m2<-lm(mpg~wt+carb+cyl,mtcars)
#' vp <- glmm.hp(m2,commonality=TRUE)$commonality.analysis
#' hp <- glmm.hp(m2)$hierarchical.partitioning
#' upset.hp(vp, hp, col = "cvd",digit = 1)



upset.hp <- function(vp, hp, plot.hp = TRUE, order.part = "effect", decreasing.part = TRUE,
                     order.var = TRUE, decreasing.var = TRUE, cutoff = -1, nVar = 30,
                     col.width = 0.6, pch.size = 3, line.lwd = 0.5, show.effect = TRUE, 
                     effect.cex = 2.7, title.cex = 10, axis.cex = 8, height.ratio = c(2, 1),
                     width.ratio = c(1, 3), col = "nature", digits = 2) {
  
  # Define color palettes
  color_palettes <- list(
    nature = list(
      pos_bars = c("#4E79A7", "#A0CBE8", "#76B7B2", "#59A14F", "#8CD17D"),
      neg_bars = c("#FFBE7D", "#F28E2B", "#E15759", "#FF9D9A", "#B07AA1"),
      point_active = "#000000",
      point_inactive = "#D3D3D3",
      tile_even = "#F7F7F7",
      tile_odd = "#FFFFFF",
      line_color = "#636363",
      effect_text = "#000000",
      axis_text = "#000000"
    ),
    science = list(
      pos_bars = c("#1F77B4", "#8C564B", "#E377C2", "#7F7F7F", "#BCBD22"),
      neg_bars = c("#FF7F0E", "#D62728", "#9467BD", "#17BECF", "#2CA02C"),
      point_active = "#000000",
      point_inactive = "#C7C7C7",
      tile_even = "#F0F0F0",
      tile_odd = "#FFFFFF",
      line_color = "#525252",
      effect_text = "#000000",
      axis_text = "#000000"
    ),
    cell = list(
      pos_bars = c("#3E7DCC", "#63B5F7", "#A5D8FF", "#4DBBD5", "#00A087"),
      neg_bars = c("#E64B35", "#F39B7F", "#FEB24C", "#FFD700", "#91D1C2"),
      point_active = "#000000",
      point_inactive = "#DFDFDF",
      tile_even = "#F5F5F5",
      tile_odd = "#FFFFFF",
      line_color = "#737373",
      effect_text = "#000000",
      axis_text = "#000000"
    ),
    bw = list(
      pos_bars = rep("#666666", 5),
      neg_bars = rep("#CCCCCC", 5),
      point_active = "#000000",
      point_inactive = "#FFFFFF",
      tile_even = "#F0F0F0",
      tile_odd = "#FFFFFF",
      line_color = "#333333",
      effect_text = "#000000",
      axis_text = "#000000"
    ),
    cvd = list(
      pos_bars = c("#4477AA", "#66CCEE", "#228833", "#CCBB44", "#EE6677"),
      neg_bars = c("#AA3377", "#BBBBBB", "#EE8866", "#FFAABB", "#AAAA00"),
      point_active = "#000000",
      point_inactive = "#DDDDDD",
      tile_even = "#F7F7F7",
      tile_odd = "#FFFFFF",
      line_color = "#333333",
      effect_text = "#000000",
      axis_text = "#000000"
    )
  )
  
  # Select color palette
  palette <- color_palettes[[col]]
  if (is.null(palette)) palette <- color_palettes$nature
  
  Constrained <- round(100 * tail(vp[,1],1), digits)
  Var.part <- as.data.frame(vp)
  Var.part <- Var.part[-nrow(Var.part), ]
  Var.part$Var <- gsub("^\\s+|\\s+$", "", gsub("and ", "",
                                               gsub("Common to ", "", gsub("Unique to ", "", rownames(Var.part)))))
  Hier.part <- as.data.frame(hp)
  Hier.part$Var <- rownames(Hier.part)
  Var.part$Fractions <- round(100 * Var.part$Fractions, digits)
  Hier.part$Individual <- round(100 * Hier.part$Individual, digits)
  
  # Unique effect and common effect in VP
  Var.part$inter <- lengths(strsplit(Var.part$Var, ", "))
  Var.part$valid <- apply(Var.part[1], 1, function(x) ifelse(x <= 0, "0", "1"))
  Var.part <- Var.part[which(Var.part$Fractions >= 100*cutoff), ]
  if (order.part == "effect")
    Var.part <- Var.part[order(Var.part$Fractions, decreasing = decreasing.part), ]
  else if (order.part == "degree")
    Var.part <- Var.part[order(Var.part$inter, Var.part$Fractions, decreasing = c(!decreasing.part, TRUE)), ]
  if (nrow(Var.part) > nVar)
    Var.part <- Var.part[1:nVar, ]
  Var.part$Var <- factor(Var.part$Var, levels = Var.part$Var)
  
  # Assign colors to each bar
  Var.part$color <- ifelse(Var.part$Fractions >= 0, 
                           palette$pos_bars[(seq_len(nrow(Var.part))-1) %% length(palette$pos_bars) + 1],
                           palette$neg_bars[(seq_len(nrow(Var.part))-1) %% length(palette$neg_bars) + 1])
  
  p.vp <- ggplot2::ggplot(data = Var.part, aes_string(x = 'Var', y = 'Fractions', fill = 'color')) +
    ggplot2::geom_col(width = col.width) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_blank(),
                   axis.line.y = element_line(color = 'black'),
                   axis.text.x = element_blank(),
                   axis.text.y = element_text(color = palette$axis_text, size = axis.cex),
                   axis.ticks.x = element_blank(),
                   axis.ticks.y = element_line(color = 'black'),
                   axis.title = element_text(color = palette$axis_text, size = title.cex),
                   plot.title = element_text(hjust = 0.5, size = title.cex),
                   legend.position = 'none') +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(ifelse(min(Var.part$Fractions) < 0, 0.1, 0), 0.1))) +
    ggplot2::labs(y = 'Fractions (%)',x = '',title = bquote(R^2 == .(Constrained)*"%" ~ "  Residual" ~ .(100 - Constrained)*"%"))
  
  if (show.effect)
    p.vp <- p.vp + ggplot2::geom_text(aes_string(label = sprintf('sprintf("%%.%df", Fractions)', digits), 
                                                 vjust = ifelse(Var.part$Fractions >= 0, -0.2, 1.2)), 
                                      color = palette$effect_text, size = effect.cex)
  p.vp <- p.vp + ggplot2::geom_hline(yintercept = 0)
  
  # Sum of unique effect and common effect
  Fractions <- NULL
  for (i in rownames(Hier.part))
    Fractions <- c(Fractions, sum(Var.part[grep(i, Var.part$Var),"Fractions"]))
  Var.exp <- data.frame(Var = rownames(Hier.part), Fractions = round(Fractions, digits))
  Var.exp$valid <- apply(Var.exp[2], 1, function(x) ifelse(x <= 0, "0", "1"))
  Var.exp <- Var.exp[which(Var.exp$Fractions >= 100*cutoff), ]
  if (order.var)
    Var.exp <- Var.exp[order(Var.exp$Fractions, decreasing = !decreasing.var), ]
  Var.exp$Var <- factor(Var.exp$Var, levels = Var.exp$Var)
  
  # Assign colors to each bar
  Var.exp$color <- ifelse(Var.exp$Fractions >= 0, 
                          palette$pos_bars[(seq_len(nrow(Var.exp))-1) %% length(palette$pos_bars) + 1],
                          palette$neg_bars[(seq_len(nrow(Var.exp))-1) %% length(palette$neg_bars) + 1])
  
  p.exp <- ggplot2::ggplot(data = Var.exp, aes_string(x = 'Var', y = 'Fractions', fill = 'color')) +
    ggplot2::geom_col(width = col.width) +
    ggplot2::scale_fill_identity() +
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_blank(),
                   axis.line.x = element_line(color = 'black'),
                   axis.text.x = element_text(color = palette$axis_text, size = axis.cex),
                   axis.text.y = element_blank(),
                   axis.ticks.x = element_line(color = 'black'),
                   axis.ticks.y = element_blank(),
                   axis.title = element_text(color = palette$axis_text, size = title.cex),
                   legend.position = 'none') +
    ggplot2::coord_flip() +
    ggplot2::scale_y_reverse(expand = ggplot2::expansion(mult = c(0.3, ifelse(min(Var.exp$Fractions) < 0, 0.3, 0)))) +
    ggplot2::labs(y = 'Fractions (%)', x = NULL)
  
  if (show.effect)
    p.exp <- p.exp + ggplot2::geom_text(aes_string(label = sprintf('sprintf("%%.%df", Fractions)', digits), 
                                                   hjust = ifelse(Var.exp$Fractions >= 0, 1.2, -0.2)), 
                                        color = palette$effect_text, size = effect.cex)
  p.exp <- p.exp + ggplot2::geom_hline(yintercept = 0)
  
  # Individual effect in HP
  if (plot.hp) {
    Hier.part <- Hier.part[which(Hier.part$Individual >= 100*cutoff), ]
    if (order.var)
      Hier.part <- Hier.part[order(Hier.part$Individual, decreasing = !decreasing.var), ]
    Hier.part$Var <- factor(Hier.part$Var, levels = Hier.part$Var)
    Hier.part$valid <- apply(Hier.part[3], 1, function(x) ifelse(x <= 0, "0", "1"))
    
    # Assign colors to each bar
    Hier.part$color <- ifelse(Hier.part$Individual >= 0, 
                              palette$pos_bars[(seq_len(nrow(Hier.part))-1) %% length(palette$pos_bars) + 1],
                              palette$neg_bars[(seq_len(nrow(Hier.part))-1) %% length(palette$neg_bars) + 1])
    
    p.hp <- ggplot2::ggplot(data = Hier.part, aes_string(x = 'Var', y = 'Individual', fill = 'color')) +
      ggplot2::geom_col(width = col.width) +
      ggplot2::scale_fill_identity() +
      ggplot2::theme(panel.grid = element_blank(),
                     panel.background = element_blank(),
                     axis.line.x = element_line(color = 'black'),
                     axis.text.x = element_text(color = palette$axis_text, size = axis.cex),
                     axis.text.y = element_blank(),
                     axis.ticks = element_line(color = 'black'),
                     axis.ticks.y = element_blank(),
                     axis.title = element_text(color = palette$axis_text, size = title.cex),
                     legend.position = 'none') +
      ggplot2::coord_flip() +
      ggplot2::scale_y_reverse(expand = ggplot2::expansion(mult = c(0.3, ifelse(min(Hier.part$Individual) < 0, 0.3, 0)))) +
      ggplot2::labs(y = 'Individual (%)', x = NULL)
    
    if (show.effect)
      p.hp <- p.hp + ggplot2::geom_text(aes_string(label = sprintf('sprintf("%%.%df", Individual)', digits), 
                                                   hjust = ifelse(Hier.part$Individual >= 0, 1.2, -0.2)), 
                                        color = palette$effect_text, size = effect.cex)
    p.hp <- p.hp + ggplot2::geom_hline(yintercept = 0)
  }
  
  # UpSet matrix plot
  panel <- NULL
  if (plot.hp)
    Var <- Hier.part
  else
    Var <- Var.exp
  for (i in Var$Var)
    for (j in Var.part$Var)
      panel <- rbind(panel, c(i, j, 0))
  panel <- data.frame(panel, stringsAsFactors = FALSE)
  panel <- panel[which(panel$X2 %in% Var.part$Var), ]
  panel$X1 <- factor(panel$X1, levels = Var$Var)
  panel$X2 <- factor(panel$X2, levels = Var.part$Var)
  for (i in 1:nrow(Var.part)) {
    i <- as.character(Var.part[i,"Var"])
    for (j in unlist(strsplit(i, ", "))) panel[which(panel$X1 == j & panel$X2 == i),"X3"] <- "1"
  }
  panel$X4 <- apply(panel, 1, function(x) ifelse(which(levels(panel$X1) == x[1])%%2 == 0, "0", "1"))
  
  p.panel <- ggplot2::ggplot(data = panel, aes_string(x = 'X2', y = 'X1', color = 'X3', fill = 'X4')) +
    ggplot2::geom_tile(color = NA) +
    ggplot2::geom_point(size = pch.size) +
    ggplot2::scale_fill_manual(values = c(palette$tile_even, palette$tile_odd), limits = c('0', '1')) +
    ggplot2::scale_color_manual(values = c(palette$point_inactive, palette$point_active), limits = c('0', '1')) +
    ggplot2::theme(panel.grid = element_blank(),
                   panel.background = element_blank(),
                   axis.text = element_text(color = palette$axis_text, size = title.cex),
                   axis.ticks = element_blank(),
                   axis.text.x = element_blank(),
                   legend.position = 'none') +
    ggplot2::scale_x_discrete(expand = c(0, 0)) +
    ggplot2::scale_y_discrete(expand = c(0, 0)) +
    ggplot2::labs(y = NULL, x = NULL)
  
  for (i in levels(panel$X2)) {
    panel.i <- panel[which(panel$X2 == i & panel$X3 == 1), ]
    panel.i <- panel.i[order(panel.i$X1), ]
    if (nrow(panel.i) > 1)
      p.panel <- p.panel + ggplot2::annotate("segment", x = i, xend = i, y = panel.i[1,1], yend = panel.i[nrow(panel.i),1], 
                                             color = palette$line_color, size = line.lwd)
  }
  
  # Draw VP or HP
  design <- c(patchwork::area(1, 1+width.ratio[1], 1+height.ratio[1], 1+width.ratio[1]+width.ratio[2]), 
              patchwork::area(1+height.ratio[1]+height.ratio[2], 1+width.ratio[1], 1+width.ratio[1]+width.ratio[2], 1+width.ratio[1]+width.ratio[2]),
              patchwork::area(1+height.ratio[1]+height.ratio[2], 1, 1+width.ratio[1]+width.ratio[2], 1))
  if (plot.hp)
    p.vp + p.panel + p.hp + patchwork::plot_layout(design = design)
  else
    p.vp + p.panel + p.exp + patchwork::plot_layout(design = design)
}
