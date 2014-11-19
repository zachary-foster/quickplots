#===================================================================================================
#' A scatter plot of two quantitative and one categorical variables
#' 
#' A scatter plot that explores the relationship of two quantitave variables that are subdivided 
#' into 3-10 categories. Applies linear regression significance tests within categories and 
#' on category means. 95\% bootstrapped cofidence intervals of variable means are displayed on the
#' plot. 
#' 
#' @param data \code{(data.frame)} The data that will be plotted. Must have at least two numeric 
#' columns and one factor column, preferably ordered. 
#' @param cat_1 (\code{character, length == 1}) The name of column in \code{data} that stores the
#' categorical variable. This column should be a factor, preferably ordered, with 3 to 10 levels. 
#' @param quant_1 (\code{character, length == 1}) The name of column in \code{data} that stores the
#' quantative that will plotted on the x-axis.
#' @param quant_2 (\code{character, length == 1}) The name of column in \code{data} that stores the
#' quantative that will plotted on the y-axis. 
#' @param x_label (\code{character, length == 1}) The label of the x-axis
#' @param y_label (\code{character, length == 1}) The label of the y-axis
#' @param title (\code{character, length == 1}) The tile of the graph
#' 
#' @examples
#' # Plot sepal width vs sepal length for three species of iris:
#' cqq_scatter_1(data = iris, cat_1 = "Species", quant_1 = "Sepal.Width", quant_2 = "Sepal.Length")
#' 
#' @import ggplot2 boot plyr grid
#' 
#' @export
cqq_scatter_1 <- function(data, cat_1, quant_1, quant_2, x_label = quant_1, y_label = quant_2,
                        title = paste(quant_1, "vs", quant_2, "by", cat_1)) {
  data[[cat_1]] <- as.factor(data[[cat_1]])
  get_ggplot2_part <- function(my_plot, part) {
    my_build <- ggplot_build(my_plot)
    my_table <- ggplot_gtable(my_build)
    tmp <- subset(my_table$layout, name == part)
    my_table[tmp$t:tmp$b, tmp$l:tmp$r]
  }
  lm_eqn <- function(x, y) {
    m = lm(y ~ x);
    s = summary(m)
    eq <- substitute(atop(italic(y) == a + b*italic(x), italic(r)^2~"="~r2*","~~italic(p)~"="~pv), 
                     list(a = format(coef(m)[1], digits = 2), 
                          b = format(coef(m)[2], digits = 2), 
                          r2 = format(summary(m)$r.squared, digits = 2),
                          pv = format(s$coefficients[2, 4], digits = 2)))
    as.character(as.expression(eq));                 
  }
  common_theme <- theme(panel.grid = element_blank(),
                        plot.margin = unit(c(0, 0, 0, 0), "cm"),
                        panel.margin = unit(c(0, 0, 0, 0), "cm"),
                        panel.background = element_blank())
  # calculate summary statistics -------------------------------------------------------------------
  get_ci <- function(x) {
    my_mean <- function(x, i) mean(x[i])
    ci <- dlply(data, cat_1,
                function(a) boot.ci(boot(a[[x]], R = 1000, statistic = my_mean), type = "norm")$normal)
    ci <- as.data.frame(matrix(unlist(ci), ncol = 3, byrow = TRUE))
    names(ci) <- c("conf", "low", "high")
    return(ci)
  }
  means <- aggregate(data[, c(quant_1, quant_2)], data[, cat_1, drop = F], FUN = mean)
  quant_1_ci <- cbind(means, get_ci(quant_1))
  quant_2_ci <- cbind(means, get_ci(quant_2))
  # density plots ----------------------------------------------------------------------------------
  density_plot <- function(quant_1, cat_1, flip = FALSE) {
    out <- ggplot(data, aes_string(x = quant_1, color = cat_1, fill = cat_1, y='..scaled..')) +
      geom_density(alpha = .2, position = "identity", adjust = .8) +
      guides(color = FALSE, fill = FALSE) +
      scale_x_continuous(expand = c(0, 0)) + 
      coord_cartesian(ylim = c(0, 1.05), xlim = range(data[[quant_1]])) +
      common_theme
    if (flip) out <- out + coord_flip(ylim = c(0, 1.05), xlim = range(data[[quant_1]]))
    get_ggplot2_part(out, "panel")
  }
  x_density <- density_plot(quant_1, cat_1)
  y_density <- density_plot(quant_2, cat_1, flip = TRUE)
  # Main scatter plot ------------------------------------------------------------------------------
  xy_scatter <- ggplot(data, aes_string(x = quant_1, y = quant_2, color = cat_1)) +
    geom_smooth(color = "#000000", fill = "#FFFFFFFF", data = means, aes_string(x = quant_1, y = quant_2, group = 1), method = lm) +
    stat_density2d(data = data, alpha = .02, aes_string(fill = cat_1, color = NULL), geom="polygon", n = 200, bins= 50) +
    geom_point(alpha = .4, size = 2) +
    geom_point(data = means, aes_string(x = quant_1, y = quant_2, color = cat_1), size = 3) +
    geom_errorbar(data = quant_2_ci, aes_string(x = quant_1, y = quant_2, ymin = "low", ymax = "high", width = 0), size = 2) +
    geom_errorbarh(data = quant_1_ci, aes_string(x = quant_1, y = quant_2, xmin = "low", xmax = "high", height = 0), size = 2) +
    annotate("text", x = mean(range(data[[quant_1]])),
             y = max(data[[quant_2]])*.8,
             label = lm_eqn(means[[quant_1]], means[[quant_2]]),
             alpha = .7, 
             parse = TRUE) +
    guides(color = FALSE, fill = FALSE, size = FALSE) +
    labs(x = x_label, y = y_label) +
    scale_x_continuous(expand = c(1, 0)) +
    scale_y_continuous(expand = c(1, 0)) +
    coord_cartesian(xlim = range(data[[quant_1]]), ylim = range(data[[quant_2]])) +
    theme(axis.title = element_text(size = rel(1.3))) +
    common_theme
  xy_scatter_panel <- get_ggplot2_part(xy_scatter, "panel")
  xy_scatter_axis_l <- get_ggplot2_part(xy_scatter, "axis-l")
  xy_scatter_axis_b <- get_ggplot2_part(xy_scatter, "axis-b")
  xy_scatter_xlab <- get_ggplot2_part(xy_scatter, "xlab")
  xy_scatter_ylab <- get_ggplot2_part(xy_scatter, "ylab")
  # individual scatter plots -----------------------------------------------------------------------
  plot_text <- cbind(ddply(data, cat_1, function(x) lm_eqn(x[[quant_1]], x[[quant_2]])),
                     ddply(data, cat_1, function(x) mean(range(x[[quant_1]])))[2],
                     ddply(data, cat_1, function(x) mean(c(mean(range(x[[quant_2]])), max(x[[quant_2]]))))[2])
  names(plot_text) <- c(cat_1,"text", "x", "y")
  plot_text[[cat_1]] <- factor(plot_text[[cat_1]], ordered = TRUE, levels = levels(unique(data[[cat_1]]))) #makes same order as in data
  small_scatters <- ggplot(data, aes_string(x = quant_1, y = quant_2, color = cat_1)) +
    geom_smooth(method = lm, fill = "#FFFFFFFF") +
    geom_point() +
    geom_text(data = plot_text, aes(x = x, y = y, label = text), color = "#444444",  parse = TRUE, size = 5, fontface = "bold") +
    facet_wrap(as.formula(paste0(" ~ ", cat_1)), scales = "free", nrow = 1) +
    guides(color = FALSE) +
    labs(title = title) +
    scale_x_continuous(expand = c(.03, 0)) +
    theme(panel.grid = element_blank(), 
          axis.ticks = element_blank(), 
          axis.text  = element_blank(), 
          axis.title = element_blank(), 
          plot.margin = unit(c(0, .3, 0, -.1), "cm"), 
          panel.margin = unit(0, "cm"),
          panel.background = element_blank(),
          strip.background = element_blank(), 
          strip.text = element_text(size = rel(1.3)),
          title = element_text(size = rel(1.4)))
  small_scatters <- ggplot_gtable(ggplot_build(small_scatters))
  
  # Graph plots in grid ----------------------------------------------------------------------------
  draw_plot <- function(row, col, my_plot) {
    pushViewport(viewport(layout.pos.col = col, layout.pos.row = row))
    #    grid.rect()
    #     print(my_plot, newpage = FALSE)
    grid.draw(my_plot)
    upViewport()
  }
  grid.newpage()
  pushViewport(viewport(layout=grid.layout(nrow = 2, ncol = 1,
                                           widths = unit(c(1), "null"),
                                           heights = unit(c(0.3, 0.7), "null"))))
  draw_plot(1, 1, small_scatters)
  pushViewport(viewport(layout.pos.col = 1, layout.pos.row = 2))
  pushViewport(viewport(layout=grid.layout(nrow = 4, ncol = 4,
                                           widths = unit(c(0.03, 0.03, 0.82, 0.14), "null"),
                                           heights = unit(c(0.2, 0.68, 0.06, 0.06), "null"))))
  draw_plot(1, 3, x_density)
  draw_plot(2, 1, xy_scatter_ylab)
  draw_plot(2, 2, xy_scatter_axis_l)
  draw_plot(2, 3, xy_scatter_panel)
  draw_plot(2, 4, y_density)
  draw_plot(3, 3, xy_scatter_axis_b)
  draw_plot(4, 3, xy_scatter_xlab)
}
