library(ggplot2)

tableau10 <- c(
  "#4E79A7", # blue
  "#F28E2B", # orange
  "#E15759", # red
  "#76B7B2", # teal
  "#59A14F", # green
  "#EDC948", # yellow
  "#B07AA1", # purple
  "#FF9DA7", # pink
  "#9C755F", # brown
  "#BAB0AB"  # gray
)

trace_plot <- function(samples, param_index = 1, thin = 1L, add = F, ...) {
  plot_args <- list(...)
  if (!("ylab" %in% names(plot_args))) { 
    plot_args$ylab <- "Parameter value" 
  }
  if (!("col" %in% names(plot_args))) {
    plot_args$col <- "#002D72"
  }
  
  if(is.null(dim(samples))) {
    samples <- matrix(samples, nrow = 1)
  }
  n_iter <- dim(samples)[2]
  sub_indices <- seq(1, n_iter, by = thin)
  y <- as.vector(samples[param_index, sub_indices])
  plot_args <- c(
    plot_args,
    list(
      x = sub_indices, y = y,
      cex = 1.2,
      lwd = 1.2
    )
  )
  if (!add) {
    plot(
      sub_indices, y, 
      ylim = plot_args$ylim,
      frame = FALSE, 
      xlab = "MCMC iteration",
      ylab = plot_args$ylab,
      cex.lab = 1.2, 
      cex.main = 1.3,
      type = "n"
    )
  }
  do.call(points, plot_args)
}

auto_cor_plot <- function(coord_samples, lag.max = NULL) {
  acf(
    coord_samples, 
    lag.max = lag.max,
    ylab = "Auto-correlation",
    main = NA,
    frame = FALSE,
    col = "#002D72",
    lwd = 1.2
  )
}

hist_against_truth <- function(
    coord_samples, true_density, xlim = NULL, breaks = 51
  ) {
  
  if (is.null(xlim)) { 
    x <- seq(min(coord_samples), max(coord_samples), length.out=10000)
  } else {
    x <- seq(xlim[1], xlim[2], length.out=10000)
  }
  y <- true_density(x)
  
  h <- hist(coord_samples, breaks = breaks, plot=FALSE)
  if (is.null(xlim)) { xlim <- range(h$breaks) } 
    # Work around that `xlim=NULL` isn't allowed when plotting a histogram
  plot(
    x = h, freq = FALSE,
    col = "#bdcedb", border = "#859bad",
    xlim = xlim,
    ylim = c(0, max(max(h$density), 1.1 * max(y))),
    xlab="Parameter value",
    main = NULL
  )
  lines(x, y, lty=1)
}

plot_trajectory <- function(
    samples, index, n_iter_to_plot, target_density,
    x_range = c(-4, 4), y_range = c(-4, 4)
  ) {
  
  bivar_samples <- data.frame(
    x = samples[index[1], 1:n_iter_to_plot],
    y = samples[index[2], 1:n_iter_to_plot],
    iter = 1:n_iter_to_plot
  )
  
  x_grid <- seq(x_range[1], x_range[2], length = 251)
  y_grid <- seq(y_range[1], y_range[2], length = 251)
  grid <- expand.grid(x = x_grid, y = y_grid)
  grid$z <- target_density(cbind(grid$x, grid$y))
  
  ggplot() +
    # Background heatmap
    geom_raster(data = grid, aes(x = x, y = y, fill = z), alpha = .85) +
    scale_fill_viridis_c(option = "magma", guide = "none") +
    # Markov chain trajectory
    geom_path(data = bivar_samples, aes(x = x, y = y, color = iter), linewidth = 0.6) +
    scale_color_viridis_c(name = " MCMC\niteration", option = "viridis") +
    labs(x = bquote(theta[.(index[1])]), 
         y = bquote(theta[.(index[2])])) +
    coord_fixed() +
    theme_minimal(base_size = 16) +
    theme(
      panel.grid = element_blank(),
      axis.title.x = element_text(margin = margin(t = 8)),
      legend.title = element_text(
        margin = margin(b = 16),   # Space below the title
        size = 14
      ),
      legend.text  = element_text(size = 12)
    )
}
