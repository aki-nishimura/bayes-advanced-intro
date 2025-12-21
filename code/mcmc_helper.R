library(ggplot2)

trace_plot <- function(samples, param_index = 1, thin = 1L, ylab="Parameter value") {
  if(is.null(dim(samples))) {
    samples <- matrix(samples, nrow = 1)
  }
  n_iter <- dim(samples)[2]
  sub_indices <- seq(1, n_iter, by = thin)
  y <- as.vector(samples[param_index, sub_indices])
  plot(
    sub_indices, y,
    xlab = "MCMC iteration",
    ylab = ylab,
    frame = FALSE, 
    col = "#002D72",
    cex = 1.2,
    lwd = 1.2,
    cex.lab = 1.2, 
    cex.main = 1.3
  )
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

hist_against_truth <- function(coord_samples, true_density) {
  x <- seq(
    min(coord_samples), max(coord_samples), length.out=10000
  )
  y <- true_density(x)
  
  hist(
    coord_samples, 
    breaks = 51, freq = FALSE,
    col = "#bdcedb", border = "#859bad",
    ylim = c(0, 1.1 * max(y)),
    xlab="Parameter value",
    main = NULL
  )
  lines(x, y, lty=1)
}

plot_trajectory <- function(samples, index, n_iter_to_plot, target_density) {
  
  bivar_samples <- data.frame(
    x = samples[index[1], 1:n_iter_to_plot],
    y = samples[index[2], 1:n_iter_to_plot],
    iter = 1:n_iter_to_plot
  )
  
  x_grid <- seq(-4, 4, length = 251)
  y_grid <- seq(-4, 4, length = 251)
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
