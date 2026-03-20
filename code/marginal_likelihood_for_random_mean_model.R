## Load CSV and compute log density (up to an additive constant) for sigma_grp

subset_data <- FALSE
path_to_data <- file.path("..", "data", "sat_verbal_scores_from_8_school.csv")
sat_data <- read.csv(path_to_data, stringsAsFactors = FALSE)
if (subset_data) {
  subset_size <- 3
  sat_data <- sat_data[1:subset_size, ]
}

y  <- sat_data$trt_effect
se <- sat_data$std_err

log_marginal_lik_sigma_grp <- function(sigma_grp, y, se, mu = 0) {
  if (any(sigma_grp < 0)) return(-Inf)  # enforce sigma_grp > 0
  total_var <- se^2 + sigma_grp^2
  sum(-0.5 * log(total_var) - 0.5 * (y - mu)^2 / total_var)
}

# Evaluate log density on a grid
grid_max <- ifelse(subset_data, 120, 50)
grid <- seq(0, grid_max, length.out = 501)
log_density <- sapply(grid, log_marginal_lik_sigma_grp, y = y, se = se)

log_density_shifted <- log_density - max(log_density) # to avoid overflow
density <- exp(log_density_shifted)
density <- density / (sum(density) * diff(grid)[1])  # normalize by grid spacing

# Plot
save_to_pdf <- FALSE
x_max <- ifelse(subset_data, 100, 30)
range_indx <- match(TRUE, grid > x_max)

if (save_to_pdf) { 
  filename <- sprintf(
    "group_var_posterior_with_%d_schools.pdf", nrow(sat_data)
  )
  filepath <- file.path("..", "lecture", "Figure", filename)
  pdf(filepath, width = 6.5, height = 4.5) 
  par(mar = c(4.1, 5.1, 2.1, 2.1)) # bottom, left, top, right
}

title_txt <- sprintf("Posterior with %d schools", nrow(sat_data))
plot(
  grid[1:range_indx], 
  density[1:range_indx], 
  type = "l", 
  lwd = 1.2,
  col = "#002D72",
  xlab = expression(sigma[grp]), 
  ylab = expression(pi(sigma[grp] ~ "|" ~ bold(y))),
  ylim = c(0, 1.05 * max(density)),
  xaxs = "i", yaxs = "i",
  main = title_txt,
  font.main = 1,
  frame = FALSE,
  cex.main = 1.4,
  cex.lab = 1.5,   # axis labels (xlab, ylab)
  cex.axis = 1.3   # tick labels (numbers on the axes)
)

if (save_to_pdf) { dev.off() }

