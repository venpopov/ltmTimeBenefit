theme_pub <- function(base_size = 13, base_family = "") {
  theme_bw(base_size, base_family) +
    theme(
      panel.background = element_rect(fill = "#ffffff", colour = NA),
      title = element_text(size = rel(1), face = "bold"),
      plot.subtitle = element_text(
        size = rel(0.8),
        family = "Noto Sans", face = "plain"
      ),
      panel.border = element_rect(color = "grey50", fill = NA, linewidth = 0.15),
      panel.spacing = unit(1, "lines"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linewidth = 0.25, colour = "grey90"),
      axis.line = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_text(face = "bold"),
      axis.title.x = element_text(vjust = 0, margin = margin(t = 10)),
      axis.title.y = element_text(vjust = 1, margin = margin(r = 10)),
      # legend.position = "bottom",
      # legend.title = element_text(
      #   size = rel(0.7), vjust = 0.5,
      #   family = "Noto Sans", face = "plain"
      # ),
      # legend.key.size = unit(0.7, "line"),
      # legend.key = element_blank(),
      # legend.spacing = unit(0.1, "lines"),
      # legend.justification = "left",
      # legend.margin = margin(t = -5, b = 0, l = 0, r = 0),
      strip.text = element_text(
        size = rel(0.9), hjust = 0, face = "bold"
      ),
      strip.background = element_rect(fill = "white", colour = NA)
    )
}

#' Plot Bootstrap Results
#'
#' This function plots the bootstrap results for a given dataset.
#'
#' @param data The dataset containing the bootstrap results.
#'
#' @return A ggplot object displaying the histogram of the bootstrap results for each parameter.
#'
#' @import ggplot2
#' @importFrom dplyr select everything
#' @importFrom tidyr pivot_longer unnest
#'
#' @examples
#' tar_load(exp1_data)
#' data <- replicate(10, boot_est(exp1_data), simplify = FALSE)
#' data <- do.call(rbind, data)
#' plot_bootstrap_results(data)
#'
#' @export
plot_bootstrap_results <- function(data) {
  as.data.frame(data) |>
    select(-convergence) |>
    pivot_longer(cols = everything(), names_to = "param", values_to = "value") |>
    unnest(value) |>
    ggplot(aes(x = value)) +
    geom_histogram(bins = 30) +
    facet_wrap(~param, scales = "free")
}

#' Plot Linear RV Recovery
#'
#' This function plots the recovery of a linear random variable (RV) over time.
#'
#' @param r A vector representing the recovery rates sampled from some distribution.
#' @param t A vector representing the time values.
#' @param title The title of the plot (default is "Uniform distribution").
#'
#' @return A ggplot object representing the plot of linear RV recovery over time.
#'
#' @details Average recovery is given as a red line, while individual trajectories are given as black lines.
#'
#' @examples
#' r <- runif(1000, 0, 1)
#' t <- seq(0, 5, by = 0.01)
#' plot_linear_rv_recovery(r, t, title = "Uniform distribution")
#'
#' @import ggplot2
plot_linear_rv_recovery <- function(r, t, title = "Uniform distribution") {
  recovery <- outer(r, t, function(r, t) pmin(r * t, 1))
  recovery <- apply(recovery, 2, mean)
  data <- data.frame(t = t, recovery = recovery)

  g <- ggplot(data, aes(x = t, y = recovery)) +
    geom_line(linewidth = 1, color = "red") +
    labs(title = title)

  # add black lines for each individual trajectory
  for (i in 1:20) {
    recovery <- pmin(r[i] * t, 1)
    data <- data.frame(t = t, recovery = recovery, distribution = "Uniform")
    g <- g + geom_line(data = data, aes(x = t, y = recovery), color = "black", alpha = 0.1)
  }

  g + theme_test()
}
