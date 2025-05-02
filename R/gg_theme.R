# create a custom ggplot2 theme for the paper

library(ggplot2)


# Define the custom theme
theme_custom <- function() {
  theme_minimal(base_size = 14) +
    theme(
      axis.title = element_text(face = "bold"),
      axis.text = element_text(color = "black"),
      axis.ticks = element_line(color = "black", linewidth = 0.3),
      axis.line.x = element_line(color = "black", linewidth = 0.8),
      axis.line.y = element_line(color = "black", linewidth = 0.8),
      # panel.grid.major = element_line(color = "gray80", linewidth = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(face = "bold"),
      # plot.subtitle = element_text(),
      legend.position = "none",
      plot.margin = margin(10, 10, 10, 10), 
      strip.text = element_text(hjust = 0),
    )
}

# p + theme_custom()
# 
# # Generate example data
# # connectance <- rnorm(200, mean = 0.126, sd = 0.021)
# # connectance <- pmin(pmax(connectance, 0.05), 0.20)  # clip to range
# 
# # Create the plot
# p <-
#   ggplot(data.frame(connectance), aes(x = connectance)) +
#   geom_histogram(aes(y = ..density..), bins = 40, 
#                  fill = "gray70", color = "black") +
#   geom_density(color = "navy", size = 1.2) +
#   geom_vline(aes(xintercept = mean(connectance)), 
#              color = "black", linetype = "dashed", size = 1.2) +
#   annotate("text", x = 0.175, y = 25, hjust = 0, vjust = 1, size = 4.5,
#            label = paste0("mean = ", round(mean(connectance), 3), "\n",
#                           "median = ", round(median(connectance), 3), "\n",
#                           "min = ", round(min(connectance), 3), "\n",
#                           "max = ", round(max(connectance), 3), "\n",
#                           "SD = ", round(sd(connectance), 3))) +
#   labs(x = "Connectance", y = "Density") 