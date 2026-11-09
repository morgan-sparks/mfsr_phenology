# palettes
cols.streams <- c(
  "Bear Valley" = "#55FF00",
  "Beaver" = "#7A8EF5",
  "Big" = "#FFFF00",
  "Camas" = "#E64C00",
  "Elk" = "#D7D79E",
  "Loon" = "#7AF5CA",
  "Marsh" = "#DF73FF",
  "Sulphur" = "#FFAA00"
)

cols.years <- c(
  "2002" = "#1B9E77", 
  "2003" = "#D95F02", 
  "2004" = "#7570B3", 
  "2005" = "#E7298A", 
  "2001" = "#66A61E"
)

# fixed date for better date axes
fixed_date <- as.Date("2000-12-31")

# ggplot theme used for all manuscript and appendix figures (moved here from
# analysis/_shared.R, now archived): white panel, axis lines only, no grid.
theme_mfsr <- function(base_size = 11, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      panel.background  = element_rect(fill = "white", color = NA),
      panel.border      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      axis.line         = element_line(color = "black", linewidth = 0.5),
      axis.ticks        = element_line(color = "black", linewidth = 0.4),
      axis.text         = element_text(size = rel(0.85), color = "grey20"),
      axis.title        = element_text(size = rel(0.95), color = "grey10"),
      strip.background  = element_rect(fill = "white", color = "black", linewidth = 0.5),
      strip.text        = element_text(size = rel(0.9), margin = margin(3, 3, 3, 3)),
      legend.background = element_blank(),
      legend.key        = element_blank(),
      legend.title      = element_text(size = rel(0.85)),
      legend.text       = element_text(size = rel(0.8)),
      plot.margin       = margin(4, 6, 4, 6)
    )
}
