# Plot comparing isaak empirical to seigel modeled temp data

isaak_siegel <- readRDS(here("data", "processed", "siegel_temperature", "Isaak_Siegel_Compare.rds"))
isaak_siegel <- isaak_siegel |> drop_na(DailyMean, Stream_Temp)

isaak_siegel |>
  ggplot(aes(x = DailyMean, y = Stream_Temp)) +
  geom_point(size = 0.001, alpha = 0.2) +
  geom_abline(intercept = 0, slope = 1, colour = "red") +
  labs(x = "Empirical Temperature", y = "Predicted Temperature") +
  lims(x = c(-2, 27), y = c(-2, 27)) +
  annotate("text", x = 10, y = 20,
           label = paste0("r = ", round(cor(isaak_siegel$DailyMean, isaak_siegel$Stream_Temp), 2)),
           size = 4)

path <- here::here("plots", "Figure_isaak-siegel-compare")
ggsave(
  glue::glue("{path}.pdf"),
  plot = last_plot(),
  device = cairo_pdf,
  scale = 2,
  width = 6,
  height = 6,
  units = "cm"
)
pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  format = "png",
  dpi = 300
)
