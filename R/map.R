library(tidyverse)
library(sf)
library(ggspatial)
library(cowplot)

# Data =========================================================================

# Redd data
dat <- read_csv(here::here("data", "russ_spawn", "mfsr_spawn_cleaned.csv"))

df <- dat |> select(year, stream, UNIQUE_ID, EASTING, NORTHING)

# Conversion of data frame to sf object
df_sf <- st_as_sf(
  x = df,
  coords = c("EASTING", "NORTHING"),
  crs = "+proj=utm +zone=11"
)

# Projection transformation
sfc <- st_transform(df_sf, crs = 5070)

# Pull out the coordinates and add them to the data frame, save it
sfc |>
  mutate(
    lon = sf::st_coordinates(geometry)[, 1],
    lat = sf::st_coordinates(geometry)[, 2]
  ) |>
  write_csv("data/df_sf.csv")

# Stream lines and watershed ===================================================
lines <- st_read(
  dsn = paste0(here::here(), "/data/gis/flowline_pn17_nsi_MFSR.gpkg"),
  layer = "flowline_pn17_nsi_MFSR"
)

watershed <- st_read(
  dsn = paste0(here::here(), "/data/gis/HU8_MFSR_dissolved.gpkg"),
  layer = "HU8_MFSR_dissolved"
)

states <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) 

# Plot =========================================================================

# Main panel
p1 <-
  ggplot() +
  geom_sf(data = watershed, fill = NA, color = "black", linewidth = .5) +
  geom_sf(data = lines, color = "dodgerblue", linewidth = 0.3) +
  geom_sf(
    data = lines |> filter(GNIS_NAME %in% c(
      "Middle Fork Salmon River",
      "Bear Valley Creek",
      "Beaver Creek",
      "Big Creek",
      "Camas Creek",
      "Cape Horn Creek",
      "Elk Creek",
      "Knapp Creek",
      "Loon Creek",
      "Marsh Creek",
      "Sulphur Creek"
    )),
    color = "blue", 
    linewidth = 0.5
  ) +
  geom_sf(
    data = sfc |> filter(UNIQUE_ID != 5065),
    aes(fill = stream),
    color = "black", shape = 21, size = 1.5, alpha = 0.7
  ) +
  ggspatial::annotation_scale(
    location = "br",
    # pad_x = unit(0, "in"), pad_y = unit(0.3, "in"),
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "br", which_north = "true",
    pad_x = unit(0, "in"), 
    pad_y = unit(0.3, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  ) + 
  theme_classic(base_size = 12) +
  labs(
    fill = "Stream"
  )
p1

# Inset panel
p2 <- 
  ggplot() + 
  geom_sf(
    data = states |> filter(ID %in% c(
      "idaho", "oregon", "nevada", "utah", "washington", "montana", "wyoming",
      "california", "colorado", "arizona", "new mexico"
    )), 
    fill = "white", color = "black", size = 0.25) + 
  geom_sf(
    data = states |> filter(ID == "idaho"), 
    fill = "grey80",
    color = "black",
    size = 0.25) + 
  geom_sf(data = watershed, fill = "black", color = "black") +
  theme_void() + 
  theme(
    plot.background = element_rect(color = NA, fill = "white"),
  )
p2


p.full.map <- 
  cowplot::ggdraw() +
  cowplot::draw_plot(p1) +
  cowplot::draw_plot(p2, x = .07, y = .76, width = 0.23, height = 0.18)
p.full.map


# Save plot ====================================================================

path <- here::here("plots", "map")

ggsave(
  glue::glue("{path}.png"), 
  p.full.map, 
  device = ragg::agg_png, 
  width = 6,
  height = 5,
  res = 300,  
  units = "in"
  ) 

ggsave(
  glue::glue("{path}.pdf"), 
  p.full.map, 
  device = cairo_pdf, 
  width = 6,
  height = 5,
  units = "in"
) 
