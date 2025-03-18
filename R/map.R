library(tidyverse); library(sf); library(ggspatial)

df <- dat |> select(year, stream, UNIQUE_ID, EASTING, NORTHING)

#Conversion of data frame to sf object
df_sf <- st_as_sf(x = df,                         
                  coords = c("EASTING", "NORTHING"),
                  crs = "+proj=utm +zone=11")

#Projection transformation
sfc = st_transform(df_sf, crs = 5070)

sfc |> mutate(lon = sf::st_coordinates(geometry)[,1],
              lat = sf::st_coordinates(geometry)[,2]) |> 
  write_csv("data/df_sf.csv")

#Convert it to data frame
sfc_df <- as_data_frame(sfc)


lines <- st_read(
  dsn = paste0(here::here(), "/data/gis/flowline_pn17_nsi_MFSR.gpkg"), 
  layer='flowline_pn17_nsi_MFSR'
  )

watershed <- st_read(
  dsn = paste0(here::here(), "/data/gis/HU8_MFSR_dissolved.gpkg"), 
  layer='HU8_MFSR_dissolved'
)

pl <- ggplot() +
  geom_sf(data = lines, color = "dodgerblue") + 
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
    color = "blue") + 
  geom_sf(data = watershed, fill = "grey85", alpha = 0.5) + 
  geom_sf(data = sfc |> filter(UNIQUE_ID!=5065), aes(fill = stream), color = "black", shape = 21, size = 2, alpha = 0.7) +
  theme_classic() + 
  labs(
    fill = "Stream"
  )


pl_full <- pl +
  ggspatial::annotation_scale(
    location = "tl",
    bar_cols = c("grey60", "white"),
    text_family = "ArcherPro Book"
  ) +
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey40", "white"),
      line_col = "grey20",
      text_family = "ArcherPro Book"
    )
  )

# save plot
path <- here::here("plots","map")
ggsave(
  glue::glue("{path}.pdf"), 
  plot = pl_full, 
  width = 6, 
  height = 5, 
  device = cairo_pdf
)

# convert
pdftools::pdf_convert(
  pdf = glue::glue("{path}.pdf"),
  filenames = glue::glue("{path}.png"),
  format = "png", 
  dpi = 600
)
