library(tidyverse); library(ggridges); library(patchwork)


mfsr_spawn_cleaned <- readRDS("./data/model_data_final.rds")


# Good plots --------------------------------------------------------------

#stream colors
cols.streams <- c(
  "Bear Valley" = "#55FF00",
  "Beaver" = "#7A8EF5",
  "Big" = "#FFFF00",
  "Camus" = "#E64C00",
  "Elk" = "#D7D79E",
  "Loon" = "#7AF5CA",
  "Marsh" = "#DF73FF",
  "Sulphur" = "#FFAA00"
)

# redd count by elevation and stream for COMID
p1 <- mfsr_spawn_cleaned |> 
  group_by(stream, year, COMID, mean_elevation) |>
  summarise(redd_count = n()) |> 
  ggplot(aes(x = mean_elevation, y = stream, size = redd_count, fill = stream) ) +
  geom_point(alpha = 0.5, shape = 21) +
  lemon::facet_rep_wrap(~year, ncol = 1) +
  scale_fill_manual(values = cols.streams) +
  labs(y = "Stream",
       x = "Elevation (m)") +
  theme_classic() +
  theme(legend.position = "none")

# spawning distribution by COMID
p2 <- mfsr_spawn_cleaned |> 
  ggplot(aes(x= yday, y = stream, fill = stream, group = COMID, height = after_stat(density))) +
  geom_density_ridges( stat = "density", scale =3, rel_min_height = .01, alpha = 0.65) +
  lemon::facet_rep_wrap(~year, ncol=1) +
  scale_fill_manual(values = cols.streams) +
  lemon::facet_rep_wrap(~year, ncol = 1) +
  labs(x = "Spawn Day of Year",
       y = "Stream") +
  theme_classic() +
  theme(legend.position = "none")

# plot two together
p1 + p3+ p2

p2 + p1 + p3

p3 <- mfsr_spawn_cleaned |> 
  group_by(stream, year, COMID, mean_elevation) |> 
  summarise(redd_count = n(), mean_temp_90 = mean(temp_90)) |> 
  ggplot(aes(y = mean_elevation,x = mean_temp_90, fill= stream, group = COMID, size = redd_count)) +
  geom_point(alpha = 0.5, shape = 21) +
  scale_fill_manual(values = cols.streams) +
  lemon::facet_rep_wrap(~year, ncol = 1) +
  theme_classic() +
  labs(x = "Average 90-day Preceding Temperature (°C)",
       y = "Elevation (m)") +
  theme(legend.position = "none")

# Bad plots ---------------------------------------------------------------

#sd of eleveation vs temperature
mfsr_spawn_cleaned |> 
  group_by(stream, year ) |>
  summarise(sd_elevation = sd(mean_elevation),sd_temp_90 = sd(temp_90), sd_yday = sd(yday)) |> 
  ggplot(aes(x = sd_elevation, sd_temp_90, color = stream)) +
  geom_point() +
  theme_classic()

#sd of temperature vs. spawn yday
mfsr_spawn_cleaned |> 
  group_by(stream, year ) |>
  summarise(sd_elevation = sd(mean_elevation),sd_temp_90 = sd(temp_90), sd_yday = sd(yday)) |> 
  ggplot(aes(x = sd_temp_90, sd_yday, color = stream)) +
  geom_point() +
  theme_classic()

mfsr_spawn_cleaned |>
  ggplot(aes(x =yday, y = as.factor(year), color = COMID, fill = COMID, height = after_stat(density)))+
  geom_density_ridges( stat = "density", scale = .95, rel_min_height = .01, alpha = 0.75,
                       point_shape = '|', point_size = 3, point_alpha = 1) +
  theme_classic() +
  facet_wrap(~stream, ncol = 1) +
  theme(legend.position = "none")

mfsr_spawn_cleaned |> 
  mutate(COMID = as.factor(COMID)) |> 
  ggplot(aes(x = yday, y = stream, fill = as.factor(year), group = COMID))+
  geom_boxplot() +
  theme_classic() +
  theme(legend.position = "none")


mfsr_spawn_cleaned |>
  mutate(COMID = as.factor(COMID),
         year = as.factor(year)) |> 
  filter(stream == "Bear Valley") |> 
  ggplot(aes(x =yday, y = COMID, color = stream, fill = stream, height = after_stat(density)))+
  geom_density_ridges( stat = "density", scale = .95, rel_min_height = .01, alpha = 0.85) +
  facet_grid(stream~year, scales = "free_y" ) +
  theme_classic() +
  theme(legend.position = "none")


mfsr_spawn_cleaned |> 
  #filter(stream == "Big") |> 
  ggplot(aes(x = yday, y = mean_elevation, fill = stream, group = COMID,height = after_stat(density))) +
  geom_density_ridges( stat = "density", scale =15, rel_min_height = .01, alpha = 0.65) +
  #facet_wrap(~year, ncol=1) +
  facet_grid(year~stream ) +
  theme_classic() 


single_day_comids <- mfsr_spawn_cleaned |> 
  #filter(stream == "Marsh" & year == 2004) |> 
  group_by(year, COMID) |> 
  summarise(yday_count = length(unique(yday))) |> 
  filter(yday_count <= 1) 

'%!in%' <- function(x,y)!('%in%'(x,y))

mfsr_spawn_cleaned |>
  filter(stream %in% c("Big", "Camas", "Loon", "Sulphur", "Bear Valley") & COMID %!in% c(single_day_comids$COMID)) |> 
  ggplot(aes(x = yday, y = mean_elevation, fill = stream, group = COMID,height = after_stat(density))) +
  geom_density_ridges( stat = "density", scale =15, rel_min_height = .01, alpha = 0.65) +
  lemon::facet_rep_wrap(~year, ncol=1) +
  #lemon::facet_rep_grid(year~stream ) +
  theme_classic() 

mfsr_spawn_cleaned |> 
  
  ggplot(aes(x = yday, y = mean_elevation, fill = stream, group = COMID)) +
  geom_boxplot() +
  #lemon::facet_rep_wrap(~year, ncol=1) +
  lemon::facet_rep_grid(year~stream ) +
  theme_classic()

cols.streams <- c(
  "Bear Valley" = "#55FF00",
  "Beaver" = "#7A8EF5",
  "Big" = "#FFFF00",
  "Camus" = "#E64C00",
  "Elk" = "#D7D79E",
  "Loon" = "#7AF5CA",
  "Marsh" = "#DF73FF",
  "Sulphur" = "#FFAA00"
)

p1 <- mfsr_spawn_cleaned |> 
  group_by(stream, year, COMID, mean_elevation) |>
  summarise(redd_count = n()) |> 
  ggplot(aes(x = mean_elevation, y = stream, size = redd_count, fill = stream) ) +
  geom_point(alpha = 0.5, shape = 21) +
  lemon::facet_rep_wrap(~year, ncol = 1) +
  scale_fill_manual(values = cols.streams) +
  theme_classic() +
  theme(legend.position = "none")

p2 <- mfsr_spawn_cleaned |> 
  ggplot(aes(x= yday, y = stream, fill = stream, group = COMID, height = after_stat(density))) +
  geom_density_ridges( stat = "density", scale =3, rel_min_height = .01, alpha = 0.65) +
  lemon::facet_rep_wrap(~year, ncol=1) +
  scale_fill_manual(values = cols.streams) +
  theme_classic() +
  theme(legend.position = "none")

p1 + p2


