library(tidyverse); library(ggridges)

mfsr_spawn_cleaned <- read_csv(here::here("data", "russ_spawn","mfsr_spawn_cleaned.csv"))

mfsr_by_site <- mfsr_spawn_cleaned |> 
  filter(stream != "Knapp" & stream != "Cape Horn") |> 
  group_by(stream, yday) 

mfsr_all <- mfsr_spawn_cleaned |> 
  filter(stream != "Knapp" & stream != "Cape Horn") |> 
  group_by(yday) 


plot <- mfsr_spawn_cleaned |> 
  filter(stream != "Knapp" & stream != "Cape Horn" & year != 2001) |> 
  mutate(across(year, as.character)) |> 
  group_by(stream, year) |> 
  ggplot() +
  geom_density(aes(x = yday, y = after_stat(count), fill = year),  alpha = 0.5) +
  geom_density(data = mfsr_by_site, aes(x = yday, after_stat(count)), alpha = 0, linetype = "dashed") +
  facet_wrap(~stream, scales = "free_y", ncol = 1) +
  scale_fill_brewer(palette = "Dark2", name = "Year") +
  labs(x = "Spawn Day of Year", y = "Count") +
  theme_classic() 

ggsave(here::here("plots", "redd_spawn_distributions.pdf"),
                  plot, 
                  height = 8,
                  width = 3,
                  units = "in",
                  dpi = 300)

mfsr_spawn_cleaned |>  filter(stream == "Knapp")  |> 
  ggplot(aes(x = yday, y = y)) +
aes(y = after_stat(count))+
  theme_classic()
