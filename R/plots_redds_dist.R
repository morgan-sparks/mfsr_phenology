library(tidyverse); library(ggridges)

mfsr_spawn_cleaned <- read_csv(here::here("data", "russ_spawn","mfsr_spawn_cleaned.csv"))

mfsr_by_site <- mfsr_spawn_cleaned |> 
  filter(stream != "Knapp" & stream != "Cape Horn" & year != 2001) |> 
  group_by(stream, yday) 

mfsr_all <- mfsr_spawn_cleaned |> 
  filter(stream != "Knapp" & stream != "Cape Horn" & year != 2001) |> 
  summarise(median = median(yday),
            percentile_95 = quantile(yday,probs = 0.95),
            percentile_5 = quantile(yday, probs = 0.05))


plot <-  mfsr_spawn_cleaned |> 
  filter(stream != "Knapp" & stream != "Cape Horn" & year != 2001) |> 
  mutate(across(year, as.character)) |> 
  group_by(stream, year) |> 
  ggplot() +
  geom_vline(xintercept = mfsr_all$median, color = "blue", linetype = "dashed") +
  geom_vline(xintercept = c(mfsr_all$percentile_95, mfsr_all$percentile_5), color = "purple", linetype = "dashed") +
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
