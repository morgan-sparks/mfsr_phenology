library(tidyverse)
library(here)
library(patchwork)
library(lme4)
library(mgcv)
library(gratia)
source(here("R", "gg_theme.R"))

# Goal: describe variation in spawn timing (DOY) and how it related to covariates
# environmental conditions (flow, temp, slope, elevation)

# want to compare stream directly, so fixed effects
# COMID nested within streams (random effects)
# likely interaction between temp and stream (diff temp responses across streams)
# nonlinear covariates and linear predictors (need GAMMS)
#  Id nonlinearities are mild, could use LMMEs

# Response data ---------------

spawn_data <- read_csv(here("data", "russ_spawn", "mfsr_spawn_cleaned.csv"))

# remove bad data
spawn_data <- spawn_data |>
  filter(stream != "Knapp" & stream != "Cape Horn" & year != 2001) |> 
  mutate(year = as.factor(year), stream = as.factor(stream)) # make factor


# Covariates --------------------------------------------------------------

flow_data <- read_csv(here("data", "spawn_flows.csv"))
comid_data <- readRDS(here("data", "elevslope.rds"))
load(here("data", "comid_temps.RData"))

# Combine data ------------------------------------------------------------

# read in temperature data and reshape
temp_data <- out |>
  filter(period == "before" & duration %in% c(30, 60, 90)) |>
  pivot_wider(
    names_from = "duration", 
    values_from = avg_temp, 
    names_prefix = "temp_"
    )

# commbine flow and spawn in combined data
combined_data <- spawn_data |>
  left_join(flow_data, join_by(date == spawn_date))

# add temperature data to combined data
combined_data <- combined_data |>
  left_join(temp_data, join_by(UNIQUE_ID == redd_id))

# add physical data to combined data and then filter
combined_data <- combined_data |>
  left_join(comid_data, join_by(COMID)) |>
  mutate(
    mean_elevation = (MAXELEVSMO + MINELEVSMO) / 2 / 100,
    yday = yday(date)
  )

# make a df of just response and covariates
model_data <- combined_data |>
  filter(SLOPE < .2) |> # bad slope data
  select(
    yday, COMID, stream, year,
    flow_30, flow_60, flow_90,
    temp_30, temp_60, temp_90,
    SLOPE, mean_elevation
  )

# Explore data ------------------

# plot histogram and density of spawn dates (yday)
p1 <- ggplot(spawn_data, aes(x = yday)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "gray70", color = "black") +
  geom_density(color = "black", size = 1.2) +
  geom_vline(aes(xintercept = mean(yday)), color = "black", linetype = "dashed", size = 1.2) +
  annotate(
    "text", x = 260, y = .06, hjust = 1, vjust = 1, size = 4.5,
    label = paste0
    (
      "mean = ", round(mean(spawn_data$yday), 2), "\n",
      "median = ", round(median(spawn_data$yday), 2), "\n",
      "min = ", round(min(spawn_data$yday), 2), "\n",
      "max = ", round(max(spawn_data$yday), 2), "\n",
      "var = ", round(var(spawn_data$yday), 2), "\n",
      "SD = ", round(sd(spawn_data$yday), 2)
    )
  ) +
  labs(
    title = "Histogram and Density of Spawn Dates",
    x = "Spawn Date (DOY)",
    y = "Density"
  ) +
  theme_custom()
p1

# mean and mediam are equal, low SD; var < mean (no overdispersion)
# data are right-skewed, lumpy/multimodal, and not symmetric
# at least 2 peaks
# some fish spawn later, some earlier, multiple groups
# likely different pops or environemtal drivers
# posisson family is response is count of spawning events per day
# gausian if model density as continuous


# plot spawn date by stream
p2 <- ggplot(spawn_data, aes(x = stream, y = yday)) +
  geom_boxplot() +
  geom_jitter(aes(color = stream), size = 1.5, alpha = 0.5) +
  labs(
    title = "Spawn Date by Stream",
    x = "Stream",
    y = "Spawn Date (DOY)"
  ) +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

p3 <- ggplot(spawn_data, aes(x = year, y = yday)) +
  geom_boxplot() +
  geom_jitter(aes(color = year), size = 1.5, alpha = 0.5) +
  labs(
    title = "Spawn Date by Year",
    x = "Year",
    y = "Spawn Date (DOY)"
  ) +
  theme_custom() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# panel
p1 / (p2 + p3)


# make simple function to plot response (yday) vs. covariates
plot_covariate <- function(data, covariate) {
  ggplot(data, aes_string(x = covariate, y = "yday")) +
    geom_point() +
    # geom_smooth(method = "lm", se = FALSE) +
    geom_smooth() +
    labs(x = covariate, y = "DOY") +
    theme_custom()
}
# map function to each covariate and store plots in a list
covariates <- c("flow_30", "flow_60", "flow_90", "temp_30", "temp_60", "temp_90", "SLOPE", "mean_elevation")
plots <- map(covariates, ~ plot_covariate(model_data, .x))
gridExtra::grid.arrange(grobs = plots, ncol = 2)

# temp_30 = no relationship
# temp_60 = good non-linear
# temp_90 = better non-linear
# flow_30 and floqw_60 = similar decaying exponential
# flow_90 = inflections, interesting grouping really spreads out
# SLOPE = no relationship
# mean_elevation = slightly negative linear

ggplot(model_data, aes(x = temp_90, y = yday, color = stream)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_custom()
# different slopes and intercepts for temp (likely non-linear)

ggplot(model_data, aes(x = flow_90, y = yday, color = year)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()
# flow grouping are based on year
# year should be fixed effect: account for year differences directly

# could drop temp_30 and temp_60

# check for collinearity --------------------------------------

cor(model_data[, 4:11], use = "pairwise.complete.obs") |> as_tibble()

car::vif(lm(yday ~ flow_30 + flow_60 + flow_90 + temp_90 + SLOPE + mean_elevation, data = model_data))
# flow_30 and flow_60 are big issues, try without
car::vif(lm(yday ~ flow_90 + temp_90 + SLOPE + mean_elevation, data = model_data))
#okay

model_data |> select(temp_30, temp_60, temp_90, flow_30, flow_60, flow_90) |> GGally::ggpairs()



# scale data and final dataset---------------------------

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# standardize continuous covariates
model_data_final <- model_data |>
  select(yday, COMID, stream, year, temp_90, flow_90, SLOPE, mean_elevation ) |>
  mutate(across(c("temp_90", "flow_90", "mean_elevation", "SLOPE"), scale2))

# unscaled
model_data_final <- model_data |>
  select(yday, COMID, stream, year, temp_90, flow_90, SLOPE, mean_elevation)
# save
saveRDS(model_data_final, here("data", "model_data_final.rds"))

# Model structure --------------------------------

# DOY (spawn timing) = response
# temp_90 - smooth(potentially varying by stream)
# flow_90 - smooth (shared across data)
# elevation - linear
# slope - linear
# year - fixed effect (account for year differences directly)
# stream - fixed effect (compare streams directly)
# COMID - random intercept nested within streams


# Full GAMM (nonlinear effects capture)
gamm <- gam(
  yday ~ 
    s(temp_90, by = stream) +  # smooth temp by stream
    s(flow_90) + 
    SLOPE + 
    mean_elevation + 
    year + 
    stream + 
    s(COMID, bs = "re"),        # random intercept
  data = model_data_final,
  method = "REML",
  family = gaussian()
)

# Simpler LMM (all linear effects)
lmm <- lme4::lmer(
  yday ~
    temp_90 * stream +
    flow_90 +
    SLOPE +
    mean_elevation +
    year +
    (1 | COMID), # random intercept for COMID nested within streams
  data = model_data_final
)

AIC(gamm, lmm)

# par(mfrow = c(2, 2))
plot(gamm, page = 1)
qqnorm(residuals(gamm)); qqline(residuals(gamm))

plot(resid(lmm))
qqnorm(residuals(lmm)); qqline(residuals(lmm))

concurvity(gamm, full = TRUE)
draw(gamm)


# Bayes --------------------------------

library(brms)

brm1 <- brm(
  formula = yday ~ 
    temp_90 +                       # linear effect
    flow_90 +                       # linear effect
    mean_elevation +                # linear effect
    SLOPE +                         # linear effect
    stream +                        # fixed effect
    year +                          # fixed effect
    (1 | stream/COMID),             # random intercept for COMID nested in Stream
  data = model_data_final,
  family = gaussian(),              # continuous response
  cores = 4,                        # Parallel computation (adjust for your machine)
  chains = 4,
  iter = 500,
  # warmup = 1000,
  control = list(adapt_delta = 0.95)  # Important for complicated models to ensure convergence
)

# Check convergence
summary(brm1)
# Plot posterior predictive checks
pp_check(brm1)
# Plot marginal smooths
plot(brm1)
# Extract smooths separately if needed:
conditional_smooths(brm1)
# Plot random effects (COMID nested within Stream)
ranef(brm1)


brm2 <- brm(
  formula = yday ~ 
    s(temp_90, by = stream) +       # smooth effect of Temp by Stream
    s(flow_90) +                    # smooth effect of Flow
    mean_elevation +                # linear effect
    SLOPE +                         # linear effect
    stream +                        # fixed effect
    year +                          # fixed effect
    (1 | stream/COMID),             # random intercept for COMID nested in Stream
  data = model_data_final,
  family = gaussian(),              # continuous response
  cores = 4,                        # Parallel computation (adjust for your machine)
  chains = 4,
  iter = 500,
  # warmup = 1000,
  control = list(adapt_delta = 0.95)  # Important for complicated models to ensure convergence
)

library(loo)
loo(brm1, brm2)
loo1 <- loo(brm1)
loo2 <- loo(brm2)
loo_compare(loo1, loo2)

# Check convergence
summary(brm2)
# Plot posterior predictive checks
pp_check(brm2)
# Plot marginal smooths
# plot(brm2)
# Extract smooths separately if needed:
conditional_smooths(brm2, smooths = "s(temp_90, by = stream)")
# Plot random effects (COMID nested within Stream)
ranef(brm2)


# 1. Extract conditional smooths from the model
smooths_temp_stream <- conditional_smooths(brm2, smooths = "s(temp_90, by = stream)", conditions = NULL)

# `smooths_temp_stream` is a list — each entry corresponds to a Stream-specific smoother

# 2. Combine all smooths into a single dataframe for easy plotting
smooth_df <- bind_rows(
  lapply(names(smooths_temp_stream), function(stream_name) {
    data.frame(smooths_temp_stream[[stream_name]], stream = stream_name)
  })
)

# 3. Clean up Stream names (optional, depending how your Stream IDs are formatted)
smooth_df$stream <- gsub("temp_90:stream", "", smooth_df$stream)

# 4. Plot!

ggplot(smooth_df, aes(x = temp_90, y = estimate__)) +
  geom_line(aes(color = stream), size = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = stream), alpha = 0.2) +
  labs(x = "Temperature (°C)", y = "Effect on Spawn Timing (DOY)",
       title = "Stream-Specific Temperature Effects on Spawn Timing") +
  theme_minimal() +
  theme(legend.position = "right") +
  guides(fill = guide_legend(title = "Stream"), color = guide_legend(title = "Stream"))

ggplot(smooth_df, aes(x = temp_90, y = estimate__)) +
  geom_line(color = "black", size = 1.2) +
  geom_ribbon(aes(ymin = lower__, ymax = upper__), fill = "skyblue", alpha = 0.3) +
  facet_wrap(~ stream) +
  labs(x = "Temperature (°C)", y = "Effect on Spawn Timing (DOY)",
       title = "Stream-Specific Temperature Effects on Spawn Timing") +
  theme_minimal()


# 1. Pull smooths for Marsh and Big
smooth_A <- smooth_df |>
  filter(stream == "Marsh") |>
  select(temp_90, estimate__, lower__, upper__)
smooth_B <- smooth_df |>
  filter(stream == "Big") |>
  select(temp_90, estimate__, lower__, upper__)

# 2. Combine them by matching Temperature values
diff_df <- data.frame(
  Temperature = smooth_A$temp_90,
  Difference = smooth_A$estimate__ - smooth_B$estimate__,
  lower = smooth_A$lower__ - smooth_B$upper__,
  upper = smooth_A$upper__ - smooth_B$lower__
)

# 3. Plot the difference
ggplot(diff_df, aes(x = Temperature, y = Difference)) +
  geom_line(color = "darkred", size = 1.2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "pink", alpha = 0.3) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(x = "Temperature (°C)", y = "Difference in Effect (StreamA - StreamB)",
       title = "Difference in Temperature Effects Between StreamA and StreamB") +
  theme_minimal()