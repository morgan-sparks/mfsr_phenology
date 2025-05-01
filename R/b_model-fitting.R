# Use LMMs and HBMs to assess associations between spawn time and covariates

# Prep ===============================
library(tidyverse)
library(here)
library(brms)
library(lme4)
library(mgcv)
library(loo)
library(gratia)

# load rds
model_data_final <- readRDS(here("data", "model_data_final.rds"))
model_data_final

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# standardize continuous covariates
model_data_final <- model_data_final |>
  mutate(across(c("temp_90", "flow_90", "mean_elevation", "SLOPE"), scale2))



# --- FREQUENTIST MODEL FITTING ------------------------------------------------

# M1: basline model; simple additive fixed linear effects
# m10 <- lm(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + year + stream, data = model_data_final)
m11 <- lmer(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + year + stream + (1 | COMID), data = model_data_final)
# m12 <- lmer(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + year + stream + (1 + temp_90 | COMID), data = model_data_final)
# m13 <- lmer(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + year + stream + (1 | stream/COMID), data = model_data_final)
# m14 <- lmer(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + year + stream + (1 + temp_90 | stream/COMID), data = model_data_final)

AIC(m10, m11, m12, m13, m14) |> arrange(AIC)

# M3: Add fixed interaction (temp_90 * stream)
m3 <- lm(yday ~ temp_90 * stream + flow_90 + mean_elevation + SLOPE + year, data = model_data_final)
m3a <- lm(yday ~ temp_90 * stream + flow_90 * year + mean_elevation + SLOPE, data = model_data_final)

# M4: Add random intercepts for COMID nested within stream
m4 <- lmer(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + year + stream + (1 | stream/COMID), data = model_data_final)
m4a <- lmer(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + year + stream + (1 | COMID), data = model_data_final)

# M5: Add random intercepts + random SLOPEs for temp_90
m5 <- lmer(yday ~ temp_90 * stream + flow_90 + mean_elevation + SLOPE + year + (1 + temp_90 | stream/COMID), data = model_data_final)
m5a <- lmer(yday ~ temp_90 * stream + flow_90 + mean_elevation + SLOPE + year + (1 + temp_90 | COMID), data = model_data_final)

# M6: GAM — smooths for Temp and flow_90
m6 <- gam(yday ~ s(temp_90) + s(flow_90) + mean_elevation + SLOPE + year + stream, data = model_data_final, method = "REML")
m6a <- gam(yday ~ s(temp_90, by = stream) + s(flow_90) + mean_elevation + SLOPE + year + stream, data = model_data_final, method = "REML")
m6b <- gam(yday ~ s(temp_90, by = stream) + s(flow_90, by = stream) + mean_elevation + SLOPE + year + stream, data = model_data_final, method = "REML")
m6c <- gam(yday ~ s(temp_90, by = stream) + s(flow_90, by = year) + mean_elevation + SLOPE + year + stream, data = model_data_final, method = "REML")
m6d <- gam(yday ~ s(temp_90, by = stream, k = 5) + s(flow_90, by = year, k = 5) + mean_elevation + SLOPE + year + stream, data = model_data_final, method = "REML")

summary(m6c)
draw(m6d)
# --- AIC Comparison (Frequentist models) --------------------------------------

# Compare AIC
AIC(m11, m3, m3a, m4, m4a, m5, m5a, m6, m6a, m6b, m6c, m6d) |> arrange(AIC)

sjPlot::tab_model(m4_lmer, m4a_lmer)
sjPlot::tab_model(m5_lmer, m5a_lmer)
sjPlot::tab_model(m3a, m6a, m6b, m6c)

# Looks like the nested random effects are overfitting, also lower marginal r2
# Makes sense, that is a lot of random terms, every stream and comid within a stream
# AIC penalized complexity, so if nesting isnt needed, simple wins
# Simple renefs allows model to focus on exaplaing actual variance instead of trying to partiion tinu diffs across stream:comid combos
# Plus we include stream as a fixed effect, so that variation is accounted for. 

# --- BAYESIAN MODEL FITTING  --------------------------------------------------

# Controls --------------

n_cores = 4
n_chains = 4
n_thin = 1
adapt_d = .95
# tree_depth = 20
# n_iter = 500  # testing
n_iter = 4000  # full


# Model Structures ---------------

# M1: null model
# M2: add fixed effects (linear)
# M3: add fixed interactions (temp x stream)
# M4: M2 + random intercept (stream)
# M5: M3 + (temp x stream)
# M6: GAM with smooths for temp and flow_90, fixed stream
# M7: M6 + random intercept (stream)
# M8: smooth effect of Temp by stream

brm_formulas <- list(
  bf(yday ~ 1),
  bf(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + stream + year),
  bf(yday ~ temp_90 * stream + flow_90 + mean_elevation + SLOPE + year),
  bf(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + stream + year + (1 | stream/COMID)),
  bf(yday ~ temp_90 * stream + flow_90 + mean_elevation + SLOPE + year + (1 + temp_90 | stream/COMID)),
  bf(yday ~ s(temp_90) + s(flow_90) + mean_elevation + SLOPE + stream + year),
  bf(yday ~ s(temp_90) + s(flow_90) + mean_elevation + SLOPE + stream + year + (1 | stream/COMID)),
  bf(yday ~ s(temp_90, by = stream) + s(flow_90) + mean_elevation + SLOPE + stream + year + (1 | COMID))
)
brm_formulas <- list(
  bf(yday ~ 1),
  bf(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + stream + year),
  bf(yday ~ temp_90 * stream + flow_90 + mean_elevation + SLOPE + year), 
  bf(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + stream + year + (1 | COMID)),
  bf(yday ~ temp_90 * stream + flow_90 + mean_elevation + SLOPE + year + (1 + temp_90 | COMID)),
  bf(yday ~ s(temp_90) + s(flow_90) + mean_elevation + SLOPE + stream + year),
  bf(yday ~ s(temp_90) + s(flow_90) + mean_elevation + SLOPE + stream + year + (1 | COMID)),
  bf(yday ~ s(temp_90, by = stream) + s(flow_90) + mean_elevation + SLOPE + stream + year + (1 | COMID))
)
names(brm_formulas) <- c("M1","M2","M3","M4","M5","M6","M7","M8")

brm_formulas <- list(
  bf(yday ~ 1),
  # bf(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + stream + year),
  bf(yday ~ temp_90 + flow_90 * year + mean_elevation + SLOPE + stream),
  # bf(yday ~ temp_90 * stream + flow_90 + mean_elevation + SLOPE + year), 
  bf(yday ~ temp_90 * stream + flow_90 * year + mean_elevation + SLOPE),
  # bf(yday ~ temp_90 + flow_90 + mean_elevation + SLOPE + stream + year + (1 | COMID)),
  # bf(yday ~ temp_90 * stream + flow_90 + mean_elevation + SLOPE + year + (1 + temp_90 | COMID)),
  # bf(yday ~ s(temp_90) + s(flow_90) + mean_elevation + SLOPE + stream + year),
  # bf(yday ~ s(temp_90) + s(flow_90) + mean_elevation + SLOPE + stream + year + (1 | COMID)),
  # bf(yday ~ s(temp_90, by = stream) + s(flow_90) + mean_elevation + SLOPE + stream + year + (1 | COMID))
)


# Fit models -----------------

# Map brm
brm_mods_2 <-
  map(
    brm_formulas,
    ~ brm(
      .,
      model_data_final,
      seed = 12345,
      cores = n_cores,
      chains = n_chains,
      iter = n_iter,
      thin = n_thin,
      control = list(adapt_delta = adapt_d)
      )
    )

# Save models
saveRDS(brm_mods_2, file = here("models", "brms", "brm_mods_simple-renefs.rds"))

brm_mods

# Loo comparison

# Compare LOOIC
l1 <- loo(brm_mods[[1]])
l2 <- loo(brm_mods[[2]])
l3 <- loo(brm_mods[[3]])
l4 <- loo(brm_mods[[4]])
l5 <- loo(brm_mods[[5]])
l6 <- loo(brm_mods[[6]])
l7 <- loo(brm_mods[[7]])
l8 <- loo(brm_mods[[8]])


# Compare
loo_compare(l1, l2, l3, l4, l5, l6, l7, l8)

# Run M8 with more iterations
brm_mods_8 <- brm(
  brm_formulas[[8]],
  model_data_final,
  seed = 12345, 
  cores = n_cores,
  chains = n_chains,
  iter = n_iter,
  thin = n_thin,
  control = list(adapt_delta = adapt_d)
)


# Compare AIC
model_aic <- loo_compare(l1, l2, l3, l4, l5, l6, l7, l8)

# Calculate delta AIC and model weights
model_aic <- model_aic %>%
  as.data.frame() %>%
  mutate(delta_looic = looic - min(looic),
         weight = exp(-0.5 * delta_looic) / sum(exp(-0.5 * delta_looic)),
         Model = rownames(.))

ggplot(model_aic, aes(x = reorder(Model, -weight), y = weight)) +
  geom_col(fill = "steelblue") +
  labs(x = "Model", y = "Akaike Weight", title = "Relative Support for Models (AIC)") +
  theme_minimal(base_size = 14)

