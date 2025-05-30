library(dplyr)
library(performance)
library(broom.mixed)


plot_covariate <- function(data, covariate) {
  ggplot(data, aes_string(x = covariate, y = "yday")) +
    geom_point() +
    geom_smooth(method = "lm") +
    labs(x = covariate, y = "Day of year") +
    theme_bw()
}

scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

summary_table <- function(model) {
  tidy(model)[, c("term", "estimate", "std.error", "p.value")]
}

compare_models_mixed <- function(...) {
  models <- list(...)
  model_names <- as.character(match.call())[-1]
  
  # Build AIC and logLik table
  aic_tbl <- AIC(...) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Model") %>%
    mutate(Model = model_names,
           delta = AIC - min(AIC),
           logLik = sapply(models, logLik),
           r2_marginal = sapply(models, function(m) r2(m)$R2_marginal),
           r2_conditional = sapply(models, function(m) r2(m)$R2_conditional)) %>%
    arrange(AIC) %>%
    select(Model, df, logLik, AIC, delta, r2_marginal, r2_conditional) %>%
    as_tibble()
  
  return(aic_tbl)
}


compare_aic <- function(...) {
  models <- list(...)
  model_names <- as.character(match.call())[-1]
  
  aic_tbl <- AIC(...) %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "Model") %>%
    mutate(Model = model_names,
           delta = AIC - min(AIC)) %>%
    arrange(AIC) %>%
    as_tibble()
  
  return(aic_tbl)
}
