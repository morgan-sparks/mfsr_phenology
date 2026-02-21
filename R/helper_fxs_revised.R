# =============================================================================
# Helper functions — Middle Fork Chinook Phenology
# General utilities, model comparison, and publication tables
#
# Tables require the following objects in environment (from appendix.Rmd):
#   ML-fit:   m16, m26, m31, m16_rs, mod_spline3, mod_spline4, m201, m202
#   REML-fit: mod_final
# =============================================================================

library(dplyr)
library(lme4)
library(splines)
library(performance)
library(broom.mixed)
library(gt)
library(gtExtras)
library(kableExtra)

# =============================================================================
# General utilities
# =============================================================================

# Standardise a variable to mean = 0, SD = 1
scale2 <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) / sd(x, na.rm)

# Quick fixed-effects summary from a broom.mixed tidy
summary_table <- function(model) {
  tidy(model)[, c("term", "estimate", "std.error", "p.value")]
}

# Bivariate scatterplot of a covariate against spawn timing
plot_covariate <- function(data, covariate) {
  ggplot(data, aes(x = .data[[covariate]], y = yday)) +
    geom_point(alpha = 0.2, size = 0.8) +
    geom_smooth(method = "lm") +
    labs(x = covariate, y = "Day of year") +
    theme_bw()
}

# =============================================================================
# Model comparison utilities
# =============================================================================

# AIC table with delta-AIC, logLik, and marginal/conditional R2
compare_models_mixed <- function(...) {
  models     <- list(...)
  model_names <- as.character(match.call())[-1]

  AIC(...) |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "Model") |>
    mutate(
      Model          = model_names,
      delta          = AIC - min(AIC),
      logLik         = sapply(models, logLik),
      r2_marginal    = sapply(models, function(m) r2(m)$R2_marginal),
      r2_conditional = sapply(models, function(m) r2(m)$R2_conditional)
    ) |>
    arrange(AIC) |>
    select(Model, df, logLik, AIC, delta, r2_marginal, r2_conditional) |>
    as_tibble()
}

# Lightweight AIC table (no R2)
compare_aic <- function(...) {
  model_names <- as.character(match.call())[-1]

  AIC(...) |>
    as.data.frame() |>
    tibble::rownames_to_column(var = "Model") |>
    mutate(Model = model_names, delta = AIC - min(AIC)) |>
    arrange(AIC) |>
    as_tibble()
}

# Extract AIC, RMSE, and R2 for a named list of models (used to build tab1_data)
extract_perf <- function(mod_list, labels) {
  purrr::map2_dfr(mod_list, labels, function(m, lab) {
    perf <- performance::model_performance(
      m, metrics = c("AIC", "R2", "RMSE", "ICC"), estimator = "ML"
    )
    tibble(
      Model = lab,
      AIC   = round(AIC(m), 1),
      RMSE  = round(perf$RMSE, 2),
      R2_m  = round(perf$R2_marginal,    3),
      R2_c  = round(perf$R2_conditional, 3)
    )
  })
}

# =============================================================================
# TABLE 1 — Model comparison
# Build tab1_data by calling this block with all ML models in environment,
# then pass to make_table1() (gt/HTML) or make_kable1() (PDF/LaTeX)
# =============================================================================

# tab1_data <- {
#   g1 <- extract_perf(list(m16, m26, m31), ...) |> mutate(Group = "1. ...", ...)
#   ...
#   bind_rows(g1, g2, g3, g4) |> select(Group, Model, dAIC, RMSE, R2_m, R2_c, Notes)
# }
# See appendix.Rmd for the full tab1_data build.

# --- gt version (HTML / Word) ------------------------------------------------
make_table1 <- function(df) {
  df |>
    gt(groupname_col = "Group") |>
    cols_label(
      Model = "Model",
      dAIC  = md("\u0394AIC"),
      RMSE  = "RMSE (days)",
      R2_m  = md("R\u00b2 (marginal)"),
      R2_c  = md("R\u00b2 (conditional)"),
      Notes = "Notes"
    ) |>
    fmt_number(columns = dAIC,          decimals = 1) |>
    fmt_number(columns = RMSE,          decimals = 2) |>
    fmt_number(columns = c(R2_m, R2_c), decimals = 3) |>
    sub_missing(columns = c(dAIC, RMSE, R2_m, R2_c), missing_text = "\u2014") |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(rows = dAIC == 0)
    ) |>
    tab_style(
      style     = list(cell_text(color = "#888888", style = "italic")),
      locations = cells_body(rows = grepl("excluded", Notes))
    ) |>
    tab_style(
      style     = cell_fill(color = "#EEEEEE"),
      locations = cells_row_groups()
    ) |>
    tab_options(
      table.font.size                    = 10,
      row_group.font.weight              = "bold",
      column_labels.font.weight          = "bold",
      table.border.top.style             = "solid",
      table.border.top.width             = px(2),
      table.border.bottom.style          = "solid",
      table.border.bottom.width          = px(2),
      column_labels.border.bottom.style  = "solid",
      column_labels.border.bottom.width  = px(1),
      row_group.border.top.style         = "solid",
      row_group.border.top.width         = px(1),
      row_group.border.bottom.style      = "none"
    ) |>
    tab_footnote(
      footnote  = "\u0394AIC computed relative to the reference (bold) model within each group. Negative values indicate better AIC than the reference. RMSE in days. R\u00b2 marginal = fixed effects only; R\u00b2 conditional = fixed + random effects.",
      locations = cells_column_labels(columns = dAIC)
    ) |>
    tab_footnote(
      footnote  = "Models marked \u2020 were excluded despite lower AIC due to biologically implausible predictions or overfitting; see Appendix A3.",
      locations = cells_column_labels(columns = Notes)
    ) |>
    tab_header(
      title    = "Table 1. Model selection for linear mixed-effects models of Chinook salmon spawn timing.",
      subtitle = "Models evaluated sequentially: base structure, random slopes, temperature functional form, and interaction terms."
    )
}

# --- kableExtra version (PDF / LaTeX) ----------------------------------------
make_kable1 <- function(df) {
  df |>
    select(-Group) |>
    mutate(
      Model = gsub("_",      "\\\\_",        Model),
      Model = gsub("\u00b2", "$^2$",         Model),
      Model = gsub("\u2020", "$\\\\dagger$", Model),
      Model = gsub("\u00d7", "$\\\\times$",  Model),
      Notes = gsub("_",      "\\\\_",        Notes),
      Notes = gsub("\u2020", "$\\\\dagger$", Notes),
      Notes = gsub("\u00d7", "$\\\\times$",  Notes),
      Notes = gsub("\u0394", "$\\\\Delta$",  Notes)
    ) |>
    kable(
      format    = "latex", booktabs = TRUE, escape = FALSE,
      col.names = c("Model", "$\\Delta$AIC", "RMSE", "$R^2$ (marg.)", "$R^2$ (cond.)", "Notes"),
      digits    = c(0, 1, 2, 3, 3, 0)
    ) |>
    kable_styling(latex_options = "HOLD_position", font_size = 9) |>
    column_spec(1,   width = "4.5cm") |>
    column_spec(2:5, width = "1.2cm") |>
    column_spec(6,   width = "4.0cm") |>
    pack_rows("1. Additive models (random intercept only)", 1, 3,  hline_after = FALSE) |>
    pack_rows("2. Random slope evaluation (ML fit)",        4, 5,  hline_after = FALSE) |>
    pack_rows("3. Temperature functional form (ML fit)",    6, 8,  hline_after = FALSE) |>
    pack_rows("4. Interaction terms (ML fit)",              9, 11, hline_after = FALSE)
}

# =============================================================================
# TABLE 2 — Final model coefficients, random effects, and performance
# Pass mod_final (REML fit) to make_table2() or make_kable2()
# =============================================================================

# --- gt version (HTML / Word) ------------------------------------------------
make_table2 <- function(mod) {

  params <- parameters::model_parameters(mod, effects = "fixed", ci = 0.95)
  vc     <- as.data.frame(VarCorr(mod))
  perf   <- performance::model_performance(mod, metrics = c("R2", "RMSE", "ICC", "AIC"))

  fe <- params |>
    as_tibble() |>
    select(Parameter, Coefficient, CI_low, CI_high, p) |>
    mutate(
      Parameter = case_when(
        Parameter == "(Intercept)"           ~ "Intercept",
        Parameter == "ns(temp_90, df = 3)1"  ~ "Temperature spline term 1",
        Parameter == "ns(temp_90, df = 3)2"  ~ "Temperature spline term 2",
        Parameter == "ns(temp_90, df = 3)3"  ~ "Temperature spline term 3",
        grepl("^stream", Parameter)          ~ gsub("stream", "Stream: ", Parameter),
        grepl("^year",   Parameter)          ~ gsub("year",   "Year: ",   Parameter),
        TRUE                                 ~ Parameter
      ),
      CI      = sprintf("[%.2f, %.2f]", CI_low, CI_high),
      p_fmt   = case_when(p < 0.001 ~ "<0.001", TRUE ~ sprintf("%.3f", p)),
      Section = "Fixed effects"
    ) |>
    select(Section, Parameter, Estimate = Coefficient, CI, p = p_fmt)

  re <- tibble(
    Section   = "Random effects",
    Parameter = c(
      "\u03c3\u00b2 (residual variance)",
      "\u03c4\u2080\u2080 COMID (intercept variance)",
      "\u03c4\u2081\u2081 COMID (slope variance, temp_90)",
      "\u03c1\u2080\u2081 COMID (intercept-slope correlation)",
      "ICC", "N (COMID)", "N (observations)"
    ),
    Estimate  = c(
      round(vc$vcov[vc$grp == "Residual"], 2),
      round(vc$vcov[vc$grp == "COMID" & vc$var1 == "(Intercept)" & is.na(vc$var2)], 2),
      round(vc$vcov[vc$grp == "COMID" & vc$var1 == "temp_90"     & is.na(vc$var2)], 2),
      round(attr(VarCorr(mod)$COMID, "correlation")[2, 1], 2),
      round(perf$ICC, 3),
      nrow(ranef(mod)$COMID),
      nobs(mod)
    ),
    CI = NA_character_,
    p  = NA_character_
  )

  mp <- tibble(
    Section   = "Model performance",
    Parameter = c("R\u00b2 marginal", "R\u00b2 conditional", "RMSE (days)"),
    Estimate  = c(round(perf$R2_marginal, 3), round(perf$R2_conditional, 3), round(perf$RMSE, 2)),
    CI        = NA_character_,
    p         = NA_character_
  )

  bind_rows(fe, re, mp) |>
    gt(groupname_col = "Section") |>
    cols_label(
      Parameter = "Parameter",
      Estimate  = "Estimate",
      CI        = "95% CI",
      p         = md("*p*-value")
    ) |>
    fmt_number(columns = Estimate, decimals = 2,
               rows = !is.na(Estimate) & Section != "Model performance") |>
    sub_missing(columns = c(CI, p), missing_text = "\u2014") |>
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(
        rows = p %in% "<0.001" | Section %in% c("Random effects", "Model performance")
      )
    ) |>
    tab_style(
      style     = cell_text(color = "#555555", style = "italic"),
      locations = cells_body(rows = grepl("^Temperature spline", Parameter))
    ) |>
    tab_style(
      style     = cell_fill(color = "#F5F5F5"),
      locations = cells_row_groups()
    ) |>
    tab_options(
      table.font.size                   = 10,
      row_group.font.weight             = "bold",
      column_labels.font.weight         = "bold",
      table.border.top.style            = "solid",
      table.border.top.width            = px(2),
      table.border.bottom.style         = "solid",
      table.border.bottom.width         = px(2),
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1)
    ) |>
    tab_footnote(
      footnote  = "Reference levels: Stream = Bear Valley Creek; Year = 2002. temp_90 scaled to mean = 0, SD = 1.",
      locations = cells_column_labels(columns = Parameter)
    ) |>
    tab_footnote(
      footnote  = "Natural spline basis terms (df = 3) are not individually interpretable; see Figure 5 for the estimated temperature\u2013spawn timing relationship.",
      locations = cells_body(rows = grepl("^Temperature spline", Parameter), columns = Parameter)
    ) |>
    tab_header(
      title    = "Table 2. Final model parameter estimates.",
      subtitle = md("Linear mixed-effects model: `yday ~ ns(temp_90, df=3) + stream + year + (1 + temp_90 | COMID)`, fit with REML.")
    )
}

# --- kableExtra version (PDF / LaTeX) ----------------------------------------
make_kable2 <- function(mod) {

  params <- parameters::model_parameters(mod, effects = "fixed", ci = 0.95)
  vc     <- as.data.frame(VarCorr(mod))
  perf   <- performance::model_performance(mod, metrics = c("R2", "RMSE", "ICC"))

  fe <- params |>
    as_tibble() |>
    mutate(
      Parameter = case_when(
        Parameter == "(Intercept)"           ~ "Intercept",
        Parameter == "ns(temp_90, df = 3)1"  ~ "Temp.\\ spline term 1",
        Parameter == "ns(temp_90, df = 3)2"  ~ "Temp.\\ spline term 2",
        Parameter == "ns(temp_90, df = 3)3"  ~ "Temp.\\ spline term 3",
        grepl("^stream", Parameter)          ~ gsub("stream", "Stream: ", Parameter),
        grepl("^year",   Parameter)          ~ gsub("year",   "Year: ",   Parameter),
        TRUE                                 ~ Parameter
      ),
      CI = sprintf("[%.2f,\\ %.2f]", CI_low, CI_high),
      p  = case_when(p < 0.001 ~ "<0.001", TRUE ~ sprintf("%.3f", p))
    ) |>
    select(Parameter, Estimate = Coefficient, CI, p)

  re <- tibble(
    Parameter = c(
      "$\\sigma^2$ (residual)",
      "$\\tau_{00}$ COMID (intercept)",
      "$\\tau_{11}$ COMID (slope)",
      "$\\rho_{01}$ COMID (int.\\ $\\times$ slope)",
      "ICC", "N (COMID)", "N (obs.)"
    ),
    Estimate = c(
      round(vc$vcov[vc$grp == "Residual"], 2),
      round(vc$vcov[vc$grp == "COMID" & vc$var1 == "(Intercept)" & is.na(vc$var2)], 2),
      round(vc$vcov[vc$grp == "COMID" & vc$var1 == "temp_90"     & is.na(vc$var2)], 2),
      round(attr(VarCorr(mod)$COMID, "correlation")[2, 1], 2),
      round(perf$ICC, 3),
      nrow(ranef(mod)$COMID),
      nobs(mod)
    ),
    CI = NA_character_,
    p  = NA_character_
  )

  mp <- tibble(
    Parameter = c("$R^2$ marginal", "$R^2$ conditional", "RMSE (days)"),
    Estimate  = c(round(perf$R2_marginal, 3), round(perf$R2_conditional, 3), round(perf$RMSE, 2)),
    CI        = NA_character_,
    p         = NA_character_
  )

  n_fe <- nrow(fe); n_re <- nrow(re); n_mp <- nrow(mp)

  bind_rows(fe, re, mp) |>
    kable(
      format    = "latex", booktabs = TRUE, escape = FALSE,
      col.names = c("Parameter", "Est.", "95\\% CI", "\\textit{p}"),
      digits    = 2
    ) |>
    kable_styling(latex_options = "HOLD_position", font_size = 9,
                  full_width = FALSE) |>
    column_spec(1, width = "4.8cm") |>
    column_spec(2, width = "1.2cm") |>
    column_spec(3, width = "2.6cm") |>
    column_spec(4, width = "1.4cm") |>
    pack_rows("Fixed effects",     1,           n_fe,                hline_after = FALSE) |>
    pack_rows("Random effects",    n_fe + 1,    n_fe + n_re,         hline_after = FALSE) |>
    pack_rows("Model performance", n_fe + n_re + 1, n_fe + n_re + n_mp, hline_after = FALSE)
    # footnote(
    #   general           = "Reference levels: Stream = Bear Valley Creek; Year = 2002. temp\\_90 scaled to mean = 0, SD = 1. Spline terms (df = 3) are not individually interpretable; see Figure 5.",
    #   general_title     = "",
    #   footnote_as_chunk = FALSE,
    #   escape            = FALSE
    # )
}
