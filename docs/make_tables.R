# =============================================================================
# Publication-quality tables for Middle Fork Chinook Phenology manuscript
# Table 1: Model comparison (AIC-based selection)
# Table 2: Final model coefficients + random effects
# =============================================================================

library(lme4)
library(splines)
library(dplyr)
library(gt)
library(gtExtras)

# NOTE: This script assumes the following objects are already in your environment
# from the appendix.Rmd workflow:
#   df_mod         — the cleaned, scaled modeling dataset
#   mod_final      — lmer(yday ~ ns(temp_90, df=3) + stream + year + (1+temp_90|COMID), REML=TRUE)

# and the ML-fit models used for AIC comparison:
#   m16            — quadratic, random intercept only, ML
#   m16_rs         — quadratic + random slopes, ML
#   m16_rs_noquad  — linear + random slopes, ML
#   mod_spline3    — ns(df=3) + random slopes, ML
#   m201           — ns(df=3) + temp_90*stream interaction, ML
#   m202           — ns(df=3) + temp_90*year interaction, ML

# =============================================================================
# HELPER: extract AIC, delta-AIC, RMSE, R2 for a list of models
# =============================================================================

extract_perf <- function(mod_list, labels) {
  purrr::map2_dfr(mod_list, labels, function(m, lab) {
    perf   <- performance::model_performance(m, metrics = c("AIC", "R2", "RMSE", "ICC"), estimator = "ML")
    r2_m   <- round(perf$R2_marginal,   3)
    r2_c   <- round(perf$R2_conditional, 3)
    rmse   <- round(perf$RMSE, 2)
    aic    <- round(AIC(m), 1)
    tibble(
      Model  = lab,
      AIC    = aic,
      RMSE   = rmse,
      R2_m   = r2_m,
      R2_c   = r2_c
    )
  })
}

# =============================================================================
# TABLE 1 — Model comparison
# Groups: (1) additive base models, (2) random slopes evaluation,
#          (3) spline vs quadratic, (4) interaction evaluation
# =============================================================================

# --- (1) Key additive models (subset; full set in Appendix A3) ---------------
row_additive <- tibble(
  Model = c("m16: temp\u2082\u2080 + stream + year",
            "m26: temp\u2082\u2080 + stream + year + elevation†",
            "m31: m26 + slope†"),
  Group = "Additive models"
)

# --- (2) Random slopes -------------------------------------------------------
row_rs <- tibble(
  Model = c("m16: random intercept only",
            "m16_rs: + random slopes for temperature"),
  Group = "Random slope evaluation"
)

# --- (3) Spline vs quadratic (both with random slopes, ML fit) ---------------
row_spline <- tibble(
  Model = c("m16_rs: quadratic temperature",
            "mod_spline3: natural spline (df = 3)",
            "mod_spline4: natural spline (df = 4)†"),
  Group = "Temperature functional form"
)

# --- (4) Interaction models --------------------------------------------------
row_int <- tibble(
  Model = c("mod_spline3: no interactions",
            "m201: + temperature × stream",
            "m202: + temperature × year†"),
  Group = "Interaction evaluation"
)

# ---- Manually populate with known values from appendix ----------------------
# Key ΔAIC values from appendix narrative:
#   m16 vs m26:             ΔAIC = 115.6 (m26 better likelihood but implausible elevation effect)
#   m16 vs m16_rs:          ΔAIC = 510   (random slopes; RMSE 2.02 -> 1.78; marginal R2 0.714 -> 0.698)
#   m16_rs vs m16_rs_noquad: ΔAIC = 20.4 (quadratic retained)
#   m16_rs vs spline df=3:  ΔAIC = 139   (spline substantially better)
#   spline df=3 vs df=4:    ΔAIC = 66    (df=4 better AIC but implausible at warm end)
#   spline df=3 vs m201:    ΔAIC ≈ 18    (temp×stream; non-significant terms)
#   spline df=3 vs m202:    large ΔAIC   (temp×year; implausible inverted response)
#
# NOTE: ΔAIC is computed within each group, relative to the group's best model.
# Replace AIC column values with actual model AICs when running live in R.
# The structure and logic below are correct; exact numbers are from appendix prose.

# tab1_data <- bind_rows(
# 
#   # --- Group 1: Additive base model selection ---------------------------------
#   # ΔAIC relative to best-AIC additive model (m26/m31), but those excluded;
#   # show ΔAIC relative to m16 (selected model) = 0 reference within this group
#   # tibble(
#   #   Group = "1. Additive models (random intercept only)",
#   #   Model = c("m16: temp\u2089\u2080 + stream + year",
#   #             "m26: m16 + elevation\u2020",
#   #             "m31: m26 + slope\u2020"),
#   #   dAIC  = c(0, AIC(m26)-AIC(m16), AIC(m31)-AIC(m16)),     # negative = better AIC than m16 (but excluded)
#   #   RMSE  = c(2.02, 1.97, 1.97),
#   #   R2_m  = c(0.714, 0.785, 0.786),
#   #   R2_c  = c(0.980, 0.981, 0.981),
#   #   Notes = c("Selected base model",
#   #             "Implausible elevation direction; excluded",
#   #             "Implausible elevation direction; excluded")
#   # ),
#   
#   g1 <- extract_perf(
#     list(m16, m26, m31),
#     c("m16: temp\u2089\u2080 + stream + year",
#       "m26: m16 + elevation\u2020", 
#       "m31: m26 + slope\u2020")
#     ) |>
#     mutate(
#       Group = "1. Additive models (random intercept only)",
#       dAIC  = AIC - AIC[Model == "m16: temp\u2089\u2080 + stream + year"],
#       Notes = c("Selected base model", 
#                 "Implausible elevation direction; excluded", 
#                 "Implausible elevation direction; excluded")
#     ),
# 
#   # --- Group 2: Random slope evaluation ---------------------------------------
#   tibble(
#     Group = "2. Random slope evaluation (ML fit)",
#     Model = c("m16: random intercept only",
#               "m16_rs: + random slopes for temp\u2089\u2080"),
#     dAIC  = c(AIC(m16)-AIC(m16_rs), 0),
#     RMSE  = c(2.02, 1.78),
#     R2_m  = c(0.714, 0.698),
#     R2_c  = c(0.980, 0.985),
#     Notes = c("", "\u0394AIC = 510 vs. intercept-only")
#   ),
# 
#   # --- Group 3: Temperature functional form (with random slopes, ML) ----------
#   tibble(
#     Group = "3. Temperature functional form (ML fit)",
#     Model = c("Quadratic: temp\u2089\u2080 + temp\u2089\u2080\u00b2",
#               "Natural spline df = 3",
#               "Natural spline df = 4\u2020"),
#     dAIC  = c(AIC(m16_rs)-AIC(mod_spline3), 0, AIC(mod_spline4)-AIC(mod_spline3)), # spline df=3 = reference; df=4 has lower AIC but excluded
#     RMSE  = c(1.78, 1.75, 1.74),
#     R2_m  = c(0.698, 0.712, 0.714),
#     R2_c  = c(0.985, 0.985, 0.985),
#     Notes = c("",
#               "Selected final functional form",
#               "Biologically implausible at high temperatures; excluded")
#   ),
# 
#   # --- Group 4: Interaction terms (vs. final spline model) --------------------
#   tibble(
#     Group = "4. Interaction terms (ML fit)",
#     Model = c("Final model: no interactions",
#               "+ temperature \u00d7 stream",
#               "+ temperature \u00d7 year\u2020"),
#     dAIC  = c(0, AIC(m201) - AIC(mod_spline3), AIC(m202) - AIC(mod_spline3)),
#     RMSE  = c(1.75, 1.74, NA),
#     R2_m  = c(0.712, 0.713, NA),
#     R2_c  = c(0.985, 0.985, NA),
#     Notes = c("",
#               "Non-significant terms; marginal gain over random slopes",
#               "Implausible inverted quadratic response; excluded")
#     )
#   ) |> 
#   select(-AIC)

# =============================================================================
# Build tab1_data dynamically from model objects
# All models must be in environment (ML fit unless noted)
# =============================================================================
g1 <- extract_perf(
  list(m16, m26, m31),
  c(
    "m16: temp_90 + stream + year",
    "m26: m16 + elevation\u2020",
    "m31: m26 + slope\u2020"
  )
) |>
  mutate(
    Group = "1. Additive models (random intercept only)",
    dAIC = AIC - AIC[Model == "m16: temp_90 + stream + year"],
    Notes = c(
      "Selected base model",
      "Implausible elevation direction; excluded",
      "Implausible elevation direction; excluded"
    )
  )

g2 <- extract_perf(
  list(m16_rs, m16),
  c(
    "m16_rs: + random slopes for temp_90",
    "m16: random intercept only"
  )
) |>
  mutate(
    Group = "2. Random slope evaluation (ML fit)",
    dAIC = AIC - AIC[Model == "m16_rs: + random slopes for temp_90"],
    Notes = c(
      "\u0394AIC = 507.1 vs. intercept-only",
      ""
    )
  )

g3 <- extract_perf(
  list(mod_spline3, m16_rs, mod_spline4),
  c(
    "Natural spline df = 3",
    "Quadratic: temp_90 + temp_90\u00b2",
    "Natural spline df = 4\u2020"
  )
) |>
  mutate(
    Group = "3. Temperature functional form (ML fit)",
    dAIC = AIC - AIC[Model == "Natural spline df = 3"],
    Notes = c(
      "Selected final functional form",
      "",
      "Biologically implausible at high temperatures; excluded"
    )
  )
g4 <- extract_perf(
  list(mod_spline3, m201, m202),
  c(
    "Final model: no interactions",
    "+ temperature \u00d7 stream",
    "+ temperature \u00d7 year\u2020"
  )
) |>
  mutate(
    Group = "4. Interaction terms (ML fit)",
    dAIC = AIC - AIC[Model == "Final model: no interactions"],
    Notes = c(
      "",
      "Spline extrapolation unstable in data-sparse streams; excluded",
      "Large AIC gain likely reflects year-specific data ranges, not biological interaction; excluded"
    )
  )

tab1_data <- bind_rows(g1, g2, g3, g4) |> 
  select(Group, Model, dAIC, RMSE, R2_m, R2_c, Notes)



# =============================================================================
# BUILD TABLE 1 with gt
# =============================================================================

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
    fmt_number(columns = dAIC, decimals = 1) |>
    fmt_number(columns = RMSE, decimals = 2) |>
    fmt_number(columns = c(R2_m, R2_c), decimals = 3) |>
    sub_missing(columns = c(dAIC, RMSE, R2_m, R2_c), missing_text = "—") |>
    # Bold selected/reference rows (dAIC = 0)
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = dAIC == 0)
    ) |>
    # Grey + italic for excluded models
    tab_style(
      style = list(cell_text(color = "#888888", style = "italic")),
      locations = cells_body(rows = grepl("excluded", Notes))
    ) |>
    # Light header shading for group rows
    tab_style(
      style = cell_fill(color = "#EEEEEE"),
      locations = cells_row_groups()
    ) |>
    tab_options(
      table.font.size = 10,
      row_group.font.weight = "bold",
      column_labels.font.weight = "bold",
      table.border.top.style    = "solid",
      table.border.top.width    = px(2),
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(2),
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1),
      row_group.border.top.style    = "solid",
      row_group.border.top.width    = px(1),
      row_group.border.bottom.style = "none"
    ) |>
    tab_footnote(
      footnote = "Models marked \u2020 were excluded despite lower AIC due to biologically implausible predictions or overfitting; see Results and Appendix A3.",
      locations = cells_column_labels(columns = Notes)
    ) |>
    tab_footnote(
      footnote = "\u0394AIC computed relative to the reference (bold) model within each group. Negative values indicate better AIC than the reference. RMSE in days. R\u00b2 marginal = variance explained by fixed effects only; R\u00b2 conditional = fixed + random effects combined.",
      locations = cells_column_labels(columns = dAIC)
    ) |>
    tab_header(
      title = "Table 1. Model selection for linear mixed-effects models of Chinook salmon spawn timing.",
      subtitle = "Models evaluated sequentially: base structure, random slopes, temperature functional form, and interaction terms."
    )
}

# table1 <- make_table1(tab1_data)

# =============================================================================
# TABLE 2 — Final model coefficients + random effects
# Uses mod_final (REML fit) + performance::model_performance()
# =============================================================================

make_table2 <- function(mod) {

  # ---- Fixed effects ---------------------------------------------------------
  params <- parameters::model_parameters(mod, effects = "fixed", ci = 0.95)

  fe <- params |>
    as_tibble() |>
    select(Parameter, Coefficient, CI_low, CI_high, p) |>
    mutate(
      # Clean up parameter names for display
      Parameter = case_when(
        Parameter == "(Intercept)"             ~ "Intercept",
        Parameter == "ns(temp_90, df = 3)1"   ~ "Temperature spline term 1",
        Parameter == "ns(temp_90, df = 3)2"   ~ "Temperature spline term 2",
        Parameter == "ns(temp_90, df = 3)3"   ~ "Temperature spline term 3",
        grepl("^stream", Parameter)            ~ gsub("stream", "Stream: ", Parameter),
        grepl("^year", Parameter)              ~ gsub("year", "Year: ", Parameter),
        TRUE                                   ~ Parameter
      ),
      CI = sprintf("[%.2f, %.2f]", CI_low, CI_high),
      p_fmt = case_when(
        p < 0.001 ~ "<0.001",
        p < 0.01  ~ sprintf("%.3f", p),
        TRUE      ~ sprintf("%.3f", p)
      ),
      Section = case_when(
        Parameter == "Intercept"                        ~ "Fixed effects",
        grepl("^Temperature", Parameter)                ~ "Fixed effects",
        grepl("^Stream", Parameter)                     ~ "Fixed effects",
        grepl("^Year", Parameter)                       ~ "Fixed effects",
        TRUE                                            ~ "Fixed effects"
      )
    ) |>
    select(Section, Parameter, Estimate = Coefficient, CI, p = p_fmt)

  # ---- Random effects --------------------------------------------------------
  vc <- as.data.frame(VarCorr(mod))
  perf <- performance::model_performance(mod, metrics = c("R2", "RMSE", "ICC", "AIC"))

  re_rows <- tibble(
    Section   = "Random effects",
    Parameter = c(
      "\u03c3\u00b2 (residual variance)",
      "\u03c4\u2080\u2080 COMID (intercept variance)",
      "\u03c4\u2081\u2081 COMID (slope variance, temp_90)",
      "\u03c1\u2080\u2081 COMID (intercept-slope correlation)",
      "ICC",
      "N (COMID)",
      "N (observations)"
    ),
    Estimate  = c(
      round(vc$vcov[vc$grp == "Residual"], 2),
      round(vc$vcov[vc$grp == "COMID" & vc$var1 == "(Intercept)" & is.na(vc$var2)], 2),
      round(vc$vcov[vc$grp == "COMID" & vc$var1 == "temp_90"     & is.na(vc$var2)], 2),
      # round(vc$vcov[vc$grp == "COMID" & !is.na(vc$var2)], 2),  # covariance
      round(attr(VarCorr(mod)$COMID, "correlation")[2, 1], 2),   # correlation, not covariance
      round(perf$ICC, 3),
      nrow(ranef(mod)$COMID),
      nobs(mod)
    ),
    CI        = NA_character_,
    p         = NA_character_
  )

  # ---- Performance footer rows -----------------------------------------------
  perf_rows <- tibble(
    Section   = "Model performance",
    Parameter = c("R\u00b2 marginal", "R\u00b2 conditional", "RMSE (days)"),
    Estimate  = c(round(perf$R2_marginal, 3), round(perf$R2_conditional, 3), round(perf$RMSE, 2)),
    CI        = NA_character_,
    p         = NA_character_
  )

  all_rows <- bind_rows(fe, re_rows, perf_rows)

  # ---- Build gt table --------------------------------------------------------
  all_rows |>
    gt(groupname_col = "Section") |>
    cols_label(
      Parameter = "Parameter",
      Estimate  = "Estimate",
      CI        = "95% CI",
      p         = md("*p*-value")
    ) |>
    fmt_number(columns = Estimate, decimals = 2, rows = !is.na(Estimate) & Section != "Model performance") |>
    sub_missing(columns = c(CI, p), missing_text = "—") |>
    tab_style(
      style = cell_text(weight = "bold"),
      locations = cells_body(rows = p %in% c("<0.001") | (Section %in% c("Random effects", "Model performance")))
    ) |>
    tab_style(
      style = cell_text(color = "#555555", style = "italic"),
      locations = cells_body(rows = grepl("^Temperature spline", Parameter))
    ) |>
    tab_style(
      style = cell_fill(color = "#F5F5F5"),
      locations = cells_row_groups()
    ) |>
    tab_options(
      table.font.size = 10,
      row_group.font.weight = "bold",
      column_labels.font.weight = "bold",
      table.border.top.style = "solid",
      table.border.top.width = px(2),
      table.border.bottom.style = "solid",
      table.border.bottom.width = px(2),
      column_labels.border.bottom.style = "solid",
      column_labels.border.bottom.width = px(1)
    ) |>
    tab_footnote(
      footnote = "Reference levels: Stream = Bear Valley Creek; Year = 2002. Continuous predictor (temp_90) was scaled to mean = 0, SD = 1 prior to fitting.",
      locations = cells_column_labels(columns = Parameter)
    ) |>
    tab_footnote(
      footnote = "Natural spline basis terms (df = 3) are not directly interpretable as individual coefficients; see Figure 5 for the estimated temperature\u2013spawn timing relationship.",
      locations = cells_body(rows = grepl("^Temperature spline", Parameter), columns = Parameter)
    ) |>
    tab_header(
      title = "Table 2. Final model parameter estimates.",
      subtitle = md("Linear mixed-effects model: `yday ~ ns(temp_90, df=3) + stream + year + (1 + temp_90 | COMID)`, fit with REML.")
    )
}

# Run when mod_final is in environment:
# table2 <- make_table2(mod_final)

# =============================================================================
# SAVE as Word-compatible HTML (paste into Word, or use officer/flextable
# for direct .docx embedding — see note below)
# =============================================================================

# Option A: save as standalone HTML for copy-paste into Word
# gtsave(table1, "table1_model_comparison.html")
# gtsave(table2, "table2_coefficients.html")

# Option B: convert to flextable for direct .docx embedding
# (requires flextable + officer packages)
make_flextable_from_gt <- function(gt_tbl, filename) {
  # gt -> flextable via as_flextable(), then save with officer
  ft <- gt_tbl |> as_flextable()
  doc <- officer::read_docx() |>
    flextable::body_add_flextable(ft)
  print(doc, target = filename)
}

# Usage (run in R with models loaded):
# make_flextable_from_gt(table1, "table1.docx")
# make_flextable_from_gt(table2, "table2.docx")

message("Table functions defined: make_table1(tab1_data) and make_table2(mod_final)")
message("Run make_table1(tab1_data) and make_table2(mod_final) when models are loaded.")
message("For .docx output: gtsave(table1, 'table1.html') or use make_flextable_from_gt()")
