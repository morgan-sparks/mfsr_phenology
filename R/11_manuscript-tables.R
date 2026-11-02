# =============================================================================
# 11_manuscript-tables.R
#
# CJFAS revision: rebuild manuscript Tables 1 and 2 for the revised analysis.
#   Table 1  model comparison (ML): random-effect structure, fixed effects,
#            temperature functional form
#   Table 2  final model (REML): fixed effects, random effects, performance
#
# Outputs: tables/revision/Table_01_rev.csv, Table_02_rev.csv, and, if the gt
# package is installed, Table_01_rev.docx / Table_02_rev.docx (same styling as
# make_table1() / make_table2() in R/utils.R).
# =============================================================================

library(lme4)
library(splines)
library(performance)
source(here::here("R", "revision_setup.R"))   # model_data(), index, out_tab

d <- model_data()
d$Tc <- d$T - mean(d$T)                        # centred T: intercept = Bear Valley, 2002, mean T
ctl <- lmerControl(optimizer = "bobyqa")
fit <- function(f) lmer(f, data = d, REML = FALSE, control = ctl)

perf_row <- function(m, label) {
  r2 <- suppressWarnings(performance::r2_nakagawa(m))
  tibble(Model = label, k = attr(logLik(m), "df"), AIC = AIC(m),
         RMSE = sqrt(mean(residuals(m)^2)),
         R2_m = r2$R2_marginal, R2_c = r2$R2_conditional,
         singular = isSingular(m))
}

# -----------------------------------------------------------------------------
# Table 1: model comparison
# -----------------------------------------------------------------------------
RE  <- "(1 | COMID) + (1 | key)"
fin <- "T + stream + year"

g1 <- bind_rows(
  perf_row(fit(as.formula(paste("yday ~", fin, "+", RE))),            "Reach + reach-year intercepts"),
  perf_row(fit(yday ~ T + stream + year + (1 | COMID)),                "Reach intercept only"),
  perf_row(fit(yday ~ T + stream + year + (1 + T | COMID) + (1 | key)), "Reach + reach-year intercepts, random slopes for T")
) |> mutate(Group = "1. Random-effect structure (fixed effects: T + stream + year)")

g2 <- bind_rows(
  perf_row(fit(as.formula(paste("yday ~ T + stream + year +", RE))), "T + stream + year"),
  perf_row(fit(as.formula(paste("yday ~ T + year +", RE))),          "T + year"),
  perf_row(fit(as.formula(paste("yday ~ T + stream +", RE))),        "T + stream"),
  perf_row(fit(as.formula(paste("yday ~ T +", RE))),                 "T"),
  perf_row(fit(as.formula(paste("yday ~ stream + year +", RE))),     "stream + year (no temperature)")
) |> mutate(Group = "2. Fixed effects (reach + reach-year intercepts)")

g3 <- bind_rows(
  perf_row(fit(as.formula(paste("yday ~ T + stream + year +", RE))),               "Linear: T"),
  perf_row(fit(as.formula(paste("yday ~ Ts + I(Ts^2) + stream + year +", RE))),   "Quadratic: T + T²"),
  perf_row(fit(as.formula(paste("yday ~ ns(Ts, 3) + stream + year +", RE))),      "Natural spline (df = 3)"),
  perf_row(fit(as.formula(paste("yday ~ T_bar + T_dev + stream + year +", RE))),  "Within–between: T̄ + (T − T̄)")
) |> mutate(Group = "3. Temperature form (stream + year; reach + reach-year intercepts)")

tbl1 <- bind_rows(g1, g2, g3) |>
  group_by(Group) |>
  mutate(dAIC = AIC - AIC[1]) |>                      # first row of each group = reference
  ungroup() |>
  mutate(Notes = case_when(
    singular & grepl("random slopes", Model) ~ "Singular fit (intercept–slope correlation = −1); not supported",
    singular ~ "Singular fit (reach variance estimated at 0; absorbed by reach-year term)",
    Model %in% c("Reach + reach-year intercepts", "T + stream + year", "Linear: T") ~ "Final model",
    TRUE ~ "")) |>
  select(Group, Model, k, dAIC, RMSE, R2_m, R2_c, Notes)
print(tbl1, n = 20, width = Inf)
write_csv(tbl1, file.path(out_tab, "Table_01_rev.csv"))

# -----------------------------------------------------------------------------
# Table 2: final model (REML), T centred at its mean
# -----------------------------------------------------------------------------
final <- lmer(yday ~ Tc + stream + year + (1 | COMID) + (1 | key), d, REML = TRUE, control = ctl)
ci <- confint(final, method = "Wald", parm = "beta_")
fe <- tibble(term = names(fixef(final)), Estimate = unname(fixef(final)),
             SE = sqrt(diag(vcov(final)))) |>
  mutate(CI_low = ci[term, 1], CI_high = ci[term, 2],
         p = 2 * pnorm(-abs(Estimate / SE)),
         Parameter = case_when(
           term == "(Intercept)"   ~ "Intercept (Bear Valley, 2002, mean T)",
           term == "Tc"            ~ "Thermal index, T (d per °C)",
           grepl("^stream", term)  ~ sub("stream", "Stream: ", term),
           grepl("^year", term)    ~ sub("year", "Year: ", term)),
         Section = "Fixed effects",
         CI = sprintf("[%.2f, %.2f]", CI_low, CI_high),
         p  = ifelse(p < 0.001, "<0.001", sprintf("%.3f", p))) |>
  select(Section, Parameter, Estimate, CI, p)

vc <- as.data.frame(VarCorr(final))
r2 <- performance::r2_nakagawa(final)
re <- tibble(
  Section = "Random effects",
  Parameter = c("SD reach-year intercept (d)", "SD reach (COMID) intercept (d)", "SD residual (d)",
                "ICC (adjusted)", "N reach-years", "N reaches (COMID)", "N redds"),
  Estimate = c(vc$sdcor[vc$grp == "key"], vc$sdcor[vc$grp == "COMID"], vc$sdcor[vc$grp == "Residual"],
               performance::icc(final)$ICC_adjusted, nlevels(d$key), nlevels(d$COMID), nrow(d)),
  CI = NA_character_, p = NA_character_)
mp <- tibble(
  Section = "Model performance",
  Parameter = c("R² marginal", "R² conditional", "RMSE (days)"),
  Estimate = c(r2$R2_marginal, r2$R2_conditional, sqrt(mean(residuals(final)^2))),
  CI = NA_character_, p = NA_character_)

tbl2 <- bind_rows(fe, re, mp)
print(tbl2, n = 30, width = Inf)
write_csv(tbl2, file.path(out_tab, "Table_02_rev.csv"))

# -----------------------------------------------------------------------------
# Word tables via gt (run locally; gt is not required for the CSVs above)
# -----------------------------------------------------------------------------
if (requireNamespace("gt", quietly = TRUE)) {
  library(gt)
  gt_style <- function(x) {
    x |>
      tab_style(style = cell_fill(color = "#EEEEEE"), locations = cells_row_groups()) |>
      tab_options(
        table.font.size = 10, row_group.font.weight = "bold", column_labels.font.weight = "bold",
        table.border.top.style = "solid", table.border.top.width = px(2),
        table.border.bottom.style = "solid", table.border.bottom.width = px(2),
        column_labels.border.bottom.style = "solid", column_labels.border.bottom.width = px(1))
  }

  t1 <- tbl1 |>
    mutate(Model = ifelse(Notes == "Final model", paste0(Model, "*"), Model)) |>
    gt(groupname_col = "Group") |>
    cols_label(Model = "Model", k = "k", dAIC = md("ΔAIC"), RMSE = "RMSE (days)",
               R2_m = md("R² (marginal)"), R2_c = md("R² (conditional)"), Notes = "Notes") |>
    fmt_number(columns = dAIC, decimals = 1) |>
    fmt_number(columns = RMSE, decimals = 2) |>
    fmt_number(columns = c(R2_m, R2_c), decimals = 3) |>
    sub_missing(columns = everything(), missing_text = "—") |>
    tab_style(style = cell_text(weight = "bold"), locations = cells_body(rows = Notes == "Final model")) |>
    gt_style() |>
    tab_footnote(md(paste0(
      "T = reach-year thermal index: ", INDEX_N, "-d mean modeled stream temperature, ",
      doy_lab(INDEX_END_DOY - INDEX_N + 1), "–", doy_lab(INDEX_END_DOY),
      ". All models fit by maximum likelihood (n = ", nrow(d), " redds). ",
      "ΔAIC is relative to the first model in each group; k = number of parameters. ",
      "R² marginal = fixed effects only; R² conditional = fixed + random effects. ",
      "* final model."))) |>
    tab_header(title = "Table 1. Model comparison for linear mixed-effects models of Chinook salmon spawn timing.")
  gtsave(t1, file.path(out_tab, "Table_01_rev.docx"))

  t2 <- tbl2 |>
    gt(groupname_col = "Section") |>
    cols_label(Parameter = "Parameter", Estimate = "Estimate", CI = "95% CI", p = md("*p*-value")) |>
    fmt_number(columns = Estimate, decimals = 2, rows = !grepl("^N ", Parameter)) |>
    fmt_number(columns = Estimate, decimals = 0, rows = grepl("^N ", Parameter)) |>
    sub_missing(columns = c(CI, p), missing_text = "—") |>
    gt_style() |>
    tab_footnote(md(paste0(
      "Model: spawn day of year ~ T + stream + year + (1 | reach) + (1 | reach-year), fit by REML. ",
      "T centred at its mean (", sprintf("%.1f", mean(d$T)), " °C). ",
      "Reference levels: Stream = Bear Valley Creek; Year = 2002. CIs are Wald intervals."))) |>
    tab_header(title = "Table 2. Final model parameter estimates.")
  gtsave(t2, file.path(out_tab, "Table_02_rev.docx"))
  message("Word tables written.")
} else {
  message("gt not installed: CSV tables only.")
}
message("Done.")
