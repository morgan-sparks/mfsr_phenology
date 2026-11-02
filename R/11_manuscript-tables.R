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
source(here::here("R", "revision_setup.R"))   # model_data(), ctl, fit_summary(), index labels

d <- model_data()
# Centre T_bar at the average reach (each reach once) so the Table 2 intercept
# is the predicted spawn DOY for Bear Valley, 2002, an average reach, T_dev = 0
T_bar_ref <- mean(distinct(d, COMID, T_bar)$T_bar)
d$T_bar_c <- d$T_bar - T_bar_ref

# -----------------------------------------------------------------------------
# Table 1: model comparison (ML), in four groups. Within each group, dAIC is
# relative to the group's first row; fm() builds yday ~ <fixed> + <random>.
# -----------------------------------------------------------------------------
RE  <- "(1 | COMID) + (1 | key)"
fm  <- function(fixed, re = RE) {
  lmer(as.formula(paste("yday ~", fixed, "+", re)), d, REML = FALSE, control = ctl)
}
perf_row <- function(m, label) fit_summary(m, label) |> rename(Model = model)
FIN <- "T_bar + T_dev + stream + year"
LAB_FINAL <- "T\u0304 + (T \u2212 T\u0304) + stream + year"

g1 <- bind_rows(
  perf_row(fm(FIN),                                         "Reach + reach-year intercepts"),
  perf_row(fm(FIN, "(1 | COMID)"),                          "Reach intercept only"),
  perf_row(fm(FIN, "(1 + T_dev | COMID) + (1 | key)"),      "Reach + reach-year intercepts, random slopes for (T \u2212 T\u0304)")
) |> mutate(Group = paste0("1. Random-effect structure (fixed effects: ", LAB_FINAL, ")"))

g2 <- bind_rows(
  perf_row(fm(FIN),                              "T\u0304 + (T \u2212 T\u0304)"),
  perf_row(fm("T + stream + year"),              "T (not split)"),
  perf_row(fm("T_bar + stream + year"),          "T\u0304 only"),
  perf_row(fm("T_dev + stream + year"),          "(T \u2212 T\u0304) only"),
  perf_row(fm("stream + year"),                  "No temperature")
) |> mutate(Group = "2. Temperature terms (with stream + year)")

g3 <- bind_rows(
  perf_row(fm(FIN),                              LAB_FINAL),
  perf_row(fm("T_bar + T_dev + year"),           "\u2212 stream"),
  perf_row(fm("T_bar + T_dev + stream"),         "\u2212 year"),
  perf_row(fm(paste(FIN, "+ E")),                "+ elevation"),
  perf_row(fm(paste(FIN, "+ slope_s")),          "+ reach slope")
) |> mutate(Group = "3. Other fixed effects (relative to the final model)")

g4 <- bind_rows(
  perf_row(fm(FIN),                                       "Linear T\u0304"),
  perf_row(fm("ns(T_bar, 3) + T_dev + stream + year"),   "Natural spline on T\u0304 (df = 3)")
) |> mutate(Group = "4. Shape of the between-reach temperature effect")

tbl1 <- bind_rows(g1, g2, g3, g4) |>
  group_by(Group) |>
  mutate(dAIC = AIC - AIC[1]) |>                      # first row of each group = reference
  ungroup() |>
  mutate(Notes = case_when(
    singular & grepl("random slopes", Model) ~ "Singular fit; not supported",
    Model == "+ elevation" ~ "Driven by Big Creek; not supported without it (Appendix A6)",
    grepl("^Natural spline", Model) ~ "Slight curvature; linear retained (Appendix A4)",
    singular ~ "Singular fit (a variance component estimated at 0)",
    row_number() == 1 | Model %in% c(LAB_FINAL, "Linear T\u0304", "T\u0304 + (T \u2212 T\u0304)") ~ "Final model",
    TRUE ~ "")) |>
  select(Group, Model, k, dAIC, RMSE, R2_m, R2_c, Notes)
print(tbl1, n = 30, width = Inf)
write_csv(tbl1, file.path(out_tab, "Table_01_rev.csv"))

# -----------------------------------------------------------------------------
# Table 2: final model (REML); T_bar centred at the average reach temperature
# -----------------------------------------------------------------------------
final <- lmer(yday ~ T_bar_c + T_dev + stream + year + (1 | COMID) + (1 | key), d, REML = TRUE, control = ctl)
ci <- confint(final, method = "Wald", parm = "beta_")
fe <- tibble(term = names(fixef(final)), Estimate = unname(fixef(final)),
             SE = sqrt(diag(vcov(final)))) |>
  mutate(CI_low = ci[term, 1], CI_high = ci[term, 2],
         p = 2 * pnorm(-abs(Estimate / SE)),
         Parameter = case_when(
           term == "(Intercept)"   ~ "Intercept (Bear Valley, 2002, average reach)",
           term == "T_bar_c"       ~ "Reach mean thermal index, T\u0304 (d per \u00b0C)",
           term == "T_dev"         ~ "Reach-year anomaly, T \u2212 T\u0304 (d per \u00b0C)",
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
                "N reach-years", "N reaches (COMID)", "N redds"),
  Estimate = c(vc$sdcor[vc$grp == "key"], vc$sdcor[vc$grp == "COMID"], vc$sdcor[vc$grp == "Residual"],
               nlevels(d$key), nlevels(d$COMID), nrow(d)),
  CI = NA_character_, p = NA_character_)
# ICC is a summary of fit, not a random effect (R2 comment on Table 2)
mp <- tibble(
  Section = "Model performance",
  Parameter = c("R² marginal", "R² conditional", "ICC (adjusted)", "RMSE (days)"),
  Estimate = c(r2$R2_marginal, r2$R2_conditional, performance::icc(final)$ICC_adjusted,
               sqrt(mean(residuals(final)^2))),
  CI = NA_character_, p = NA_character_)

tbl2 <- bind_rows(fe, re, mp)
print(tbl2, n = 30, width = Inf)
write_csv(tbl2, file.path(out_tab, "Table_02_rev.csv"))

# -----------------------------------------------------------------------------
# Word-ready versions: rounded values, group headings as their own rows, "--"
# for empty cells, saved with a UTF-8 byte-order mark (write_excel_csv) so
# Excel shows T-bar, minus signs and superscripts correctly. Open in Excel and
# paste into Word, or use the .docx made below when gt is installed.
# -----------------------------------------------------------------------------
with_group_rows <- function(df, group) {
  df |>
    group_by(.data[[group]]) |>
    group_modify(\(x, k) bind_rows(tibble(Model = k[[1]]), x)) |>   # heading row, then members
    ungroup() |>
    select(-all_of(group))
}
tbl1_fmt <- tbl1 |>
  mutate(Group = str_remove(Group, "^\\d\\. ") |>
           str_replace("\\(fixed effects: .*\\)$", "(fixed effects as in the final model)"),
         k = as.character(k),
         dAIC = sprintf("%.1f", dAIC), RMSE = sprintf("%.2f", RMSE),
         R2_m = sprintf("%.3f", R2_m), R2_c = ifelse(is.na(R2_c), "--", sprintf("%.3f", R2_c))) |>
  mutate(Group = factor(Group, levels = unique(Group))) |>
  with_group_rows("Group") |>
  mutate(across(everything(), \(x) replace_na(x, ""))) |>
  rename(`ΔAIC` = dAIC, `RMSE (days)` = RMSE, `R² marginal` = R2_m, `R² conditional` = R2_c)
write_excel_csv(tbl1_fmt, file.path(out_tab, "Table_01_rev_formatted.csv"))

tbl2_fmt <- tbl2 |>
  mutate(Estimate = ifelse(grepl("^N ", Parameter), sprintf("%.0f", Estimate), sprintf("%.2f", Estimate)),
         CI = replace_na(CI, "--"), p = replace_na(p, "--"),
         Section = factor(Section, levels = unique(Section))) |>
  rename(Model = Parameter) |>                       # with_group_rows() writes headings into Model
  with_group_rows("Section") |>
  mutate(across(everything(), \(x) replace_na(x, ""))) |>
  rename(Parameter = Model, `95% CI` = CI)
write_excel_csv(tbl2_fmt, file.path(out_tab, "Table_02_rev_formatted.csv"))

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
      "Model: spawn day of year ~ T̄ + (T − T̄) + stream + year + (1 | reach) + (1 | reach-year), fit by REML. ",
      "T = reach-year thermal index; T̄ = reach mean over years sampled, centred at the average reach (",
      sprintf("%.1f", T_bar_ref), " °C). ",
      "Reference levels: Stream = Bear Valley Creek; Year = 2002. CIs are Wald intervals."))) |>
    tab_header(title = "Table 2. Final model parameter estimates.")
  gtsave(t2, file.path(out_tab, "Table_02_rev.docx"))
  message("Word tables written.")
} else {
  message("gt not installed: CSV tables only.")
}
message("Done.")
