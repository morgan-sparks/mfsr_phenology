# =============================================================================
# 10_manuscript-figures.R
#
# CJFAS revision: rebuild manuscript Figures 2-5 with the revised analysis.
#   Figure 2  spawn-date distributions: all (A), by stream (B), by year (C)
#   Figure 3  cumulative proportion of redds by stream and year
#   Figure 4  partial effects: between-reach (T_bar) and within-reach (T - T_bar)
#             temperature components (replaces the temp_90 figure)
#   Figure 5  observed and temperature-adjusted mean spawn dates by stream (A)
#             and year (B); the stream x year panel (old 5C) is dropped
#             because the final model has no interaction and Fig. 3 already
#             shows stream x year patterns
# Figure 6 (random slopes) is retired; reach intercepts vs network position
# go to the appendix (plots/revision/F4_comid_intercepts_vs_drainage_area.png).
#
# Style follows the first submission (theme_mfsr, cols.streams, cols.years).
# Outputs: plots/revision/Figure_0X_rev.pdf and .png
# =============================================================================

library(lme4)
library(emmeans)
library(patchwork)
library(ggridges)
source(here::here("R", "revision_setup.R"))   # redds, model_data(), FINAL_FORMULA, adjust_stream_year()
emm_options(lmer.df = "asymptotic")
theme_set(theme_mfsr(base_size = 11))

# Save each figure as vector PDF (for submission) and PNG (for review); cm units
save_fig <- function(p, name, width, height) {
  path <- file.path(out_plot, name)
  ggsave(paste0(path, ".pdf"), p, device = cairo_pdf, width = width, height = height, units = "cm")
  ggsave(paste0(path, ".png"), p, width = width, height = height, units = "cm", dpi = 300, bg = "white")
}
fmt_date <- function(doy) format(fixed_date + doy, "%d %b")

spawn <- redds |> mutate(year = factor(year))

# -----------------------------------------------------------------------------
# Figure 2: distributions of spawn dates
# -----------------------------------------------------------------------------
yr_means <- spawn |>
  group_by(year) |>
  summarise(mean = mean(yday))
x_breaks <- as.Date(c("2001-08-01", "2001-08-15", "2001-09-01", "2001-09-15")) # fixed_date + yday is in 2001
stats_lab <- with(spawn, paste0(
  "mean = ", fmt_date(round(mean(yday))), "\n",
  "median = ", fmt_date(median(yday)), "\n",
  "range = ", fmt_date(min(yday)), " – ", fmt_date(max(yday)), "\n",
  "SD = ", sprintf("%.1f", sd(yday)), " d"
))

p2a <- ggplot(spawn, aes(x = fixed_date + yday)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 3, fill = "gray80", color = "black") +
  geom_density(color = "black", linewidth = 1, adjust = 1.5) +
  geom_vline(xintercept = fixed_date + mean(spawn$yday), linetype = "dashed", linewidth = 1) +
  geom_vline(data = yr_means, aes(xintercept = fixed_date + mean, color = year), linewidth = 1) +
  annotate("text",
    x = fixed_date + min(spawn$yday), y = 0.016, hjust = 0, vjust = 0.5,
    size = 3.2, lineheight = 0.95, label = stats_lab
  ) +
  scale_color_manual(values = cols.years) +
  scale_x_date(breaks = x_breaks, date_labels = "%d %b") +
  labs(x = NULL, y = "Density of spawn dates", color = NULL) +
  theme(legend.position = c(0.06, 0.78)) # works in ggplot2 3.4 and 3.5+

p2b <- ggplot(spawn, aes(x = fixed_date + yday, y = fct_rev(stream), fill = stream)) +
  geom_density_ridges(scale = 1.4, linewidth = 0.3) +
  scale_fill_manual(values = cols.streams, guide = "none") +
  scale_x_date(breaks = x_breaks, date_labels = "%d %b") +
  labs(x = NULL, y = NULL)

p2c <- ggplot(spawn, aes(x = fixed_date + yday, y = fct_rev(year), fill = year)) +
  geom_density_ridges(scale = 1.4, linewidth = 0.3) +
  scale_fill_manual(values = cols.years, guide = "none") +
  scale_x_date(breaks = x_breaks, date_labels = "%d %b") +
  labs(x = NULL, y = NULL)

fig2 <- p2a / (p2b + p2c) + plot_annotation(tag_levels = "A") + plot_layout(heights = c(1, 1.2))
save_fig(fig2, "Figure_02_rev", width = 18, height = 15)

# -----------------------------------------------------------------------------
# Figure 3: cumulative proportion of redds by stream and year
# -----------------------------------------------------------------------------
props <- spawn |>
  count(stream, year, yday) |>
  arrange(stream, year, yday) |>
  group_by(stream, year) |>
  mutate(cum_p = cumsum(n) / sum(n)) |>
  ungroup()

fig3 <- ggplot(props, aes(x = fixed_date + yday, y = cum_p, color = year)) +
  facet_wrap(~stream, ncol = 2) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 0.9) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5)) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  scale_color_manual(values = cols.years) +
  labs(x = NULL, y = "Cumulative proportion of redds", color = "Year") +
  theme(panel.spacing = unit(0.8, "lines"))
save_fig(fig3, "Figure_03_rev", width = 17.6, height = 16)

# -----------------------------------------------------------------------------
# Final model (identical to R/09; FINAL_FORMULA in R/revision_setup.R)
# -----------------------------------------------------------------------------
d <- model_data()
final <- lmer(FINAL_FORMULA, d, REML = TRUE, control = ctl)
fe <- fixef(final)

# -----------------------------------------------------------------------------
# Figure 4: partial effects of the two temperature components
#   Spawn dates are adjusted for the stream and year fixed effects (set to
#   their average over levels, matching emmeans), so points show what the
#   model attributes to temperature.
#   (A) Between reaches: reach mean adjusted spawn date vs reach mean index
#       (T_bar); line = population-level fit at T_dev = 0, 95% CI.
#   (B) Within reaches: reach-year anomaly in adjusted spawn date (relative to
#       that reach's own mean) vs the reach-year anomaly in the index
#       (T - T_bar); reaches sampled in >= 2 years; line = within-reach slope.
# -----------------------------------------------------------------------------
d <- adjust_stream_year(d, final) # adds y_adj

# (A) between reaches: one point per reach. The reach's own anomaly effect
#     (b_T_dev * T_dev) is also removed so points reflect T_bar alone.
reach <- d |>
  group_by(COMID, stream) |>
  summarise(
    T_bar = first(T_bar), 
    y = mean(y_adj - fe[["T_dev"]] * T_dev), 
    n = n(), 
    .groups = "drop"
    )
grid_b <- seq(min(d$T_bar), max(d$T_bar), length.out = 60)
pred_b <- as_tibble(emmeans(final, ~T_bar, at = list(T_bar = grid_b, T_dev = 0)))

p4a <- ggplot() +
  geom_ribbon(
    data = pred_b, aes(T_bar, ymin = fixed_date + asymp.LCL, ymax = fixed_date + asymp.UCL),
    fill = "grey50", alpha = 0.3
  ) +
  geom_line(data = pred_b, aes(T_bar, fixed_date + emmean), linewidth = 1) +
  geom_point(data = reach, aes(T_bar, fixed_date + y, color = stream, size = n), alpha = 0.8) +
  scale_color_manual(values = cols.streams) +
  scale_size_area(max_size = 5, breaks = c(10, 100, 200)) +
  scale_y_date(date_breaks = "1 week", date_labels = "%d %b") +
  labs(
    x = "Reach mean thermal index (\u00B0C)", y = "Adjusted spawn date",
    color = "Stream", size = "Redds", subtitle = "Between reaches"
  ) +
  guides(color = guide_legend(override.aes = list(size = 2.5), order = 1))

# (B) within reaches, between years: one point per reach-year, for reaches
#     surveyed in >= 2 years. y_anom = reach-year mean adjusted spawn date
#     minus that reach's mean over its reach-years. The line and its CI are
#     b_T_dev * T_dev (it passes through 0 by construction).
multi <- d |>
  group_by(COMID) |>
  filter(n_distinct(year) >= 2) |>
  ungroup()
ryw <- multi |>
  group_by(COMID, stream, key) |>
  summarise(T_dev = first(T_dev), y = mean(y_adj), n = n(), .groups = "drop") |>
  group_by(COMID) |>
  mutate(y_anom = y - mean(y)) |>
  ungroup()
b_dev <- fe[["T_dev"]]
se_dev <- sqrt(vcov(final)["T_dev", "T_dev"])
grid_w <- tibble(
  T_dev = seq(min(ryw$T_dev), max(ryw$T_dev), length.out = 50)) |>
  mutate(
    fit = b_dev * T_dev, 
    lo = (b_dev - 1.96 * se_dev) * T_dev, 
    hi = (b_dev + 1.96 * se_dev) * T_dev)

p4b <- ggplot() +
  geom_hline(yintercept = 0, linetype = 3) +
  geom_vline(xintercept = 0, linetype = 3) +
  geom_ribbon(data = grid_w, aes(T_dev, ymin = pmin(lo, hi), ymax = pmax(lo, hi)), fill = "grey50", alpha = 0.3) +
  geom_line(data = grid_w, aes(T_dev, fit), linewidth = 1) +
  geom_point(data = ryw, aes(T_dev, y_anom, color = stream, size = n), alpha = 0.8) +
  scale_color_manual(values = cols.streams, guide = "none") +
  scale_size_area(max_size = 5, guide = "none") +
  labs(
    x = "Reach-year anomaly in thermal index (\u00B0C)",
    y = "Spawn-date anomaly (days)", subtitle = "Within reaches, between years"
  )

fig4 <- p4a + p4b + plot_layout(guides = "collect") + plot_annotation(tag_levels = "A")
save_fig(fig4, "Figure_04_rev", width = 20, height = 10)

# -----------------------------------------------------------------------------
# Figure 5: observed vs temperature-adjusted means by stream (A) and year (B)
#   grey jitter: individual redds; open diamond: observed mean;
#   black point + bar: model-adjusted marginal mean (at mean T_bar, T_dev ~ 0) with 95% CI
# -----------------------------------------------------------------------------
emm_s <- as_tibble(emmeans(final, ~stream))
emm_y <- as_tibble(emmeans(final, ~year))
obs_s <- d |>
  group_by(stream) |>
  summarise(mean = mean(yday))
obs_y <- d |>
  group_by(year) |>
  summarise(mean = mean(yday))

panel5 <- function(x, emm, obs, cols) {
  ggplot(d, aes(x = .data[[x]], y = fixed_date + yday)) +
    geom_jitter(aes(color = .data[[x]]), width = 0.25, height = 0.3, size = 0.6, alpha = 0.35) +
    geom_point(
      data = obs, aes(y = fixed_date + mean), shape = 23, size = 2.8,
      fill = "white", color = "black", position = position_nudge(x = -0.28)
    ) +
    geom_pointrange(
      data = emm, aes(
        y = fixed_date + emmean, ymin = fixed_date + asymp.LCL,
        ymax = fixed_date + asymp.UCL
      ),
      size = 0.45, linewidth = 0.8, color = "black", position = position_nudge(x = 0.28)
    ) +
    scale_color_manual(values = cols, guide = "none") +
    scale_y_date(date_breaks = "2 weeks", date_labels = "%d %b") +
    labs(x = NULL, y = "Spawn date")
}
p5a <- panel5("stream", emm_s, obs_s, cols.streams) +
  theme(axis.text.x = element_text(angle = 35, hjust = 1))
p5b <- panel5("year", emm_y, obs_y, cols.years) + labs(y = NULL)
fig5 <- p5a + p5b + plot_layout(widths = c(2, 1)) + plot_annotation(tag_levels = "A")
save_fig(fig5, "Figure_05_rev", width = 18, height = 9.5)

# Numbers for captions / text
message(sprintf("Fig 2: n = %d redds; mean %s; median %s; range %s-%s; SD %.1f d",
                nrow(spawn), fmt_date(round(mean(spawn$yday))), fmt_date(median(spawn$yday)),
                fmt_date(min(spawn$yday)), fmt_date(max(spawn$yday)), sd(spawn$yday)))
message(sprintf("Fig 4: T_bar range %.1f-%.1f C; between %.2f d/C; within %.2f d/C (SE %.2f); %d multi-year reaches, %d reach-years",
                min(d$T_bar), max(d$T_bar), fe[["T_bar"]], b_dev, se_dev, n_distinct(ryw$COMID), nrow(ryw)))
