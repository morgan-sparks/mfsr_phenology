# =============================================================================
# 10_manuscript-figures.R
#
# CJFAS revision: rebuild manuscript Figures 2-5 with the revised analysis.
#   Figure 2  spawn-date distributions: all (A), by stream (B), by year (C)
#   Figure 3  cumulative proportion of redds by stream and year
#   Figure 4  spawn date vs reach-year thermal index (replaces temp_90 figure)
#   Figure 5  observed and temperature-adjusted mean spawn dates by stream (A)
#             and year (B); the stream x year panel (old 5C) is dropped
#             because the final model has no interaction and Fig. 3 already
#             shows stream x year patterns
# Figure 6 (random slopes) is retired; reach intercepts vs network position
# go to the appendix (plots/revision/F_comid_intercepts_vs_drainage_area.png).
#
# Style follows the first submission (theme_mfsr, cols.streams, cols.years).
# Outputs: plots/revision/Figure_0X_rev.pdf and .png
# =============================================================================

library(lme4)
library(emmeans)
library(patchwork)
library(ggridges)
source(here::here("R", "revision_setup.R"))   # redds, model_data(), index, theme
emm_options(lmer.df = "asymptotic")
theme_set(theme_mfsr(base_size = 11))

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
yr_means <- spawn |> group_by(year) |> summarise(mean = mean(yday))
x_breaks <- as.Date(c("2001-08-01", "2001-08-15", "2001-09-01", "2001-09-15"))  # fixed_date + yday is in 2001
stats_lab <- with(spawn, paste0(
  "mean = ", fmt_date(round(mean(yday))), "\n",
  "median = ", fmt_date(median(yday)), "\n",
  "range = ", fmt_date(min(yday)), " – ", fmt_date(max(yday)), "\n",
  "SD = ", sprintf("%.1f", sd(yday)), " d"))

p2a <- ggplot(spawn, aes(x = fixed_date + yday)) +
  geom_histogram(aes(y = after_stat(density)), binwidth = 3, fill = "gray80", color = "black") +
  geom_density(color = "black", linewidth = 1, adjust = 1.5) +
  geom_vline(xintercept = fixed_date + mean(spawn$yday), linetype = "dashed", linewidth = 1) +
  geom_vline(data = yr_means, aes(xintercept = fixed_date + mean, color = year), linewidth = 1) +
  annotate("text", x = fixed_date + min(spawn$yday), y = 0.016, hjust = 0, vjust = 0.5,
           size = 3.2, lineheight = 0.95, label = stats_lab) +
  scale_color_manual(values = cols.years) +
  scale_x_date(breaks = x_breaks, date_labels = "%d %b") +
  labs(x = NULL, y = "Density of spawn dates", color = NULL) +
  theme(legend.position = c(0.06, 0.78))   # works in ggplot2 3.4 and 3.5+

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
# Final model (identical to R/09)
# -----------------------------------------------------------------------------
d <- model_data()
final <- lmer(yday ~ T + stream + year + (1 | COMID) + (1 | key), d, REML = TRUE,
              control = lmerControl(optimizer = "bobyqa"))

# -----------------------------------------------------------------------------
# Figure 4: spawn date vs reach-year thermal index
# -----------------------------------------------------------------------------
grid_T <- seq(min(d$T), max(d$T), length.out = 60)
pred <- as_tibble(emmeans(final, ~ T, at = list(T = grid_T)))
ry <- d |>
  group_by(key, stream, year) |>
  summarise(T = first(T), yday = mean(yday), n = n(), .groups = "drop")

fig4 <- ggplot() +
  geom_point(data = ry, aes(T, fixed_date + yday, color = stream, size = n), alpha = 0.75) +
  geom_ribbon(data = pred, aes(T, ymin = fixed_date + asymp.LCL, ymax = fixed_date + asymp.UCL),
              fill = "grey50", alpha = 0.3) +
  geom_line(data = pred, aes(T, fixed_date + emmean), linewidth = 1) +
  scale_color_manual(values = cols.streams) +
  scale_size_area(max_size = 5, breaks = c(10, 50, 100)) +
  scale_y_date(date_breaks = "1 week", date_labels = "%d %b") +
  labs(x = paste0("Reach-year thermal index (°C; ", INDEX_N, "-d mean, ",
                  doy_lab(INDEX_END_DOY - INDEX_N + 1), "–", doy_lab(INDEX_END_DOY), ")"),
       y = "Spawn date", color = "Stream", size = "Redds") +
  guides(color = guide_legend(override.aes = list(size = 2.5), order = 1))
save_fig(fig4, "Figure_04_rev", width = 17.6, height = 11)

# -----------------------------------------------------------------------------
# Figure 5: observed vs temperature-adjusted means by stream (A) and year (B)
#   grey jitter: individual redds; open diamond: observed mean;
#   black point + bar: model-adjusted marginal mean (at mean T) with 95% CI
# -----------------------------------------------------------------------------
emm_s <- as_tibble(emmeans(final, ~ stream))
emm_y <- as_tibble(emmeans(final, ~ year))
obs_s <- d |> group_by(stream) |> summarise(mean = mean(yday))
obs_y <- d |> group_by(year)   |> summarise(mean = mean(yday))

panel5 <- function(x, emm, obs, cols) {
  ggplot(d, aes(x = .data[[x]], y = fixed_date + yday)) +
    geom_jitter(aes(color = .data[[x]]), width = 0.25, height = 0.3, size = 0.6, alpha = 0.35) +
    geom_point(data = obs, aes(y = fixed_date + mean), shape = 23, size = 2.8,
               fill = "white", color = "black", position = position_nudge(x = -0.28)) +
    geom_pointrange(data = emm, aes(y = fixed_date + emmean, ymin = fixed_date + asymp.LCL,
                                    ymax = fixed_date + asymp.UCL),
                    size = 0.45, linewidth = 0.8, color = "black", position = position_nudge(x = 0.28)) +
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
message("Fig 4: index range ", sprintf("%.1f-%.1f C", min(d$T), max(d$T)),
        "; slope ", sprintf("%.2f d/C", fixef(final)[["T"]]))
message("Done.")
