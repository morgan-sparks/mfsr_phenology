# CJFAS Phenology Revision — Review Analysis & Task List

Bryan M. Maitland · 30 Oct 2026 (updated 2 Nov 2026)

> Snapshot of the shared doc "CJFAS Phenology Revision — Review Analysis & Task List" (Claude Docs), which is the live version. Codes in square brackets (D1–F4) refer to outputs from `R/07`–`R/09`; see [Revision outputs map](#revision-outputs-map).

Reviewer 1 is right, and it goes deeper than either of you suspected. Our temperature covariate is computed over the 90 days *ending on each redd's own spawn date*, so a later spawn date mechanically produces a warmer temp_90. Shuffled, biologically meaningless spawn dates reproduce the within-reach "thermal response" exactly. The core among-reach result does survive a date-independent temperature metric, so the paper is salvageable. But this is a re-analysis, not a response-letter fix.

## Reviewer 1 decoded: the covariate contains the response

The point is not "temperatures decline in fall, so of course." It is that our predictor is built from the response. In `R/04_comid-temperature.R`, temp_90 is the mean of the 90 days ending on each redd's inferred spawn date. Spawn date sits on both sides of the model, so any seasonal trend in stream temperature creates a correlation even if fish ignore temperature entirely. That is R1's "temporal correlation," and the AE explicitly asks us to show the association is not driven by it.

The artefact runs in the *same direction* as our result, opposite to R1's guess. Network-mean stream temperature peaks about 24 July (14.6 °C across study reaches, DOY 205; D1), and every redd falls after that (DOY 206–263). Sliding a 90-day window later drops cold early-June days (6–9 °C) and adds warm August days (12–14 °C), so later spawning means a warmer temp_90.

I reproduced temp_90 exactly from the repo data (`model_data_final.rds` + Siegel daily temps; r = 1.000) and ran four checks:

| Check | Result | What it means |
| --- | --- | --- |
| Same reach, same year: temp_90 vs spawn date [D2] | +0.73 °C per 10 d later, r = 0.96 | Within a reach-year, temp_90 is essentially a re-coding of the date |
| Null: spawn dates shuffled within year, temp_90 recomputed [D3; 200 runs in R] | R² (yday ~ temp_90 quad) = 0.30 vs 0.71 real; with stream + year, 0.47 vs 0.84 | About 40–55% of the headline fit is produced with zero biology |
| Within-reach slope of spawn date on temp_90 [D3] | Null 13.5 d/°C; real 12.6 d/°C | The within-reach "thermal response" is indistinguishable from the artefact |
| Per-COMID within-reach slope vs 1/(rate the window warms at that reach), 37 COMIDs [D4] | r = 0.75; medians 13.1 vs 13.1 d/°C | Random slopes ("thermal sensitivity", Fig. 6) mostly measure each reach's thermograph shape |
| Window length: within-reach change per 10 d later [D2] | 30-d: −0.58 °C; 60-d: +0.34 °C; 90-d: +0.73 °C | Raw r with spawn date: 0.08, 0.65, 0.82. The 90-d window "won" because its artefact aligns with the spatial pattern |

Consequences for the current paper:

- Marginal/conditional R² (0.60/0.99), RMSE 1.7 d and the large ΔAIC for random slopes (510) are inflated by the circularity. A mixed model with temp_90 has AIC 13,541; the same model with a fixed-date temperature has 18,290.
- R1's specific prediction is confirmed: reach-level slope differences track how fast the window warms at each reach (their Ln 245 point).
- The random slopes, the ρ = −0.83 intercept–slope correlation, Fig. 6, and the Discussion paragraph interpreting them cannot stand as biology.
- The "latent effect" and the dismissal of fixed-date and post-spawn metrics (Ln 194, 334–336) are explained by this artefact, not by biology.

## What survives, and the re-analysis to run

The main finding holds with a temperature metric that cannot see spawn date: cooler reaches spawn earlier. At the reach-year level (262 COMID-years, weighted by redds), mean spawn date regressed on temperature metrics fixed in calendar time:

| Temperature metric (per COMID-year) | r with mean spawn date | Slope alone (d/°C) | Slope with stream + year (p) | R² with stream + year |
| --- | --- | --- | --- | --- |
| temp_90, current (date-anchored) | 0.77 | +4.6 | +4.5 (1e-81) | 0.90 |
| 90-d mean ending 24 Jul (before any redd) | 0.66 | +6.4 | +6.3 (9e-44) | 0.79 |
| 30-d mean ending 1 Aug | 0.54 | +4.2 | +4.6 (4e-47) | 0.80 |
| Mean 1 Jul–31 Aug | 0.57 | +4.5 | +4.8 (2e-49) | 0.81 |
| Mean 3 Aug–30 Sep | 0.55 | +4.6 | +5.4 (8e-52) | 0.82 |

The among-reach slope is about 4.5–6 days per °C, robust to the metric. (This was a first-pass Python screen; E1 and F1–F2 below supersede it.) That is the honest effect size; the mixed model's 11 d/°C was inflated. This is also the direct answer to the AE: a pre-spawn, fixed-date thermal index still explains about half the among-reach variance on its own.

Recommended re-analysis (needs your R environment; I could only approximate lme4 in Python here):

1. Replace temp_90 with a reach-year thermal index fixed in calendar time. The 90 days ending 24 July is pre-spawn, includes the adult holding period, and wins among the clean metrics. Alternatively use a reach-level thermal regime summary such as mean Jul–Aug temperature or degree days to a fixed date.
2. Refit `yday ~ ns(T_fixed) + stream + year + (1|COMID)`. Drop random slopes: with a reach-year covariate there is no within-reach-year temperature variation to estimate them from.
3. Optionally decompose temperature into a reach mean and a reach-year anomaly (within–between, Mundlak). This separates spatial thermal regime from interannual thermal variation, which is what the title claims.
4. Report the shuffled-date null in an appendix. It shows reviewers we found and quantified the problem.
5. Rebuild Table 1/2 and Figs 4–6. Rewrite the Abstract, Results and Discussion around the corrected effect size. Retire the "thermal sensitivity" and "latent effect" narratives.
6. Soften causal language throughout ("associated with"), and state the alternative R1 raises. Spatial temperature co-varies with elevation and with upstream position/migration distance, so observational data cannot isolate temperature. Consider a title change, e.g. "… is associated with a diverse Chinook salmon spawning portfolio."

Expect a lower fit but a defensible one: conditional R² will drop, but every remaining number will survive R1's objection. The current numbers would not.

## Reviewer 2 (Tim Copeland): he understood it fine

Tim followed the random effects well. His third general point independently found the same flaw as R1, from biology rather than statistics: fish weren't in these reaches for 90 days, and the plausible 30- and 60-day windows don't work. The artefact explains exactly why (the window-length row above). Treat his comments as constructive and specific. The draft responses currently decline many of them, and the AE sided with him on trimming the Discussion.

| Placeholder (Russ's response doc) | Suggested answer |
| --- | --- |
| General 1 + Ln 166–168: what is a COMID | Define early in Methods as an NHDPlus V2 flowline segment, the unit Siegel et al. (2023) predict temperature for. Our 104 segments are 0.07–3.2 km long (median 1.0 km, mean 1.1 km; from `flowline_pn17_nsi_MFSR.gpkg`). Use "reach" in the text and mention COMID once. |
| General 1 + Ln 202–203: COMID not nested in stream | It is nested. Every COMID ID is unique and belongs to one stream, so a COMID random intercept is equivalent to a stream:COMID intercept. Stream is fixed, and reach deviations are estimated around each stream's mean. Say this in one sentence. Reaches are median 0.95 km long (F4). |
| General 1 + 2, Ln 314–317: up/downstream structure in COMID effects | Fair and testable. Plot reach random intercepts against network position (NHDPlus Hydroseq or distance from mouth, available in the GIS layer) within stream. Or fit an SSN tail-up term, which the Discussion already proposes. At minimum, show the plot in an appendix. |
| General 2 + Ln 242: what "stream" represents | A drainage-level grouping: shared migration route length, hydrologic and thermal context, and survey segment. Not a demographic unit (concede Ln 217, 310). Note that disjunct segments (e.g., upper vs lower Bear Valley around Elk Cr.) are held in by the COMID level. |
| General 3 + Ln 334–336: mechanism for 90-day window | Concede. The 90-day antecedent mean partly encodes spawn date itself, and that artefact is why it outperformed 30/60-d windows. Replace it with a fixed-date pre-spawn index (see above). Thank Tim; this is the same issue R1 raised. |
| Ln 170–171 | Replace with a description of the redd distribution (Fig. A1.1): e.g., redds per reach-year median 6; 27 of 103 reaches in the repo data sampled in all 4 years. |
| Ln 230–231: year confounded with abundance | Agree. Four years and one abundance peak (2003) cannot separate year, temperature and escapement. Say so; drop "trend toward later spawning" (year means span only 26 Aug–1 Sep). |
| Ln 412–413 | Russ's text is fine; add that 2005 also lacked Sulphur and Big Cr. data, as Tim notes. |
| Table 2: significant stream effects in short segments; ICC listed as random effect | Stream coefficients are conditional on temperature and are relative to Bear Valley. Moot after the refit; move ICC to model performance. |
| Fig. 4: lines are stream-level predictions | Tim is right on wording: the caption says "population-level" but the lines are per stream × year. Change to "stream-level predictions (fixed effects, random effects at zero)". This figure gets rebuilt anyway. |
| Fig. 5C: omit lines | Agree (Russ agrees too). Use points with CIs only; categories on the x-axis are not connected. |

Two of Russ's drafts may draw pushback:

- **Tonina citation:** Russ's response calls it "Tonina et al. 2009" and says its conclusions are unsupported. Tim's citation is GRL vol. 49, e2022GL098552, which is a 2022 paper. Correct the year. A neutral response ("we cite Jacobs et al. 2021, 2022, which model the Middle Fork specifically") avoids critiquing a published paper to a reviewer.
- **Obedzinski and "esoteric":** Declining to cite one paper is fine. Rebuffing the "esoteric" comment is risky, though, because the AE explicitly asked for a tighter Discussion.

## Problems in the current revision files

These are independent of the re-analysis; fix them regardless.

| Where | Problem | Fix |
| --- | --- | --- |
| Text, Davis (2015) sentence | Revision inserted "Sockeye Salmon (*Oncorhynchus tshawytscha*)" | *O. nerka* |
| Text, Methods covariates (R1 Ln 174–178) | Says development "accelerates disproportionately in cold water" and that cooler sites spawn earlier because of gonadal maturation. R1 is right that this is muddled. | State the actual logic: cold incubation needs more days to reach emergence, so cold sites spawn earlier (Brannon et al. 2004; Beacham & Murray 1990). Revise; don't answer "correct as written." |
| Text vs Discussion | Methods motivates the incubation hypothesis; Discussion says our data don't support it | Pick one framing after the refit (fixed-date pre-spawn metric may change this) |
| Results, Observed spawn timing | "year-specific means varied by nearly three weeks" | Raw year means span 26 Aug–1 Sep (~6 d) |
| Abstract | "18-day range (August 20 to September 7)" | That is the range among stream means (raw: Sulphur 19 Aug to Loon 6 Sep). Say "among stream means". Stream × year means span 16 Aug–10 Sep (24 d). Answers Tim's Ln 26 question. |
| Text, Ln 132–133 edit | Still says populations "at high risk of extirpation", Tim's exact concern | Frame as NMFS 2017 viability risk rating, separate from ESA "Threatened" status |
| References | Hauck 1951 entry incomplete ("Available from" blank); Pess 2009 not in list; response says "inserted Pess 2009" but text cites "Pess (2009) and Pess et al. (2008)" | Complete Hauck; add Pess 2009 dissertation or Pess et al. 2014 CJFAS per Tim |
| Footnotes | Footnotes 2 and 3 duplicate the Buffington et al. in-review citation with stray "^3^"; footnote 2 is attached to the hyporheic sentence | Clean up; one note per citation |
| Data vs text | Repo `model_data_final.rds` has 3,013 redds, 103 COMIDs; manuscript says 3,016 and 104 | Confirm which version produced the tables |
| Table 1 | Caption says "four decision points … interaction terms"; table and Methods have three stages. m7 marginal R² is 0.729 in group 1 vs 0.714 in group 2. | Align caption; note ML vs REML |
| Table 2 footnote | "see Figure 5 for the estimated temperature–spawn timing relationship" | Figure 4 |
| Table 2 | ICC under random effects | Move to model performance (Tim) |
| Fig. 3 | Y-axis label typo "ccumulative" | "cumulative" |
| Fig. 4 caption | "population-level model predictions" but lines are stream × year | See Tim row above |
| Discussion structure | Limitations moved mid-Discussion (good); 1879 Sheepeater campaign anecdote and the Ln 327–341 incubation paragraph remain | AE + both reviewers asked for trimming. Cut R1's Ln 327–341 paragraph (it collapses anyway with the latent-effect claim). Shorten historical anecdotes to one sentence. |
| Response doc tone | Several bare "We disagree" (R1 Ln 47, 132–147, 178; R2 Results, Discussion, Ln 389, 391–392) | Keep the substance; add one sentence of reasoning plus a small concession where possible. The AE is weighing R1 heavily. |

## Re-analysis results (R, CJFAS-revisions branch)

The corrected model gives spawning about 5.0 days later per °C of reach-year thermal index (95% CI 4.3–5.8). Scripts are `R/revision_setup.R` and `R/07`–`09`; outputs are in `tables/revision/` and `plots/revision/`.

| Result | Value | Source |
| --- | --- | --- |
| Shuffled-date null (200 runs), within-reach slope | 13.5 d/°C null vs 12.6 observed | D3 |
| Shuffled-date null, mixed-model residual SD | 2.4 d null vs 2.0 observed | D3 |
| Per-reach slope vs artefact-only slope (37 COMIDs) | r = 0.75, medians 13.1 vs 13.1 | D4 |
| Fixed-date indices tested | 32; slope 3.8–6.1 d/°C and marginal R² 0.52–0.55 for all (with reach-year RE) | E1 |
| Best-fitting indices | none stands out: all 32 within 22 AIC once the reach-year RE is included (93 without it). August-ending windows rank slightly higher; pre-spawn windows are 10–22 AIC back | E1 |
| Reach-year random intercept (1\|COMID:year) | ΔAIC ≈ −425; required, since the index is a reach-year value | F1 |
| Random slopes (with reach-year RE) | singular (ρ = −1): drop | F1 |
| Spline vs linear | no gain (ΔAIC < 1): use linear; the warm-site plateau does not hold | F1 |
| Final model | T + stream + year + (1\|COMID) + (1\|COMID:year); R²m 0.52, R²c 0.71, RMSE 4.2 d | F2 |
| Between-reach vs within-reach (interannual) | 4.9 vs 7.2 d/°C, same sign | F1 |
| Stream means (emmeans) | 26 Aug (Elk) to 3 Sep (Loon, Marsh) | F3 |
| Year means | 28 Aug (2002) to 2 Sep (2005) | F3 |
| COMID length | median 0.95 km (0.07–3.21) | F4 |

Open decision: the index. The data cannot choose the window (E1), so pick it on biological grounds. A window ending before the first redd (25 Jul) is the easiest to defend, because no temperature after any fish spawned enters it. The script default is the 60-d mean ending 16 Jul (18 May–16 Jul; slope 5.0 d/°C). Describe the index as a reach thermal-regime signature rather than an exposure cue.

## Revision outputs map

Each code used in this doc points to the files below in the CJFAS-revisions branch. All are regenerated by running the scripts in order; `R/revision_setup.R` holds the shared data loading and window function.

| Code | Script | Output files | What it shows | Answers |
| --- | --- | --- | --- | --- |
| D1 | R/07 | plots/revision/D1_seasonal_thermograph.png | Network daily thermograph; all spawning falls after the 24 Jul thermal peak | R1 main; AE |
| D2 | R/07 | tables/revision/D2_within_reach_window_change.csv | Change in 30/60/90-d antecedent temperature per day of later spawning, same reach-year; share of variance within reach-year | R1 main; R2 general 3 (why the 90-d window won) |
| D3 | R/07 | tables/revision/D3_shuffled_date_null.csv, D3_shuffled_date_null_runs.csv; plots/revision/D3_shuffled_date_null.png | 200 shuffled-date nulls vs observed: R², within-reach slope, mixed-model residual SD | R1 main; AE request for an analysis |
| D4 | R/07 | tables/revision/D4_comid_slope_vs_artefact.csv; plots/revision/D4_comid_slope_vs_artefact.png | Per-COMID observed slope vs artefact-only slope (1 / window warming rate) | R1 Ln 245; current Fig. 6 |
| E1 | R/08 | tables/revision/E1_fixed_date_candidates.csv; plots/revision/E1_fixed_date_candidates.png | 32 date-independent indices fit with COMID and reach-year random intercepts: ΔAIC, slope, marginal R², reach-year r; AIC without the reach-year term kept for comparison | AE; R1 Ln 194; R2 general 3 |
| F1 | R/09 | tables/revision/F1_model_comparison_fixed_index.csv | Model comparison: reach-year RE, random slopes, spline, quadratic, within–between; submitted model as reference | New Table 1; R1 plasticity vs divergence |
| F2 | R/09 | tables/revision/F2_final_model_fixed_index.csv | Final model coefficients, variance components, R², ICC, RMSE | New Table 2; R2 Table 2 (ICC) |
| F3 | R/09 | tables/revision/F3_emmeans_fixed_index.csv, F3_contrasts_fixed_index.csv | Stream and year marginal means with Tukey contrasts | New Fig. 5A–B; Abstract date range; R2 Ln 26 |
| F4 | R/09 | tables/revision/F4_comid_intercepts_vs_position.csv, F4_comid_random_intercepts.csv; plots/revision/F_comid_intercepts_vs_drainage_area.png | COMID intercepts vs drainage area and elevation within stream; COMID lengths | R2 general 1–2; Ln 166–168, 314–317 |
| F | R/09 | plots/revision/F_temperature_response_fixed_index.png | Reach-year mean spawn date vs index, population-level fit with 95% CI | New Fig. 4 |

## Task list

**Now**

- [ ] Bryan + Russ (+ Morgan): agree that temp_90 is circular and that we re-analyse rather than rebut. Share this doc.

**Re-analysis (Bryan, Morgan)**

- [x] Reproduce the diagnostics in R: within-reach temp_90 vs date, the shuffled-date null, and the 30/60/90-d window comparison. Done: R/07_window-artefact-diagnostics.R.
- [ ] Choose a fixed-date thermal index. Sensitivity done (E1): the data cannot distinguish windows (all within 22 AIC; slopes 3.8–6.1 d/°C). Choose on biology; script default is the pre-spawn 60-d mean ending 16 Jul. Bryan to decide.
- [x] Refit with the fixed-date index. Draft done in R/09 (F1–F3): linear T + stream + year with COMID and reach-year random intercepts; within–between split included. Rerun once the index is chosen.
- [x] Reconcile n = 3,013 vs 3,016 redds and 103 vs 104 COMIDs. Resolved: the analysis/_shared.R pipeline gives 3,016 and 104, matching the manuscript; model_data_final.rds is a stale 3,013-row copy.
- [x] Plot COMID random intercepts against network position (Hydroseq/distance upstream) within stream for Tim. Consider an SSN tail-up check. Done with drainage area (R/09, F4): no consistent pattern except Big Cr.
- [ ] New appendix: covariate-construction artefact + null test + fixed-date results.
- [ ] Rebuild Tables 1–2 and Figs 4–6. Fig. 6 likely becomes reach intercepts only, or goes. Fig. 5C: points + CIs, no lines. Fix the Fig. 3 axis typo.

**Text (Russ drafts, Bryan edits)**

- [ ] Rewrite Abstract, Results and Discussion around the corrected effect size (about 5 d/°C; F2).
- [ ] Remove the thermal-sensitivity, intercept–slope and latent-effect claims.
- [ ] Soften causal language. Add R1's alternatives (elevation, migration distance, stock structure). Reconsider the title verb "structures".
- [ ] Define reach/COMID early with lengths. Explain the nesting and the meaning of the stream effect.
- [ ] Rewrite the gonadal/incubation rationale in Methods (R1 Ln 174–178). Cut the Ln 327–341 paragraph (R1).
- [ ] Trim the Discussion per AE: shorten historical anecdotes and the consumer-portfolio passage.
- [ ] Fix the errors in the table above: *O. nerka*, "three weeks", the 18-day range wording, extirpation wording, Hauck/Pess references, footnotes, Table 2 footnote and ICC, Table 1 caption.

**Response letter (Bryan drafts the stats answers)**

- [ ] Open with a direct reply to the AE/R1. Thank R1, state what we found (the window is anchored to the spawn date), show the null test, and present the corrected analysis.
- [ ] Fill each "Bryan?" placeholder using the Reviewer 2 table above.
- [ ] Soften the bare "We disagree" responses. Correct the Tonina year (2022) and neutralize that reply.
- [ ] Add line numbers for every change (required by CJFAS). Upload clean + tracked versions.

**Final check**

- [ ] Independent read of revised numbers against model output (Dan or John as fresh eyes).
