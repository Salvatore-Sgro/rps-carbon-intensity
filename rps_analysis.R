# SETUP

# "did" runs the Callaway & Sant'Anna estimator; "ggplot2" makes the graphs.
# The loop installs each package the first time only.
user_lib <- Sys.getenv("R_LIBS_USER")
if (dir.exists(user_lib)) .libPaths(c(user_lib, .libPaths()))

for (pkg in c("did", "ggplot2")) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg, repos = "https://cloud.r-project.org")
  }
}

suppressPackageStartupMessages({
  library(did)
  library(ggplot2)
})


# show_table() prints a table in the Console with a title above it.
show_table <- function(title, x) {
  cat("\n\n", title, "\n\n", sep = "")
  print(x, row.names = FALSE)
}


# Colours used in every graph: blue for RPS states, grey for never-RPS states.
COL_RPS  <- "#1f77b4"
COL_CTRL <- "grey60"
group_colours <- c("RPS states" = COL_RPS, "Never-RPS states" = COL_CTRL)


# A clean, consistent look for all graphs.
theme_set(theme_minimal(base_size = 13) +
            theme(legend.position = "bottom",
                  plot.title = element_text(face = "bold"),
                  plot.caption = element_text(colour = "grey40", size = 9)))


# Window for event-study graphs (years relative to RPS enactment)
MIN_E <- -10
MAX_E <-  15




# DATA




# Load the dataset

# Each row is one state in one year (51 jurisdictions x 35 years = 1,785 rows).
# The dataset was built by build_panel.py from EIA, LBNL and NOAA data.
# Key columns:
#   co2_per_mwh      CO2 emitted per MWh of electricity generated (metric tons)
#   rps_ever         1 if the state ever adopted an RPS, 0 if never
#   rps_enact_year   the year the state enacted its RPS (blank if never)
#   coal_share, gas_share, ...   share of the state's electricity from each fuel (%)
#   sample_preferred 1 if the state is in our main sample (see below)

full <- read.csv("rps_panel_1990_2024.csv")




# Keep the states in our main ("preferred") sample

# We drop five jurisdictions (already marked in the column sample_preferred):
#   Iowa            - enacted its RPS in 1983, so there is no "before" period
#   Kansas, Montana - repealed their RPS, so treatment switches off
#   Vermont, DC     - generate almost no fossil electricity (extreme outliers)
# That leaves 46 states: 27 RPS states and 19 never-RPS states.

d <- subset(full, sample_preferred == 1)


# Add a text label for each state's group, used in the tables and graphs.
d$group <- factor(ifelse(d$rps_ever == 1, "RPS states", "Never-RPS states"),
                  levels = c("RPS states", "Never-RPS states"))


# Print a quick check in the Console.
cat("States in sample:", length(unique(d$state)), "\n")
cat("RPS states:      ", length(unique(d$state[d$rps_ever == 1])), "\n")
cat("Never-RPS states:", length(unique(d$state[d$rps_ever == 0])), "\n")
cat("State-years:     ", nrow(d), "\n")




# Data Table 1: summary statistics

# For each variable, the table shows:
#   - the mean and standard deviation over all state-years
#   - the average for RPS states in 1990 and 2024
#   - the average for never-RPS states in 1990 and 2024
# Comparing 1990 with 2024 shows how each group changed over time.

# The variables to include, with the label to print in the table.
table_vars <- c(
  co2_per_mwh          = "CO2 per MWh (metric tons)",
  ln_co2_per_mwh       = "ln(CO2 per MWh)",
  coal_share           = "Coal share (%)",
  gas_share            = "Natural gas share (%)",
  nuclear_share        = "Nuclear share (%)",
  hydro_share          = "Hydro share (%)",
  nonhydro_renew_share = "Non-hydro renewable share (%)",
  rps_target_pct       = "RPS target (%)",
  rggi                 = "RGGI participant",
  restructured         = "Restructured",
  hdd                  = "Heating degree days",
  cdd                  = "Cooling degree days",
  precip_in            = "Precipitation (inches)"
)


# Build the table one variable (one row) at a time.
table1 <- data.frame()

for (v in names(table_vars)) {
  x <- d[[v]]                                         # the column for this variable
  rps_1990   <- d[d$rps_ever == 1 & d$year == 1990, v]
  rps_2024   <- d[d$rps_ever == 1 & d$year == 2024, v]
  never_1990 <- d[d$rps_ever == 0 & d$year == 1990, v]
  never_2024 <- d[d$rps_ever == 0 & d$year == 2024, v]

  table1 <- rbind(table1, data.frame(
    variable          = table_vars[[v]],
    mean_all          = mean(x, na.rm = TRUE),       # na.rm = TRUE skips missing values
    sd_all            = sd(x, na.rm = TRUE),
    rps_1990          = mean(rps_1990, na.rm = TRUE),
    rps_2024          = mean(rps_2024, na.rm = TRUE),
    never_rps_1990    = mean(never_1990, na.rm = TRUE),
    never_rps_2024    = mean(never_2024, na.rm = TRUE),
    observations      = sum(!is.na(x))
  ))
}


# Round to 3 decimals and print it in the Console.
num_cols <- sapply(table1, is.numeric)
table1[num_cols] <- round(table1[num_cols], 3)

show_table("Data Table 1: summary statistics", table1)




# Data Figure 1: when each state adopted its RPS

# One row per RPS state. The blue dot is the year the RPS law was enacted;
# the hollow dot is the first year utilities actually had to comply.
# This shows the "staggered" adoption: states adopted in different years,
# which is why we use a staggered difference-in-differences method.

adopt <- unique(d[d$rps_ever == 1,
                  c("state", "rps_enact_year", "rps_first_compliance_year")])
adopt <- adopt[order(adopt$rps_enact_year), ]                 # earliest adopters first
adopt$state <- factor(adopt$state, levels = rev(adopt$state)) # keeps that order on the graph

fig1 <- ggplot(adopt, aes(y = state)) +
  geom_segment(aes(x = rps_enact_year, xend = rps_first_compliance_year, yend = state),
               colour = "grey70", linewidth = 1) +
  geom_point(aes(x = rps_enact_year, shape = "Enacted"), colour = COL_RPS, size = 2.8) +
  geom_point(aes(x = rps_first_compliance_year, shape = "First compliance year"),
             colour = "grey40", size = 2.4) +
  scale_shape_manual(values = c("Enacted" = 16, "First compliance year" = 1), name = NULL) +
  scale_x_continuous(breaks = seq(1990, 2025, 5), limits = c(1990, 2024)) +
  labs(title = "Staggered RPS adoption: 27 states in 13 enactment years",
       x = NULL, y = NULL,
       caption = "Source: Berkeley Lab RPS database. 19 never-RPS control states not shown.")

print(fig1)




# Data Figure 2: generation mix over time

# For each group, the average share of electricity from each fuel, by year.
# Shows coal falling and natural gas rising in BOTH groups.

fuel_labels <- c(coal_share           = "Coal",
                 gas_share            = "Natural gas",
                 nuclear_share        = "Nuclear",
                 hydro_share          = "Hydro",
                 nonhydro_renew_share = "Wind, solar & other renewables")


# Average each fuel's share by year and group, stacking the results into one table.
mix <- data.frame()

for (v in names(fuel_labels)) {
  avg <- aggregate(d[[v]], by = list(year = d$year, group = d$group), FUN = mean)
  names(avg)[3] <- "share"
  avg$fuel <- fuel_labels[[v]]
  mix <- rbind(mix, avg)
}

mix$fuel  <- factor(mix$fuel, levels = fuel_labels)           # legend order
mix$group <- factor(mix$group, levels = c("RPS states", "Never-RPS states"))


fig2 <- ggplot(mix, aes(year, share, colour = fuel)) +
  geom_line(linewidth = 1) +
  facet_wrap(~ group) +                                       # one panel per group
  scale_colour_manual(values = c("Coal" = "grey25", "Natural gas" = "#ff7f0e",
                                 "Nuclear" = "#9467bd", "Hydro" = "#17becf",
                                 "Wind, solar & other renewables" = "#2ca02c"),
                      name = NULL) +
  guides(colour = guide_legend(nrow = 2)) +
  labs(title = "Generation mix: coal's decline drives both groups",
       subtitle = "Average share of in-state net generation",
       x = NULL, y = "Share of generation (%)",
       caption = "Source: EIA net generation by state and energy source")

print(fig2)




# Data Figure 3: carbon intensity over time

# Average CO2 per MWh for RPS states vs never-RPS states, every year.
# This is the main descriptive graph: the two lines move in parallel before
# most RPS laws, then the RPS line falls faster.

trend <- aggregate(co2_per_mwh ~ year + group, data = d, FUN = mean)
trend$group <- factor(trend$group, levels = c("RPS states", "Never-RPS states"))

fig3 <- ggplot(trend, aes(year, co2_per_mwh, colour = group)) +
  # Shaded band marks the years when most RPS laws were passed.
  annotate("rect", xmin = 1997, xmax = 2008, ymin = -Inf, ymax = Inf, alpha = 0.10) +
  annotate("text", x = 2002.5, y = min(trend$co2_per_mwh) + 0.02,
           label = "Most RPS laws enacted\n1997-2008", size = 3.5, colour = "grey35") +
  geom_line(linewidth = 1.1) +
  geom_point(size = 1.4) +
  scale_colour_manual(values = group_colours, name = NULL) +
  labs(title = "Carbon intensity of electricity, RPS vs. never-RPS states",
       subtitle = "Unweighted average across states",
       x = NULL, y = "CO2 per MWh (metric tons)",
       caption = "Source: EIA State Electricity Profiles, 1990-2024")

print(fig3)


cat("\n\nPart 1 done: Data Table 1 printed above, Data Figures 1-3 in the Plots pane.\n\n")




# RESULTS




# Helper functions for the Callaway & Sant'Anna estimator

# run_cs() estimates ATT(g,t), aggregates it to an event study and an overall
# ATT, and runs a joint Wald test that all pre-period effects are zero.
run_cs <- function(data, yname = "ln_co2_per_mwh", gname = "cohort_g",
                   xformla = ~1, control_group = "nevertreated") {
  data <- data[!is.na(data[[gname]]), ]

  att <- att_gt(yname = yname, tname = "year", idname = "state_id", gname = gname,
                xformla = xformla, data = data, control_group = control_group,
                base_period = "universal",
                bstrap = FALSE, cband = FALSE)   # standard errors by formula, clustered by state
  es <- aggte(att, type = "dynamic", min_e = MIN_E, max_e = MAX_E, na.rm = TRUE)
  ov <- aggte(att, type = "group", na.rm = TRUE)

  # Joint pre-trend test using the influence functions (cov = IF'IF / n^2)
  IF  <- es$inf.function$dynamic.inf.func.e
  pre <- which(es$egt < -1)
  b   <- es$att.egt[pre]
  V   <- crossprod(IF[, pre, drop = FALSE]) / nrow(IF)^2
  W   <- as.numeric(t(b) %*% solve(V, b))

  es_df <- data.frame(event_time = es$egt, att = es$att.egt, se = es$se.egt)
  es_df$lo <- es$att.egt - 1.96 * es$se.egt   # 95% confidence interval
  es_df$hi <- es$att.egt + 1.96 * es$se.egt

  cohorts <- data.frame(cohort = ov$egt, att = ov$att.egt, se = ov$se.egt,
                        n_states = sapply(ov$egt, function(g) length(unique(data$state_id[data[[gname]] == g]))))

  list(event = es_df, overall = ov$overall.att, overall_se = ov$overall.se,
       dyn_overall = es$overall.att, dyn_overall_se = es$overall.se, cohorts = cohorts,
       pre_chi2 = W, pre_p = pchisq(W, df = length(b), lower.tail = FALSE),
       n_states = length(unique(data$state_id)))
}


# plot_event() draws an event-study graph from the output of run_cs().
plot_event <- function(res, title, ylab = "Effect on ln(CO2 per MWh)") {
  df <- res$event
  df$period <- ifelse(df$event_time < 0, "Before enactment", "After enactment")
  df$lo[df$event_time == -1] <- 0; df$hi[df$event_time == -1] <- 0

  ggplot(df, aes(event_time, att, colour = period)) +
    geom_hline(yintercept = 0, linetype = "dashed") +
    geom_vline(xintercept = -0.5, colour = "grey50") +
    geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.3) +
    geom_point(size = 2.3) +
    scale_colour_manual(values = c("Before enactment" = COL_CTRL, "After enactment" = COL_RPS),
                        name = NULL) +
    labs(title = title,
         subtitle = sprintf("Overall ATT = %.3f (SE %.3f); pre-trend test p = %.2f",
                            res$overall, res$overall_se, res$pre_p),
         x = "Years relative to RPS enactment (0 = enactment year)", y = ylab,
         caption = paste("Callaway & Sant'Anna (2021); never-treated controls; base period = year",
                         "before enactment;\n95% confidence intervals, standard errors clustered by state"))
}


# pval() and stars() are used by the results tables further down.
pval  <- function(est, se) 2 * pnorm(-abs(est / se))
stars <- function(p) ifelse(p < 0.01, "***", ifelse(p < 0.05, "**", ifelse(p < 0.10, "*", "")))




# Results Figure 1: main event study (treatment dated from the enactment year)
# Each dot is the effect of an RPS on ln(CO2 per MWh) in one year relative to
# enactment (0 = enactment year). Grey dots are years before the law, blue dots
# are years after, and the bars are 95% confidence intervals. Grey dots near
# zero support parallel trends; the blue dots show the effect over time.

es_main <- run_cs(d)

print(plot_event(es_main, "RPS and carbon intensity: event study (enactment year)"))




# Results Figure 2: treatment dated from the first compliance year
# Same graph as Results Figure 1, but a state counts as treated from the first
# year its RPS was binding instead of the year it was passed. This is the
# robustness check for treatment timing.

es_comp <- run_cs(d, gname = "cohort_binding")

print(plot_event(es_comp, "Event study using first compliance year"))




# Results Figure 3: conditional on the 1990 coal share
# Same graph as Results Figure 1, but it only compares states with similar
# coal reliance in 1990. This is the robustness check for parallel trends.
# The 1990 value is used because it comes before any RPS (not a bad control).

es_coal <- run_cs(d, xformla = ~ coal_share_1990)

print(plot_event(es_coal, "Event study conditional on 1990 coal share"))




# Results Figure 4: the three event studies on one graph
# The point estimates from Results Figures 1-3 together, to show that all
# three versions follow the same pattern.

both <- rbind(transform(es_main$event, spec = "Enactment year (main)"),
              transform(es_comp$event, spec = "First compliance year"),
              transform(es_coal$event, spec = "Conditional on 1990 coal share"))
both$spec <- factor(both$spec, levels = c("Enactment year (main)", "First compliance year",
                                          "Conditional on 1990 coal share"))

fig_compare <- ggplot(both, aes(event_time, att, colour = spec)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, colour = "grey50") +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.8) +
  scale_colour_manual(values = c(COL_RPS, "#ff7f0e", "#2ca02c"), name = NULL) +
  labs(title = "Event-study estimates are similar across timing definitions",
       x = "Years relative to RPS adoption", y = "Effect on ln(CO2 per MWh)",
       caption = "Point estimates only; see Results Figures 1-3 for confidence bands")

print(fig_compare)




# Results Figure 5: mechanisms - effect of an RPS on the generation mix
# Three event studies side by side. The outcome is each fuel's share of
# generation (coal, natural gas, wind/solar) instead of CO2 per MWh. Shows HOW
# an RPS changes carbon intensity: less coal, more gas, or more renewables.

mech_vars <- c(coal_share = "Coal share", gas_share = "Natural gas share",
               nonhydro_renew_share = "Wind, solar & other renewables share")

mech_overall <- data.frame()

mech <- do.call(rbind, lapply(names(mech_vars), function(v) {
  r <- run_cs(d, yname = v)
  cat(sprintf("Mechanism %-22s overall ATT = %6.2f pp (SE %.2f)\n", v, r$overall, r$overall_se))
  late <- r$event[r$event$event_time >= 9, ]
  mech_overall <<- rbind(mech_overall, data.frame(
    outcome = mech_vars[[v]], overall_att_pp = r$overall, se = r$overall_se,
    p_value = 2 * pnorm(-abs(r$overall / r$overall_se)),
    avg_effect_years_9_15_pp = mean(late$att), pre_trend_p = r$pre_p))
  transform(r$event, outcome = mech_vars[[v]])
}))

mech$outcome <- factor(mech$outcome, levels = unname(mech_vars))


fig_mech <- ggplot(mech, aes(event_time, att)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = -0.5, colour = "grey50") +
  geom_ribbon(aes(ymin = lo, ymax = hi), fill = COL_RPS, alpha = 0.15) +
  geom_line(colour = COL_RPS, linewidth = 0.9) +
  geom_point(colour = COL_RPS, size = 1.4) +
  facet_wrap(~ outcome, scales = "free_y") +
  labs(title = "Mechanisms: RPS effects on the generation mix",
       x = "Years relative to RPS enactment", y = "Effect (percentage points)",
       caption = "Callaway & Sant'Anna; never-treated controls; 95% confidence intervals")

print(fig_mech)




# Results Table 1: main estimate and every robustness check

results <- data.frame()

add_row <- function(spec, est, se, n, group) {
  results <<- rbind(results, data.frame(
    specification = spec, group = group, estimate = est, se = se,
    ci_lo = est - 1.96 * se, ci_hi = est + 1.96 * se,
    pct_change = 100 * (exp(est) - 1), n_states = n))
}

add_cs <- function(spec, res) add_row(spec, res$overall, res$overall_se, res$n_states, "Callaway-Sant'Anna")


# Callaway & Sant'Anna versions
add_cs("Main: enactment year", es_main)
add_cs("First compliance year", es_comp)
add_cs("Conditional on 1990 coal share", es_coal)
add_cs("Not-yet-treated controls", run_cs(d, control_group = "notyettreated"))
add_cs("Add back Vermont & DC",
       run_cs(subset(full, sample_main == 1 & flag_reversal == 0)))
add_cs("Drop hydro states (ID, OR, WA)", run_cs(subset(d, !state %in% c("ID", "OR", "WA"))))


# TWFE: Y_st = a_s + l_t + b*RPS_st + X_st'g + e_st, SEs clustered by state
twfe <- function(formula, data) {
  data <- data[complete.cases(data[, all.vars(formula)]), ]
  m  <- lm(update(formula, . ~ . + factor(state) + factor(year)), data = data)
  X  <- model.matrix(m); e <- resid(m)
  meat <- Reduce(`+`, lapply(split(seq_along(e), data$state), function(i) {
    u <- crossprod(X[i, , drop = FALSE], e[i]); u %*% t(u) }))
  G <- length(unique(data$state)); n <- nrow(X); k <- ncol(X)
  V <- solve(crossprod(X)) %*% meat %*% solve(crossprod(X)) * (G / (G - 1)) * ((n - 1) / (n - k))
  term <- all.vars(formula)[2]
  list(est = unname(coef(m)[term]), se = unname(sqrt(diag(V))[term]), n = G)
}

controls <- "rggi + restructured + hdd + cdd + precip_in"
t1 <- twfe(ln_co2_per_mwh ~ rps_enacted, d)
t2 <- twfe(as.formula(paste("ln_co2_per_mwh ~ rps_enacted +", controls)), d)
t3 <- twfe(as.formula(paste("ln_co2_per_mwh ~ rps_target_pct +", controls)), d)

add_row("TWFE: RPS dummy, no controls", t1$est, t1$se, t1$n, "TWFE benchmark")
add_row("TWFE: RPS dummy + controls", t2$est, t2$se, t2$n, "TWFE benchmark")


results[, 3:7] <- round(results[, 3:7], 4)

show_table("Results Table 1: main estimate and robustness checks", results)




# Results Table 2: dose response (effect per 1 pp of RPS target)

dose <- data.frame(specification = "TWFE dose-response: per 1 pp of RPS target + controls",
                   estimate = round(t3$est, 5), se = round(t3$se, 5),
                   pct_change_per_pp = round(100 * (exp(t3$est) - 1), 3),
                   implied_pct_at_avg_2024_target =
                     round(100 * (exp(t3$est * mean(d$rps_target_pct[d$rps_ever == 1 & d$year == 2024])) - 1), 2),
                   n_states = t3$n)

show_table("Results Table 2: dose response (RPS target)", dose)




# Results Table 3: pre-trend tests

pre_tests <- data.frame(
  specification = c("Enactment year (main)", "First compliance year", "Conditional on 1990 coal share"),
  chi2 = round(c(es_main$pre_chi2, es_comp$pre_chi2, es_coal$pre_chi2), 2),
  p_value = round(c(es_main$pre_p, es_comp$pre_p, es_coal$pre_p), 3))

show_table("Results Table 3: pre-trend tests", pre_tests)




# Results Table 4: main results formatted like a regression table

main_rows <- results
main_rows$p_value   <- pval(main_rows$estimate, main_rows$se)
main_rows$formatted <- sprintf("%.3f%s (%.3f)", main_rows$estimate, stars(main_rows$p_value), main_rows$se)
main_rows$pct_change_ci <- sprintf("%.1f%% [%.1f%%, %.1f%%]", main_rows$pct_change,
                                   100 * (exp(main_rows$ci_lo) - 1), 100 * (exp(main_rows$ci_hi) - 1))
main_rows$pre_trend_p <- c(es_main$pre_p, es_comp$pre_p, es_coal$pre_p, rep(NA, nrow(main_rows) - 3))

table5 <- main_rows[, c("specification", "group", "formatted", "p_value", "pct_change_ci",
                        "pre_trend_p", "n_states")]
table5$p_value <- round(table5$p_value, 3); table5$pre_trend_p <- round(table5$pre_trend_p, 3)

show_table("Results Table 4: results formatted like a regression table", table5)




# Results Table 5: effect by years since enactment (main specification)

ev <- es_main$event
ev$p_value_pointwise <- pval(ev$att, ev$se)
ev$pct_change <- 100 * (exp(ev$att) - 1)
ev$significant_10pct <- ifelse(ev$p_value_pointwise < 0.10 & ev$event_time != -1, "yes", "no")

table6 <- transform(ev, att = round(att, 4), se = round(se, 4), lo = round(lo, 4), hi = round(hi, 4),
                    p_value_pointwise = round(p_value_pointwise, 3), pct_change = round(pct_change, 1))
names(table6)[names(table6) %in% c("lo", "hi")] <- c("ci_lo", "ci_hi")

show_table("Results Table 5: effect by years since enactment", table6)




# Results Table 6: effect by adoption cohort

coh <- es_main$cohorts
coh$p_value <- pval(coh$att, coh$se)
coh$pct_change <- 100 * (exp(coh$att) - 1)
coh$states <- sapply(coh$cohort, function(g) paste(sort(unique(d$state[d$cohort_g == g])), collapse = ", "))
coh$years_observed_after <- 2024 - coh$cohort + 1

show_table("Results Table 6: effect by adoption cohort",
           transform(coh, att = round(att, 4), se = round(se, 4), p_value = round(p_value, 3),
                     pct_change = round(pct_change, 1)))




# Results Figure 6: effect by adoption cohort
# One row per enactment year, listing its states. The dot is that group's
# average effect after enactment, its size is the number of states, and the
# blue vertical line is the overall effect. Shows how the effect varies.

coh$label <- sprintf("%d: %s", coh$cohort, coh$states)
coh$label <- factor(coh$label, levels = rev(coh$label))

fig_cohort <- ggplot(coh, aes(100 * (exp(att) - 1), label)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 100 * (exp(es_main$overall) - 1), colour = COL_RPS, alpha = 0.5) +
  geom_errorbar(aes(xmin = 100 * (exp(att - 1.96 * se) - 1), xmax = 100 * (exp(att + 1.96 * se) - 1)),
                width = 0.25, orientation = "y", colour = "grey45") +
  geom_point(aes(size = n_states), colour = COL_RPS) +
  scale_size_continuous(range = c(2, 5), name = "States in cohort") +
  labs(title = "Effect of an RPS by adoption cohort",
       subtitle = "Average post-enactment effect for each enactment year (blue line = overall effect)",
       x = "Effect on CO2 per MWh (% change, 95% CI)", y = "Enactment year: states",
       caption = "Callaway & Sant'Anna group-level ATTs; cohorts with one state are imprecise")

print(fig_cohort)




# Results Table 7: mechanisms (fuel shares)

mech_overall$p_value <- round(mech_overall$p_value, 3)
mech_overall[, c(2, 3, 5, 6)] <- round(mech_overall[, c(2, 3, 5, 6)], 3)

show_table("Results Table 7: mechanisms (fuel shares)", mech_overall)




# Results Table 8: CO2 avoided in 2024 in RPS states

# Counterfactual emissions without an RPS = actual / exp(theta), so
# avoided = actual * (exp(-theta) - 1). Uses each state's 2024 emissions.
# Social cost of carbon: EPA (2023) central estimate, ~$190 per metric ton
# (2020 dollars, 2% discount rate). Change SCC below to use another value.
SCC <- 190

treated24 <- d[d$rps_ever == 1 & d$year == 2024, ]
us_power_co2_2024 <- sum(full$co2_tons[full$year == 2024])
avoided <- function(theta) sum(treated24$co2_tons * (exp(-theta) - 1))

mag <- data.frame(
  scenario = c("Main estimate", "Lower 95% bound (larger effect)", "Upper 95% bound (smaller effect)",
               "Conditional on 1990 coal share"),
  theta = c(es_main$overall, es_main$overall - 1.96 * es_main$overall_se,
            es_main$overall + 1.96 * es_main$overall_se, es_coal$overall))
mag$pct_change_intensity    <- 100 * (exp(mag$theta) - 1)
mag$co2_avoided_million_t   <- sapply(mag$theta, avoided) / 1e6
mag$pct_of_us_power_co2     <- 100 * mag$co2_avoided_million_t * 1e6 / us_power_co2_2024
mag$climate_benefit_billion <- mag$co2_avoided_million_t * 1e6 * SCC / 1e9
mag[, -1] <- round(mag[, -1], 3)
mag$note <- c(sprintf("RPS states' actual 2024 power-sector CO2: %.1f million t; U.S. total: %.1f million t",
                      sum(treated24$co2_tons) / 1e6, us_power_co2_2024 / 1e6),
              "", "Negative = emissions would have been lower without RPS (CI includes zero)",
              sprintf("Climate benefit uses SCC = $%d per ton", SCC))

show_table("Results Table 8: CO2 avoided in 2024", mag)




# Results Figure 7: actual vs. estimated carbon intensity without an RPS
# Solid line = actual average CO2 per MWh in RPS states. Dashed line = what
# the main estimates imply it would have been without an RPS (the estimated
# effect added back). Illustrates the size of the effect.
cf <- d[d$rps_ever == 1, c("state", "year", "co2_per_mwh", "event_time")]
cf <- merge(cf, es_main$event[, c("event_time", "att")], by = "event_time", all.x = TRUE)
cf$att[is.na(cf$att) | cf$event_time < 0] <- 0
cf$att[cf$event_time > MAX_E] <- es_main$event$att[es_main$event$event_time == MAX_E]
cf$counterfactual <- cf$co2_per_mwh / exp(cf$att)

cf_avg <- aggregate(cbind(co2_per_mwh, counterfactual) ~ year, data = cf, FUN = mean)
cf_long <- rbind(data.frame(year = cf_avg$year, value = cf_avg$co2_per_mwh, series = "Actual (RPS states)"),
                 data.frame(year = cf_avg$year, value = cf_avg$counterfactual,
                            series = "Estimated without RPS"))

fig_counterfactual <- ggplot(cf_long, aes(year, value, colour = series, linetype = series)) +
  geom_line(linewidth = 1.2) +
  scale_colour_manual(values = c("Actual (RPS states)" = COL_RPS, "Estimated without RPS" = "grey40"), name = NULL) +
  scale_linetype_manual(values = c("Actual (RPS states)" = "solid", "Estimated without RPS" = "dashed"), name = NULL) +
  labs(title = "What the estimates imply: RPS states' carbon intensity with and without an RPS",
       subtitle = "Counterfactual removes the estimated event-study effect for each state-year",
       x = NULL, y = "Average CO2 per MWh (metric tons)",
       caption = "Effects beyond 15 years after enactment held at the year-15 estimate")

print(fig_counterfactual)




# Summary of every key number (printed at the end of the Console)

f <- function(x, k = 3) formatC(x, format = "f", digits = k)
avg_target_2024 <- mean(d$rps_target_pct[d$rps_ever == 1 & d$year == 2024])
late <- es_main$event[es_main$event$event_time >= 9, ]
sig_years <- es_main$event$event_time[es_main$event$event_time >= 0 &
                                       pval(es_main$event$att, es_main$event$se) < 0.10]

summary_md <- c(
  "KEY RESULTS", "",
  "Sample",
  sprintf("- %d states, %d state-years, 1990-2024: %d RPS states adopting in %d years, %d never-RPS controls.",
          length(unique(d$state)), nrow(d), length(unique(d$state[d$rps_ever == 1])),
          length(unique(d$cohort_g[d$rps_ever == 1])), length(unique(d$state[d$rps_ever == 0]))), "",
  "Main effect (Callaway & Sant'Anna, enactment year, never-treated controls)",
  sprintf("- Overall ATT on ln(CO2/MWh): %s (SE %s), p = %s -> %s%% change in carbon intensity.",
          f(es_main$overall), f(es_main$overall_se), f(pval(es_main$overall, es_main$overall_se)),
          f(100 * (exp(es_main$overall) - 1), 1)),
  sprintf("- 95%% CI: %s%% to %s%%.", f(100 * (exp(es_main$overall - 1.96 * es_main$overall_se) - 1), 1),
          f(100 * (exp(es_main$overall + 1.96 * es_main$overall_se) - 1), 1)),
  sprintf("- Average effect 9-15 years after enactment: %s log points (%s%%).", f(mean(late$att)),
          f(100 * (exp(mean(late$att)) - 1), 1)),
  sprintf("- Post-enactment years with pointwise p < 0.10: %s.",
          ifelse(length(sig_years) == 0, "none", paste(sig_years, collapse = ", "))),
  sprintf("- Pre-trend joint test: chi2 = %s, p = %s (cannot reject parallel pre-trends).",
          f(es_main$pre_chi2, 2), f(es_main$pre_p)), "",
  "Robustness (overall ATT, % change)",
  sprintf("- %s: %s%% (p = %s)", results$specification, f(results$pct_change, 1),
          f(pval(results$estimate, results$se))), "",
  "Policy intensity",
  sprintf("- Each 1 pp of RPS target: %s%% change in carbon intensity (p = %s); at the average 2024 target (%s%%) this implies %s%%.",
          f(100 * (exp(t3$est) - 1), 2), f(pval(t3$est, t3$se)), f(avg_target_2024, 1),
          f(100 * (exp(t3$est * avg_target_2024) - 1), 1)), "",
  "Mechanisms (overall ATT, percentage points of generation)",
  sprintf("- %s: %s pp (SE %s, p = %s); years 9-15 average %s pp.", mech_overall$outcome,
          f(mech_overall$overall_att_pp, 2), f(mech_overall$se, 2), f(mech_overall$p_value),
          f(mech_overall$avg_effect_years_9_15_pp, 2)), "",
  "Economic magnitude (2024)",
  sprintf("- Main estimate implies %s million metric tons of CO2 avoided in 2024 in RPS states (%s%% of U.S. power-sector CO2), worth about $%s billion at an SCC of $%d/ton.",
          f(mag$co2_avoided_million_t[1], 1), f(mag$pct_of_us_power_co2[1], 1),
          f(mag$climate_benefit_billion[1], 1), SCC),
  sprintf("- Range across the 95%% CI: %s to %s million tons.", f(mag$co2_avoided_million_t[3], 1),
          f(mag$co2_avoided_million_t[2], 1)), "",
  "Heterogeneity by adoption cohort",
  sprintf("- %d: %s%% (SE %s log points; %d state(s): %s)", coh$cohort, f(coh$pct_change, 1), f(coh$se),
          coh$n_states, coh$states), "",
  "Caveats to state in the interpretation",
  "- Emissions are measured where electricity is generated; imports and out-of-state renewable credits are not captured.",
  "- The estimate is the effect of an RPS plus any policies adopted at the same time.",
  "- Confidence intervals are wide (46 state clusters); several cohorts contain a single state.")

cat("\n\n", paste(summary_md, collapse = "\n"), "\n", sep = "")

