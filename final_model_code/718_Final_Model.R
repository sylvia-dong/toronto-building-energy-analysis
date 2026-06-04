# ============================================================
# MSE718 Term Project - Toronto Municipal Building Energy
# Bayesian Multilevel Analysis
# ============================================================

# ============================================================
# STEP 0: INSTALL PACKAGES (run once, then comment out)
# ============================================================
# install.packages(c("brms", "tidyverse", "bayesplot", "loo", "patchwork", "scales"))

# ============================================================
# STEP 1: SET WORKING DIRECTORY & LOAD PACKAGES
# ============================================================

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(brms)
library(tidyverse)
library(bayesplot)
library(loo)
library(patchwork)
library(scales)

set.seed(42)

# ============================================================
# 1. LOAD DATA
# ============================================================
# Input: Toronto_Master_Panel_Ready_v2.csv
# Drops 4 rows with missing Weekly_Operating_Hours

df <- read_csv(
  file.path(dirname(rstudioapi::getSourceEditorContext()$path),
            "../cleaned data/merged dataset/Toronto_Master_Panel_Ready_v2.csv")
) %>%
  drop_na(log_Hours)

cat("Dataset loaded:", nrow(df), "rows\n")
cat("Building types:", paste(sort(unique(df$Building_Type)), collapse = ", "), "\n")
cat("Years:", paste(sort(unique(df$Year)), collapse = ", "), "\n")
cat("\nObservations by year:\n")
print(table(df$Year))
cat("\nObservations by building type:\n")
print(table(df$Building_Type))
cat("\nElectricity emission factor used per year:\n")
print(df %>% group_by(Year) %>% summarise(ELEC_factor = first(ELEC_factor_used)))

# ============================================================
# 2. DESCRIPTIVE STATISTICS
# ============================================================

desc_stats <- df %>%
  group_by(Building_Type) %>%
  summarise(
    n        = n(),
    mean_EUI = round(mean(EUI_ekWh_sqft), 2),
    sd_EUI   = round(sd(EUI_ekWh_sqft), 2),
    mean_GHG = round(mean(GHG_Intensity_kg_sqft), 4),
    sd_GHG   = round(sd(GHG_Intensity_kg_sqft), 4),
    .groups  = "drop"
  )
cat("\nDescriptive stats by building type:\n")
print(desc_stats)

# ============================================================
# 3. PRIOR SPECIFICATION
# ============================================================
# Energy track (Y = log_EUI):
#   Intercept ~ Normal(3, 1)   exp(3) ~ 20 ekWh/sqft, matches portfolio mean
#   b         ~ Normal(0, 1)   weakly informative slopes
#   sigma     ~ Exponential(1) residual SD, constrained positive
#   sd        ~ Exponential(1) group-level SD (multilevel models only)
#   cor       ~ LKJ(2)         mild shrinkage toward independence (E3/C3 only)
#
# Carbon track (Y = log_GHG_intensity):
#   Intercept ~ Normal(1, 1)   exp(1) ~ 2.7 kg CO2e/sqft, matches portfolio mean
#   (all other priors identical to energy track)

ep_base <- c(prior(normal(3, 1),   class = Intercept),
             prior(normal(0, 1),   class = b),
             prior(exponential(1), class = sigma))

ep_ri   <- c(prior(normal(3, 1),   class = Intercept),
             prior(normal(0, 1),   class = b),
             prior(exponential(1), class = sigma),
             prior(exponential(1), class = sd))

ep_rs   <- c(prior(normal(3, 1),   class = Intercept),
             prior(normal(0, 1),   class = b),
             prior(exponential(1), class = sigma),
             prior(exponential(1), class = sd),
             prior(lkj(2),         class = cor))

cp_base <- c(prior(normal(1, 1),   class = Intercept),
             prior(normal(0, 1),   class = b),
             prior(exponential(1), class = sigma))

cp_ri   <- c(prior(normal(1, 1),   class = Intercept),
             prior(normal(0, 1),   class = b),
             prior(exponential(1), class = sigma),
             prior(exponential(1), class = sd))

cp_rs   <- c(prior(normal(1, 1),   class = Intercept),
             prior(normal(0, 1),   class = b),
             prior(exponential(1), class = sigma),
             prior(exponential(1), class = sd),
             prior(lkj(2),         class = cor))

CHAINS <- 4; ITER <- 2000; WARMUP <- 1000; CORES <- 4

# ============================================================
# 4. PRIOR PREDICTIVE CHECK
# ============================================================
# Verifies that priors generate physically plausible EUI and GHG values
# before observing any data. Red dashed lines mark the plausible range.

E_prior <- brm(
  formula = log_EUI ~ log_GFA + log_Hours + Year_scaled +
    (1 + log_GFA | Building_Type),
  data = df, family = gaussian(),
  prior = ep_rs,
  sample_prior = "only",
  chains = 2, iter = 1000, warmup = 500,
  cores = CORES, seed = 42,
  file = "../output/E_prior_only"
)

C_prior <- brm(
  formula = log_GHG_intensity ~ log_GFA + log_Hours + Year_scaled +
    (1 + log_GFA | Building_Type),
  data = df, family = gaussian(),
  prior = cp_rs,
  sample_prior = "only",
  chains = 2, iter = 1000, warmup = 500,
  cores = CORES, seed = 42,
  file = "../output/C_prior_only"
)

ppc_prior_e <- pp_check(E_prior, ndraws = 100) +
  geom_vline(xintercept = c(log(5), log(150)),
             linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Prior Predictive Check: Energy",
       subtitle = "Red dashes = plausible EUI range (5-150 ekWh/sqft)",
       x = "log(EUI)") +
  theme_minimal(base_size = 11)

ppc_prior_c <- pp_check(C_prior, ndraws = 100) +
  geom_vline(xintercept = c(log(0.1), log(20)),
             linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Prior Predictive Check: Carbon",
       subtitle = "Red dashes = plausible GHG range (0.1-20 kg/sqft)",
       x = "log(GHG Intensity)") +
  theme_minimal(base_size = 11)

ggsave("../output/718_prior_ppc_energy_carbon.png",
       ppc_prior_e + ppc_prior_c, width = 10, height = 4, dpi = 300)
cat("Prior PPC saved.\n")

# ============================================================
# 5. ENERGY TRACK MODELS
# ============================================================
# E1: baseline (no multilevel structure)
# E2: varying intercept by building type
# E3: varying intercept + varying slope on log_GFA (primary model)

cat("\n--- Fitting Energy Models (E1, E2, E3) ---\n")

E1 <- brm(
  formula = log_EUI ~ log_GFA + log_Hours + Year_scaled,
  data = df, family = gaussian(), prior = ep_base,
  chains = CHAINS, iter = ITER, warmup = WARMUP,
  cores = CORES, seed = 42,
  file = "../output/E1_baseline"
)

E2 <- brm(
  formula = log_EUI ~ log_GFA + log_Hours + Year_scaled +
    (1 | Building_Type),
  data = df, family = gaussian(), prior = ep_ri,
  chains = CHAINS, iter = ITER, warmup = WARMUP,
  cores = CORES, seed = 42,
  control = list(adapt_delta = 0.95),
  file = "../output/E2_random_intercept"
)

E3 <- brm(
  formula = log_EUI ~ log_GFA + log_Hours + Year_scaled +
    (1 + log_GFA | Building_Type),
  data = df, family = gaussian(), prior = ep_rs,
  chains = CHAINS, iter = ITER, warmup = WARMUP,
  cores = CORES, seed = 42,
  control = list(adapt_delta = 0.99, max_treedepth = 15),
  file = "../output/E3_random_slope"
)

# ============================================================
# 6. CARBON TRACK MODELS
# ============================================================
# C1: baseline (no multilevel structure)
# C2: varying intercept by building type
# C3: varying intercept + varying slope on log_GFA (primary model)

cat("\n--- Fitting Carbon Models (C1, C2, C3) ---\n")

C1 <- brm(
  formula = log_GHG_intensity ~ log_GFA + log_Hours + Year_scaled,
  data = df, family = gaussian(), prior = cp_base,
  chains = CHAINS, iter = ITER, warmup = WARMUP,
  cores = CORES, seed = 42,
  file = "../output/C1_baseline"
)

C2 <- brm(
  formula = log_GHG_intensity ~ log_GFA + log_Hours + Year_scaled +
    (1 | Building_Type),
  data = df, family = gaussian(), prior = cp_ri,
  chains = CHAINS, iter = ITER, warmup = WARMUP,
  cores = CORES, seed = 42,
  control = list(adapt_delta = 0.995, max_treedepth = 15),
  file = "../output/C2_random_intercept"
)

C3 <- brm(
  formula = log_GHG_intensity ~ log_GFA + log_Hours + Year_scaled +
    (1 + log_GFA | Building_Type),
  data = df, family = gaussian(), prior = cp_rs,
  chains = CHAINS, iter = ITER, warmup = WARMUP,
  cores = CORES, seed = 42,
  control = list(adapt_delta = 0.995, max_treedepth = 15),
  file = "../output/C3_random_slope"
)

# ============================================================
# 7. CONVERGENCE DIAGNOSTICS
# ============================================================
# Rhat < 1.01 indicates adequate convergence.
# E3 and C3 each produced one divergent transition after warmup,
# which is expected given the varying-slope specification with
# only six building-type groups. All Rhat values remain below 1.01,
# so the estimates are considered reliable for inference.

cat("\n--- Convergence Check (Rhat, all should be < 1.01) ---\n")
cat("E2 fixed effects Rhat:\n")
print(round(summary(E2)$fixed[, "Rhat"], 4))
cat("C2 fixed effects Rhat:\n")
print(round(summary(C2)$fixed[, "Rhat"], 4))

png("../output/718_diagnostics_E2_trace_energy.png", width = 1200, height = 800)
plot(E2); dev.off()
png("../output/718_diagnostics_C2_trace_carbon.png", width = 1200, height = 800)
plot(C2); dev.off()

# ============================================================

cat("\n--- Convergence Check (Rhat, all should be < 1.01) ---\n")
cat("E3 fixed effects Rhat:\n")
print(round(summary(E3)$fixed[, "Rhat"], 4))
cat("C3 fixed effects Rhat:\n")
print(round(summary(C3)$fixed[, "Rhat"], 4))

png("../output/718_diagnostics_E3_trace_energy.png", width = 1200, height = 800)
plot(E3); dev.off()
png("../output/718_diagnostics_C3_trace_carbon.png", width = 1200, height = 800)
plot(C3); dev.off()

# ============================================================
# 8. POSTERIOR PREDICTIVE CHECK
# ============================================================
# Compares observed outcome distribution (dark line) against
# 100 replicated draws from the fitted posterior (light lines).
# Both models recover the central mass well; some underfitting
# in the tails is expected for a Gaussian multilevel specification.

ppc_e <- pp_check(E2, ndraws = 100) +
  labs(title = "Energy Model (E2) - PPC", x = "log(EUI)") +
  theme_minimal(base_size = 11)

ppc_c <- pp_check(C2, ndraws = 100) +
  labs(title = "Carbon Model (C2) - PPC", x = "log(GHG Intensity)") +
  theme_minimal(base_size = 11)

ggsave("../output/718_fig2_posterior_ppc_E2C2.png",
       ppc_e + ppc_c, width = 10, height = 4, dpi = 300)
cat("Posterior PPC saved.\n")

# ============================================================

ppc_e <- pp_check(E3, ndraws = 100) +
  labs(title = "Energy Model (E3) - PPC", x = "log(EUI)") +
  theme_minimal(base_size = 11)

ppc_c <- pp_check(C3, ndraws = 100) +
  labs(title = "Carbon Model (C3) - PPC", x = "log(GHG Intensity)") +
  theme_minimal(base_size = 11)

ggsave("../output/718_fig3_posterior_ppc_E3C3.png",
       ppc_e + ppc_c, width = 10, height = 4, dpi = 300)
cat("Posterior PPC saved.\n")

# ============================================================
# 9. MODEL COMPARISON: LOO
# ============================================================
# LOO-CV selects the best-fitting model via leave-one-out
# cross-validation. elpd_diff > 2 * se_diff indicates a
# meaningful difference. E3/C3 are confirmed as primary models.

cat("\n--- LOO Comparison: Energy Track ---\n")
E1 <- add_criterion(E1, "loo")
E2 <- add_criterion(E2, "loo")
E3 <- add_criterion(E3, "loo")
loo_E <- loo_compare(E1, E2, E3)
print(loo_E)

cat("\n--- LOO Comparison: Carbon Track ---\n")
C1 <- add_criterion(C1, "loo")
C2 <- add_criterion(C2, "loo")
C3 <- add_criterion(C3, "loo")
loo_C <- loo_compare(C1, C2, C3)
print(loo_C)

loo_table <- bind_rows(
  as.data.frame(loo_E) %>% rownames_to_column("Model") %>% mutate(Track = "Energy"),
  as.data.frame(loo_C) %>% rownames_to_column("Model") %>% mutate(Track = "Carbon")
)
write_csv(loo_table, "../output/718_table1_loo_model_comparison.csv")
cat("LOO table saved.\n")

# ============================================================
# 9.5. REGRESSION TABLES (E1-E3, C1-C3) - Visualization
# ============================================================

fmt_est <- function(est, se, digits = 2) {
  ifelse(is.na(est), "",
         paste0(round(est, digits), "\n(" , round(se, digits), ")"))
}

get_fixed_block <- function(fit, model_name) {
  fx <- suppressWarnings(as.data.frame(summary(fit)$fixed))
  tibble(
    term = c("Grand Intercept", "log_GFA", "log_Hours", "Year_scaled"),
    !!model_name := c(
      fmt_est(fx["Intercept", "Estimate"], fx["Intercept", "Est.Error"]),
      fmt_est(fx["log_GFA", "Estimate"], fx["log_GFA", "Est.Error"]),
      fmt_est(fx["log_Hours", "Estimate"], fx["log_Hours", "Est.Error"]),
      fmt_est(fx["Year_scaled", "Estimate"], fx["Year_scaled", "Est.Error"])
    )
  )
}

get_group_block <- function(fit, model_name, type = c("baseline", "ri", "rs")) {
  type <- match.arg(type)
  
  if (type == "baseline") {
    return(tibble(
      term = c("SD(Intercept): Building_Type",
               "SD(log_GFA): Building_Type",
               "Cor(Intercept, log_GFA)"),
      !!model_name := c("", "", "")
    ))
  }
  
  vc <- suppressWarnings(as.data.frame(summary(fit)$random$Building_Type))
  
  if (type == "ri") {
    return(tibble(
      term = c("SD(Intercept): Building_Type",
               "SD(log_GFA): Building_Type",
               "Cor(Intercept, log_GFA)"),
      !!model_name := c(
        fmt_est(vc["sd(Intercept)", "Estimate"], vc["sd(Intercept)", "Est.Error"]),
        "",
        ""
      )
    ))
  }
  
  if (type == "rs") {
    return(tibble(
      term = c("SD(Intercept): Building_Type",
               "SD(log_GFA): Building_Type",
               "Cor(Intercept, log_GFA)"),
      !!model_name := c(
        fmt_est(vc["sd(Intercept)", "Estimate"], vc["sd(Intercept)", "Est.Error"]),
        fmt_est(vc["sd(log_GFA)", "Estimate"], vc["sd(log_GFA)", "Est.Error"]),
        fmt_est(vc["cor(Intercept,log_GFA)", "Estimate"], vc["cor(Intercept,log_GFA)", "Est.Error"])
      )
    ))
  }
}

get_fit_block <- function(fit, model_name) {
  sp <- suppressWarnings(summary(fit)$spec_pars)
  r2 <- suppressWarnings(bayes_R2(fit)[1, "Estimate"])
  
  tibble(
    term = c("Observations", "Residual SD (sigma)", "Bayesian R2"),
    !!model_name := c(
      nobs(fit),
      fmt_est(sp["sigma", "Estimate"], sp["sigma", "Est.Error"]),
      round(r2, 2)
    )
  )
}

# -----------------------------
# ENERGY TABLE (E1, E2, E3)
# -----------------------------
energy_fixed <- reduce(list(
  get_fixed_block(E1, "E1"),
  get_fixed_block(E2, "E2"),
  get_fixed_block(E3, "E3")
), full_join, by = "term")

energy_group <- reduce(list(
  get_group_block(E1, "E1", "baseline"),
  get_group_block(E2, "E2", "ri"),
  get_group_block(E3, "E3", "rs")
), full_join, by = "term")

energy_fit <- reduce(list(
  get_fit_block(E1, "E1"),
  get_fit_block(E2, "E2"),
  get_fit_block(E3, "E3")
), full_join, by = "term")

table_energy <- bind_rows(
  tibble(term = "=== Fixed effects ===", E1 = "", E2 = "", E3 = ""),
  energy_fixed,
  tibble(term = "=== Group-level parameters ===", E1 = "", E2 = "", E3 = ""),
  energy_group,
  tibble(term = "=== Model fit ===", E1 = "", E2 = "", E3 = ""),
  energy_fit
)

write_csv(table_energy, "../output/table2_energy_models.csv")

# -----------------------------
# CARBON TABLE (C1, C2, C3)
# -----------------------------
carbon_fixed <- reduce(list(
  get_fixed_block(C1, "C1"),
  get_fixed_block(C2, "C2"),
  get_fixed_block(C3, "C3")
), full_join, by = "term")

carbon_group <- reduce(list(
  get_group_block(C1, "C1", "baseline"),
  get_group_block(C2, "C2", "ri"),
  get_group_block(C3, "C3", "rs")
), full_join, by = "term")

carbon_fit <- reduce(list(
  get_fit_block(C1, "C1"),
  get_fit_block(C2, "C2"),
  get_fit_block(C3, "C3")
), full_join, by = "term")

table_carbon <- bind_rows(
  tibble(term = "=== Fixed effects ===", C1 = "", C2 = "", C3 = ""),
  carbon_fixed,
  tibble(term = "=== Group-level parameters ===", C1 = "", C2 = "", C3 = ""),
  carbon_group,
  tibble(term = "=== Model fit ===", C1 = "", C2 = "", C3 = ""),
  carbon_fit
)

write_csv(table_carbon, "../output/table3_carbon_models.csv")

cat("Table 2 (Energy models) saved.\n")
cat("Table 3 (Carbon models) saved.\n")

# ============================================================
# 10. MODEL SUMMARIES & RANDOM EFFECTS
# ============================================================

cat("\n=== ENERGY MODEL E2 SUMMARY ===\n"); print(summary(E2))
cat("\n=== CARBON MODEL C2 SUMMARY ===\n"); print(summary(C2))
cat("\n--- Random Effects: Energy E2 ---\n"); print(ranef(E2))
cat("\n--- Random Effects: Carbon C2 ---\n"); print(ranef(C2))

# ============================================================

cat("\n=== ENERGY MODEL E3 SUMMARY ===\n")
suppressWarnings(print(summary(E3)))
cat("\n=== CARBON MODEL C3 SUMMARY ===\n")
suppressWarnings(print(summary(C3)))
cat("\n--- Random Effects: Energy E3 ---\n"); print(ranef(E3))
cat("\n--- Random Effects: Carbon C3 ---\n"); print(ranef(C3))

# ============================================================
# 11. FIGURE 1: Caterpillar Plot (E2/C2)
# ============================================================
# Random intercepts from E2/C2 used for stable type-level interpretation.
# Points to the right of zero indicate above-baseline energy/carbon intensity.
# Error bars show 95% credible intervals.

re_energy <- ranef(E2)$Building_Type[,,"Intercept"] %>%
  as.data.frame() %>%
  rownames_to_column("Building_Type") %>%
  rename(estimate = Estimate, lower = Q2.5, upper = Q97.5) %>%
  mutate(track = "Energy (log EUI)")

re_carbon <- ranef(C2)$Building_Type[,,"Intercept"] %>%
  as.data.frame() %>%
  rownames_to_column("Building_Type") %>%
  rename(estimate = Estimate, lower = Q2.5, upper = Q97.5) %>%
  mutate(track = "Carbon (log GHG Intensity)")

re_combined <- bind_rows(re_energy, re_carbon) %>%
  mutate(Building_Type = fct_reorder(Building_Type, estimate))

caterpillar <- ggplot(re_combined,
                      aes(x = estimate, y = Building_Type, color = track)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(y = Building_Type, xmin = lower, xmax = upper), 
                orientation = "y", width = 0.2) +
  geom_point(size = 3) +
  facet_wrap(~track, scales = "free_x") +
  scale_color_manual(values = c(
    "Energy (log EUI)"           = "#E05C1A",
    "Carbon (log GHG Intensity)" = "#1A6EB5")) +
  labs(
    title    = "Random Intercepts by Building Type",
    subtitle = "Deviation from portfolio baseline after controlling for GFA, operating hours, and year",
    x = "Estimated Deviation from Baseline",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

ggsave("../output/718_fig1_caterpillar_random_intercepts_E2C2.png",
       plot = caterpillar, width = 10, height = 5, dpi = 300)
cat("Figure 1 (caterpillar E2/C2) saved.\n")

# ============================================================
# 12. FIGURE 2: Caterpillar Plot (E3/C3) — Primary Model
# ============================================================
# Random intercepts from the primary varying-slope models (E3/C3).
# Larger points indicate 95% CI excludes zero (credible deviation).

re_energy_E3 <- ranef(E3)$Building_Type[,,"Intercept"] %>%
  as.data.frame() %>%
  rownames_to_column("Building_Type") %>%
  rename(estimate = Estimate, lower = Q2.5, upper = Q97.5) %>%
  mutate(track = "Energy (log EUI)",
         significant = !(lower < 0 & upper > 0))

re_carbon_C3 <- ranef(C3)$Building_Type[,,"Intercept"] %>%
  as.data.frame() %>%
  rownames_to_column("Building_Type") %>%
  rename(estimate = Estimate, lower = Q2.5, upper = Q97.5) %>%
  mutate(track = "Carbon (log GHG Intensity)",
         significant = !(lower < 0 & upper > 0))

re_combined_best <- bind_rows(re_energy_E3, re_carbon_C3) %>%
  mutate(Building_Type = fct_reorder(Building_Type, estimate))

caterpillar_best <- ggplot(re_combined_best,
                           aes(x = estimate, y = Building_Type, color = track)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(y = Building_Type, xmin = lower, xmax = upper), 
                orientation = "y", width = 0.2) +
  geom_point(aes(size = significant)) +
  scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2.5), guide = "none") +
  facet_wrap(~track, scales = "free_x") +
  scale_color_manual(values = c(
    "Energy (log EUI)"           = "#E05C1A",
    "Carbon (log GHG Intensity)" = "#1A6EB5")) +
  labs(
    title    = "Random Intercepts by Building Type (E3 & C3)",
    subtitle = "Larger points indicate 95% CI excludes zero (statistically credible deviation)",
    x = "Estimated Deviation from Portfolio Baseline",
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        strip.text = element_text(face = "bold"))

ggsave("../output/718_fig2_caterpillar_random_intercepts_E3C3.png",
       caterpillar_best, width = 10, height = 5, dpi = 300)
cat("Figure 2 (caterpillar E3/C3) saved.\n")

# ============================================================
# 13. RESIDUAL ANALYSIS
# ============================================================
# Identifies buildings that underperform beyond what building type,
# size, operating hours, and year can explain.
# residual = observed log(outcome) - fitted value from E2/C2

df$fitted_EUI <- fitted(E2)[, "Estimate"]
df$resid_EUI  <- df$log_EUI - df$fitted_EUI
df$fitted_GHG <- fitted(C2)[, "Estimate"]
df$resid_GHG  <- df$log_GHG_intensity - df$fitted_GHG

cat("\n--- Top 10 Energy Underperformers (2024) ---\n")
df %>% filter(Year == 2024) %>%
  arrange(desc(resid_EUI)) %>%
  select(Property_Name, Building_Type, EUI_ekWh_sqft, resid_EUI) %>%
  head(10) %>% print()

cat("\n--- Top 10 Carbon Underperformers (2024) ---\n")
df %>% filter(Year == 2024) %>%
  arrange(desc(resid_GHG)) %>%
  select(Property_Name, Building_Type, GHG_Intensity_kg_sqft, resid_GHG) %>%
  head(10) %>% print()

cat("\n--- Persistent Underperformers (>=3 years) ---\n")
df %>%
  group_by(Property_Name, Building_Type) %>%
  summarise(
    n_years        = n(),
    mean_resid_EUI = mean(resid_EUI),
    mean_resid_GHG = mean(resid_GHG),
    .groups = "drop"
  ) %>%
  filter(n_years >= 3) %>%
  arrange(desc(mean_resid_EUI)) %>%
  head(10) %>% print()

# ============================================================

df$fitted_EUI <- fitted(E3)[, "Estimate"]
df$resid_EUI  <- df$log_EUI - df$fitted_EUI
df$fitted_GHG <- fitted(C3)[, "Estimate"]
df$resid_GHG  <- df$log_GHG_intensity - df$fitted_GHG

cat("\n--- Top 10 Energy Underperformers (2024) ---\n")
df %>% filter(Year == 2024) %>%
  arrange(desc(resid_EUI)) %>%
  select(Property_Name, Building_Type, EUI_ekWh_sqft, resid_EUI) %>%
  head(10) %>% print()

cat("\n--- Top 10 Carbon Underperformers (2024) ---\n")
df %>% filter(Year == 2024) %>%
  arrange(desc(resid_GHG)) %>%
  select(Property_Name, Building_Type, GHG_Intensity_kg_sqft, resid_GHG) %>%
  head(10) %>% print()

cat("\n--- Persistent Underperformers (>=3 years) ---\n")
df %>%
  group_by(Property_Name, Building_Type) %>%
  summarise(
    n_years        = n(),
    mean_resid_EUI = mean(resid_EUI),
    mean_resid_GHG = mean(resid_GHG),
    .groups = "drop"
  ) %>%
  filter(n_years >= 3) %>%
  arrange(desc(mean_resid_EUI)) %>%
  head(10) %>% print()

# ============================================================
# 14. EDA FIGURE: EUI Boxplot by Building Type
# ============================================================
# Shows raw (unadjusted) EUI distribution across building types.
# Motivates the need for multilevel modelling by illustrating
# that naive comparisons do not account for size and operating hours.

df_eda <- read_csv(
  "../cleaned data/merged dataset/Toronto_Master_Panel_Ready_v2.csv",
  show_col_types = FALSE
)

required_cols <- c("Building_Type", "EUI_ekWh_sqft")
missing_cols  <- setdiff(required_cols, names(df_eda))
if (length(missing_cols) > 0) {
  stop(paste0("Missing columns: ", paste(missing_cols, collapse = ", ")))
}

fig_eda_boxplot <- df_eda %>%
  filter(!is.na(Building_Type), !is.na(EUI_ekWh_sqft)) %>%
  mutate(Building_Type = fct_reorder(Building_Type, EUI_ekWh_sqft, median, .desc = FALSE)) %>%
  ggplot(aes(x = Building_Type, y = EUI_ekWh_sqft)) +
  geom_boxplot(fill = "#A8D0E6", color = "#2C3E50", width = 0.65, outlier.alpha = 0.35) +
  labs(
    title = "EUI Distribution by Building Type",
    x = "Building Type",
    y = "EUI (ekWh/sqft)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.text.x = element_text(angle = 20, hjust = 1),
    panel.grid.minor = element_blank()
  )

print(fig_eda_boxplot)

ggsave(
  filename = "../output/718_figEDA_boxplot_EUI_by_buildingtype.png",
  plot     = fig_eda_boxplot,
  width = 10, height = 6, dpi = 300
)
cat("EDA boxplot saved.\n")

# ============================================================
# 15. FIGURE3: Adjusted Excess GHG vs Total Emissions
# ============================================================
# X-axis: adjusted excess GHG intensity (%) derived from C3 residuals.
#         Positive values = building type emits more carbon than
#         expected after controlling for size, hours, and year.
# Y-axis: total panel-level CO2e emissions (electricity + natural gas).
# Quadrant labels identify policy priority categories.
# Right upper quadrant = priority decarbonization target.

required_cols <- c("Building_Type", "log_GHG_intensity",
                   "Electric_Grid_kWh", "Natural_Gas_m3", "ELEC_factor_used")
missing_cols  <- setdiff(required_cols, names(df))
if (length(missing_cols) > 0) {
  stop(paste0("Missing columns: ", paste(missing_cols, collapse = ", ")))
}

df_quadrant <- df %>%
  mutate(
    fitted_GHG_C3      = fitted(C3)[, "Estimate"],
    resid_GHG_C3       = log_GHG_intensity - fitted_GHG_C3,
    Total_Emissions_kg = Electric_Grid_kWh * ELEC_factor_used + Natural_Gas_m3 * 1.921
  )

quad_data <- df_quadrant %>%
  group_by(Building_Type) %>%
  summarise(
    Adjusted_Excess_GHG_pct = 100 * (exp(mean(resid_GHG_C3, na.rm = TRUE)) - 1),
    Total_Emissions_kg      = sum(Total_Emissions_kg, na.rm = TRUE),
    n_obs                   = n(),
    .groups = "drop"
  )

x_mid <- median(quad_data$Adjusted_Excess_GHG_pct, na.rm = TRUE)
y_mid <- median(quad_data$Total_Emissions_kg, na.rm = TRUE)

x_rng   <- range(quad_data$Adjusted_Excess_GHG_pct, na.rm = TRUE)
y_rng   <- range(quad_data$Total_Emissions_kg, na.rm = TRUE)
x_left  <- x_rng[1] + 0.18 * diff(x_rng)
x_right <- x_rng[1] + 0.82 * diff(x_rng)
y_low   <- y_rng[1] + 0.18 * diff(y_rng)
y_high  <- y_rng[1] + 0.82 * diff(y_rng)

fig_appendix_quadrant_C3 <- ggplot(
  quad_data,
  aes(x = Adjusted_Excess_GHG_pct, y = Total_Emissions_kg)
) +
  geom_vline(xintercept = x_mid, linetype = "dashed", color = "grey50") +
  geom_hline(yintercept = y_mid, linetype = "dashed", color = "grey50") +
  geom_point(size = 4, color = "#1A6EB5", alpha = 0.9) +
  geom_text(aes(label = Building_Type), vjust = -0.8, size = 3.8) +
  annotate("text", x = x_right, y = y_high,
           label = "Priority\ndecarbonization\ntarget",
           color = "grey35", size = 3.6, fontface = "bold", lineheight = 0.95) +
  annotate("text", x = x_right, y = y_low,
           label = "Inefficient\nbut small",
           color = "grey35", size = 3.6, fontface = "bold", lineheight = 0.95) +
  annotate("text", x = x_left, y = y_high,
           label = "Large but\nrelatively efficient",
           color = "grey35", size = 3.6, fontface = "bold", lineheight = 0.95) +
  annotate("text", x = x_left, y = y_low,
           label = "Low priority",
           color = "grey35", size = 3.6, fontface = "bold", lineheight = 0.95) +
  scale_y_continuous(labels = label_number(big.mark = ",", accuracy = 1)) +
  labs(
    title    = "Adjusted Excess GHG Intensity vs Total Emissions",
    subtitle = "Based on C3 residuals; each point represents a building type aggregated across the panel",
    x = "Adjusted Excess GHG Intensity (%)",
    y = "Total Emissions (kg CO2e, panel total)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title    = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.minor = element_blank()
  )

print(fig_appendix_quadrant_C3)

ggsave(
  filename = "../output/718_fig3_GHG_excess_vs_total_emissions_C3.png",
  plot     = fig_appendix_quadrant_C3,
  width = 10, height = 6, dpi = 300
)
cat(" quadrant figure3 saved.\n")

# ============================================================
# DONE — all outputs saved to ../output/
# ============================================================
cat("\n============================================================\n")
cat("All outputs saved to ../output/:\n")
cat("  MAIN:\n")
cat("    718_fig1_caterpillar_random_intercepts_E2C2.png\n")
cat("    718_fig2_caterpillar_random_intercepts_E3C3.png\n")
cat("    718_fig2_posterior_ppc_E2C2.png\n")
cat("    718_table1_loo_model_comparison.csv\n")
cat("  SUPPLEMENTARY:\n")
cat("    718_prior_ppc_energy_carbon.png\n")
cat("    718_diagnostics_E2_trace_energy.png\n")
cat("    718_diagnostics_C2_trace_carbon.png\n")
cat("    718_figEDA_boxplot_EUI_by_buildingtype.png\n")
cat("    718_figAppendix_GHG_excess_vs_total_emissions_C3.png\n")
cat("  CACHED MODELS (.rds):\n")
cat("    E1/E2/E3/C1/C2/C3/E_prior/C_prior\n")
cat("============================================================\n")
