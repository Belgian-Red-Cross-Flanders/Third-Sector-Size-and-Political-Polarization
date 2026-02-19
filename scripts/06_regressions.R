# 06_regressions.R
# -----------------------------------------------------------
# - Core model: Polarization (%) ~ Third pillar size
# - By income groups (all / upper+high / low+lower)
# - GDP-adjusted model (all incomes)
# - OLS (ordinary least squares) additions: Trust, Gini, Loneliness, Facebook per 100k (upper+high)
# - Diagnostics & tidy outputs saved to outputs/regressions/
# -----------------------------------------------------------

source(here::here("scripts", "01_load_packages.R"))

# Create output folders
dir.create(here::here("outputs", "regressions"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs", "figures"),      recursive = TRUE, showWarnings = FALSE)


# -----------------------
# Load frozen analysis subsets
# -----------------------
polarization_all <- readRDS(here::here("data_clean", "analysis_subsets", "polarization_all.rds"))
pol_upper        <- readRDS(here::here("data_clean", "analysis_subsets", "pol_upper.rds"))
pol_lower        <- readRDS(here::here("data_clean", "analysis_subsets", "pol_lower.rds"))
polarization_gdp <- readRDS(here::here("data_clean", "analysis_subsets", "polarization_gdp.rds"))

# upper+high mediation table:
med_uh           <- readRDS(here::here("data_clean", "analysis_subsets", "mediation_dataset.rds"))

# Separate income groups
pol_high <- polarization_all %>% dplyr::filter(World_bank_income_group == "High income")
pol_upper_middle <- polarization_all %>% dplyr::filter(World_bank_income_group == "Upper-middle income")
pol_lower_middle <- polarization_all %>% dplyr::filter(World_bank_income_group == "Lower-middle income")
pol_low <- polarization_all %>% dplyr::filter(World_bank_income_group == "Low income")


# Original TPT dataset countries (those with original data, not extrapolated)

complete_51_countries <- c(
  "Egypt", "India", "Kenya", "Pakistan", "Philippines", "Uganda",
  "Argentina", "Australia", "Austria", "Belgium", "Brazil", "Bulgaria",
  "Canada", "Chile", "Colombia", "Croatia", "Cyprus", "Czechia",
  "Denmark", "Estonia", "Finland", "France", "Germany", "Greece",
  "Hungary", "Ireland", "Israel", "Italy", "Japan", "Republic of Korea", "Latvia",
  "Lithuania", "Luxembourg", "Mexico", "Netherlands", "New Zealand",
  "Norway", "Peru", "Poland", "Portugal", "Romania", "Russian Federation",
  "Slovakia", "Slovenia", "South Africa", "Spain", "Sweden",
  "Switzerland", "Turkey", "United Kingdom of Great Britain and Northern Ireland", "United States of America"
)

pol_51 <- polarization_all %>%
  dplyr::filter(Country %in% complete_51_countries)




# 2) Helper: fit model, run diagnostics, export everything --------------------
# + AUTOMATIC INTERPRETATION
save_model_outputs <- function(model, name_prefix, data_used) {
  
  # ------------------------------------------------------------
  # 0. Model components
  # ------------------------------------------------------------
  tdy <- broom::tidy(model, conf.int = TRUE)
  gln <- broom::glance(model)
  aug <- broom::augment(model)
  
  # Extract main effect (first non-intercept)
  main_term <- tdy$term[tdy$term != "(Intercept)"][1]
  main_est  <- tdy$estimate[tdy$term == main_term]
  main_p    <- tdy$p.value[tdy$term == main_term]
  main_ci_l <- tdy$conf.low[tdy$term == main_term]
  main_ci_u <- tdy$conf.high[tdy$term == main_term]
  r2        <- gln$r.squared
  
  # ------------------------------------------------------------
  # 1. Regression assumption tests
  # ------------------------------------------------------------
  shapiro_p <- tryCatch(shapiro.test(residuals(model))$p.value,
                        error = function(e) NA_real_)
  dw        <- tryCatch(car::durbinWatsonTest(model), error = function(e) NA)
  bp        <- tryCatch(lmtest::bptest(model),        error = function(e) NA)
  
  dw_stat <- ifelse(is.list(dw), dw$dw, NA)
  dw_p    <- ifelse(is.list(dw), dw$p, NA)
  bp_stat <- ifelse(is.list(bp), bp$statistic, NA)
  bp_p    <- ifelse(is.list(bp), bp$p.value, NA)
  
  # ------------------------------------------------------------
  # 2. Assumption Interpretation Logic
  # ------------------------------------------------------------
  interp_normality <- if (is.na(shapiro_p)) {
    "Normality test unavailable."
  } else if (shapiro_p > 0.05) {
    "Residuals appear normally distributed (Shapiro p > 0.05)."
  } else {
    "Residuals deviate from normality (Shapiro p < 0.05)."
  }
  
  interp_dw <- if (is.na(dw_stat)) {
    "Durbin–Watson test unavailable."
  } else if (dw_stat > 1.5 & dw_stat < 2.5) {
    paste0("Residuals show no evidence of autocorrelation (DW = ",
           round(dw_stat, 3), ").")
  } else {
    paste0("Possible autocorrelation detected (DW = ",
           round(dw_stat, 3), ").")
  }
  
  interp_bp <- if (is.na(bp_p)) {
    "Breusch–Pagan test unavailable."
  } else if (bp_p > 0.05) {
    "No evidence of heteroscedasticity (BP p > 0.05)."
  } else {
    "Heteroscedasticity detected (BP p < 0.05)."
  }
  
  # ------------------------------------------------------------
  # 3. Automatic Plain‑Language Interpretation
  # ------------------------------------------------------------
  effect_direction <- ifelse(main_est > 0, "increases", "decreases")
  
  effect_strength <- if (abs(main_est) < 0.5) {
    "a very small effect"
  } else if (abs(main_est) < 1.5) {
    "a modest effect"
  } else if (abs(main_est) < 3) {
    "a strong effect"
  } else {
    "a very strong effect"
  }
  
  sig_text <- if (main_p < 0.001) {
    "highly statistically significant (p < 0.001)"
  } else if (main_p < 0.05) {
    "statistically significant (p < 0.05)"
  } else {
    "not statistically significant (p ≥ 0.05)"
  }
  
  auto_interpretation <- paste0(
    "\n\n=== AUTOMATIC INTERPRETATION ===\n",
    "Main predictor: ", main_term, "\n",
    "Effect size: ", round(main_est, 3), " (",
    effect_strength, ")\n",
    "Interpretation: A one-unit increase in ", main_term, 
    " is associated with a ",
    abs(round(main_est, 3)), "-point change in the outcome.\n",
    "Direction: The predictor ", effect_direction, " the outcome.\n",
    "Significance: The effect is ", sig_text, ".\n",
    "R-squared: The model explains ", round(r2 * 100, 1),
    "% of variance.\n\n",
    "=== ASSUMPTION CHECKS ===\n",
    "- Normality: ", interp_normality, "\n",
    "- Independence: ", interp_dw, "\n",
    "- Homoscedasticity: ", interp_bp, "\n",
    "\n--------------------------------------------\n",
    "If all assumptions are met: coefficients, SEs, ",
    "and p-values are trustworthy.\n",
    "If assumptions are violated: consider robust SEs, ",
    "transformations, or alternative models.\n"
  )
  
  # ------------------------------------------------------------
  # 4. Export to Excel
  # ------------------------------------------------------------
  out_xlsx <- here::here("outputs","regressions", paste0(name_prefix, "_tables.xlsx"))
  
  writexl::write_xlsx(
    list(
      tidy   = tdy,
      glance = gln,
      augment = aug,
      infer = data.frame(
        test = c("Shapiro-Wilk (normality)",
                 "Durbin-Watson (autocorrelation)",
                 "Breusch-Pagan (heteroscedasticity)"),
        statistic = c(NA, dw_stat, bp_stat),
        p_value = c(shapiro_p, dw_p, bp_p),
        reference = c(
          "> 0.05 desirable",
          "≈ 2.0 indicates independence",
          "> 0.05 desirable"
        )
      )
    ),
    path = out_xlsx
  )
  
  # ------------------------------------------------------------
  # 5. Write Summary + Interpretation to TXT
  # ------------------------------------------------------------
  out_txt <- here::here("outputs","regressions", paste0(name_prefix, "_summary.txt"))
  capture.output({
    cat("\n=== MODEL SUMMARY ===\n")
    print(summary(model))
    
    cat("\n=== DIAGNOSTICS ===\n")
    print(paste("Shapiro-Wilk p =", round(shapiro_p, 4)))
    if (is.list(dw)) print(dw)
    if (is.list(bp)) print(bp)
    
    cat(auto_interpretation)
  }, file = out_txt)
  
  # ------------------------------------------------------------
  # 6. Diagnostic Plots
  # ------------------------------------------------------------
  
  # Residuals vs Fitted
  p1 <- ggplot2::ggplot(aug, aes(.fitted, .resid)) +
    ggplot2::geom_point(shape = 20) +
    ggplot2::geom_hline(yintercept = 0, color = "red", linewidth = 0.5) +
    ggplot2::labs(
      x = "Fitted values",
      y = "Residuals",
      title = paste0(name_prefix, ": Residuals vs Fitted")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      plot.title   = ggplot2::element_text(hjust = 0.5)
    )
  
  ggplot2::ggsave(
    here::here("outputs","figures", paste0(name_prefix, "_residuals_vs_fitted.png")),
    p1, width = 7.2, height = 4.8, dpi = 300
  )
  
  # QQ plot
  qq_df <- data.frame(sample = residuals(model))
  
  p2 <- ggplot2::ggplot(qq_df, aes(sample = sample)) +
    ggplot2::stat_qq(shape = 20) +
    ggplot2::stat_qq_line(color = "red", linewidth = 0.5) +
    ggplot2::labs(
      title = paste0(name_prefix, ": QQ plot of residuals")
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.border = ggplot2::element_blank(),
      plot.title   = ggplot2::element_text(hjust = 0.5)
    )
  
  ggplot2::ggsave(
    here::here("outputs","figures", paste0(name_prefix, "_qqplot.png")),
    p2, width = 7.2, height = 4.8, dpi = 300
  )
  
  invisible(TRUE)
}


# 3) Core regressions ---------------------------------------------------------

# A1) All incomes: Poli_polarization ~ Third_pillar_size
if (nrow(polarization_all) >= 3) {
  m_all <- lm(Poli_polarization ~ Third_pillar_size, data = polarization_all)
  save_model_outputs(m_all, "A1_all_incomes_simple", polarization_all)
}

# A2) Upper + High
if (nrow(pol_upper) >= 3) {
  m_upper <- lm(Poli_polarization ~ Third_pillar_size, data = pol_upper)
  save_model_outputs(m_upper, "A2_upper_high_simple", pol_upper)
}

# A3) Low + Lower-middle
if (nrow(pol_lower) >= 3) {
  m_lower <- lm(Poli_polarization ~ Third_pillar_size, data = pol_lower)
  save_model_outputs(m_lower, "A3_low_lower_simple", pol_lower)
}

# 4) GDP-adjusted (all incomes) ----------------------------------------------
if (nrow(polarization_gdp) >= 3) {
  m_gdp <- lm(Poli_polarization ~ Third_pillar_size + GDP_Most_Recent, data = polarization_gdp)
  save_model_outputs(m_gdp, "B1_all_incomes_gdp_adjusted", polarization_gdp)
}

# 5) Single-mediator style OLS (Upper+High) ----------------------------------
add_and_fit <- function(varname, nice) {
  df <- med_uh %>% dplyr::select(Country, Third_pillar_size, Poli_polarization, any_of(varname)) %>% stats::na.omit()
  if (nrow(df) < 3) return(invisible(NULL))
  f <- as.formula(paste("Poli_polarization ~ Third_pillar_size +", varname))
  m <- lm(f, data = df)
  save_model_outputs(m, paste0("C_", nice, "_UH"), df)
}

# 6) 51 non-inputed countries
if (nrow(pol_51) >= 3) {
  m_51 <- lm(Poli_polarization ~ Third_pillar_size, data = pol_51)
  save_model_outputs(m_51, "D1_full_51countries", pol_51)
}

# 7) Individual income groups

if (nrow(pol_high) >= 3) {
  m_high <- lm(Poli_polarization ~ Third_pillar_size, data = pol_high)
  save_model_outputs(m_high, "D2_high_income", pol_high)
}

if (nrow(pol_upper_middle) >= 3) {
  m_upmid <- lm(Poli_polarization ~ Third_pillar_size, data = pol_upper_middle)
  save_model_outputs(m_upmid, "D3_upper_middle_income", pol_upper_middle)
}

if (nrow(pol_lower_middle) >= 3) {
  m_lowmid <- lm(Poli_polarization ~ Third_pillar_size, data = pol_lower_middle)
  save_model_outputs(m_lowmid, "D4_lower_middle_income", pol_lower_middle)
}

if (nrow(pol_low) >= 3) {
  m_low <- lm(Poli_polarization ~ Third_pillar_size, data = pol_low)
  save_model_outputs(m_low, "D5_low_income", pol_low)
}



add_and_fit("Trust_ivs",      "trust")
add_and_fit("Gini_index",     "gini")
add_and_fit("Loneliness",     "loneliness")
add_and_fit("Facebook_users", "facebook_per100k")

# 6) Session info snapshot ----------------------------------------------------
sink(here::here("outputs","regressions","99_sessionInfo.txt"))
print(sessionInfo())
sink()

message("Regressions complete. See outputs/regressions and outputs/figures.")