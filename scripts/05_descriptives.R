# 05_descriptives.R
# -----------------------------------------------------------
# Descriptives for master dataset (tpt anchor)
# Ensure you’ve run 04_clean_merge.R so data_clean/master_dataset.rds exists.
# -----------------------------------------------------------

source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R"))


# 0) Load data ---------------------------------------------------------------
master <- readRDS(here::here("data_clean", "master_dataset.rds"))
dir.create(here::here("outputs", "descriptives"), recursive = TRUE, showWarnings = FALSE)
dir.create(here::here("outputs", "figures"),      recursive = TRUE, showWarnings = FALSE)

# 0.1) Helper functions ------------------------------------------------------
only_numeric <- function(df) dplyr::select(df, where(is.numeric))
nz_na <- function(x) sum(is.na(x))
z_score <- function(x) if (all(is.na(x))) x else (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

message("Master rows: ", nrow(master), " | cols: ", ncol(master))


# 2) Missingness overview ----------------------------------------------------
missing_tbl <- tibble::tibble(
  variable    = names(master),
  n_missing   = vapply(master, nz_na, numeric(1)),
  pct_missing = round(100 * n_missing / nrow(master), 1),
  class       = vapply(master, function(x) paste(class(x), collapse = ","), character(1))
) %>% dplyr::arrange(dplyr::desc(pct_missing), variable)

writexl::write_xlsx(missing_tbl, here::here("outputs", "descriptives", "01_missingness_by_variable.xlsx"))

# 3) Global descriptives (numeric only) --------------------------------------
num_df <- only_numeric(master)
desc_global <- psych::describe(num_df)
desc_global$variable <- rownames(desc_global); rownames(desc_global) <- NULL
writexl::write_xlsx(desc_global, here::here("outputs", "descriptives", "02_describe_global_numeric.xlsx"))

# 4) Descriptives by income group -------------------------------------------
# Your income group lives in Third Pillar: World.Bank.Income.Group_tpt
income_var <- "World.Bank.Income.Group_tpt"
if (!income_var %in% names(master)) {
  warning("Income group variable (World.Bank.Income.Group_tpt) not found. Skipping grouped descriptives.")
} else {
  groups <- sort(unique(master[[income_var]]))
  for (g in groups) {
    chunk <- master %>% dplyr::filter(.data[[income_var]] == g) %>% only_numeric()
    if (nrow(chunk) > 0 && ncol(chunk) > 0) {
      d <- psych::describe(chunk)
      d$variable <- rownames(d); rownames(d) <- NULL
      out <- here::here("outputs", "descriptives", paste0("03_describe_numeric_", g, ".xlsx"))
      writexl::write_xlsx(d, out)
    }
  }
}

# 5) POLARIZATION subset (all incomes) --------------------------------------
# keep Country, third pillar (Total_tpt), polarization (%)
polarization_all <- master %>%
  dplyr::select(
    Country,
    Third_pillar_size = Total_tpt,
    Poli_polarization = Poli_polarization_pct,
    World_bank_income_group = World.Bank.Income.Group_tpt
  ) %>%
  stats::na.omit()

# Console descriptives 
summary(polarization_all)
sd(polarization_all$Third_pillar_size)
sd(polarization_all$Poli_polarization)

# Scatter with linear fit (all incomes)
p_all <- plot_thirdpillar_vs_polarization(
  df = polarization_all,
  title = "Third Pillar vs Political Polarization (All Incomes)",
  outfile = here::here("outputs/figures/scatter_thirdpillar_polarization_all_incomes.png")
)


# 6) Income-group splits and plots ------------------------------------------
upper_high_levels <- c("Upper-middle income", "High income")
lower_low_levels  <- c("Low income", "Lower-middle income")

upper_high <- master %>% dplyr::filter(.data[[income_var]] %in% upper_high_levels)
lower_low  <- master %>% dplyr::filter(.data[[income_var]] %in% lower_low_levels)

# a) Upper-middle+High
pol_upper <- upper_high %>%
  dplyr::select(Country,
                Third_pillar_size = Total_tpt,
                Poli_polarization = Poli_polarization_pct) %>%
  stats::na.omit()

summary(pol_upper)
if (nrow(pol_upper) > 2) {
  lm_u <- lm(Poli_polarization ~ Third_pillar_size, data = pol_upper)
  capture.output(summary(lm_u),
                 file = here::here("outputs","descriptives","04_lm_upper_high.txt"))
}
p_upper <- plot_thirdpillar_vs_polarization(
  df = pol_upper,
  title = "High- & Upper-Middle-Income Countries",
  outfile = here::here("outputs/figures/scatter_thirdpillar_polarization_upper_high.png")
)


# b) Low+Lower-middle
pol_lower <- lower_low %>%
  dplyr::select(Country,
                Third_pillar_size = Total_tpt,
                Poli_polarization = Poli_polarization_pct) %>%
  stats::na.omit()

summary(pol_lower)
if (nrow(pol_lower) > 2) {
  lm_l <- lm(Poli_polarization ~ Third_pillar_size, data = pol_lower)
  capture.output(summary(lm_l),
                 file = here::here("outputs","descriptives","05_lm_low_lower.txt"))
}
p_lower <- plot_thirdpillar_vs_polarization(
  df = pol_lower,
  title = "Low- & Lower-Middle-Income Countries",
  outfile = here::here("outputs/figures/scatter_thirdpillar_polarization_low_lower.png")
)

income_plot <- ggplot(polarization_all, aes(x = Third_pillar_size, y = Poli_polarization)) +
  geom_point(shape = 20, aes(color = World_bank_income_group)) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5, aes(color = World_bank_income_group)) +
  labs(x = "Third Sector Size (%)", y = "Political Polarization (%)", color = "Income Group") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = c(0.8,0.8)) 
ggsave(here::here("outputs/figures/scatter_thirdpillar_polarization_per_income_group.png"), plot = income_plot)


# 7) MEDIATION subset (Upper + High income) ----------------------------------
# Third_pillar_size  -> Total_tpt
# Poli_polarization  -> Poli_polarization_pct (derived above)
# Trust_ivs          -> Agree.Most.people.can.be.trusted_trust_ivs
# Gini_index         -> Gini_index_gini
# Facebook_users     -> Facebook_users_per100k (normalized, like previous workflows)
# Loneliness         -> Loneliness_loneliness_owid
# GDP_Most_Recent    -> GDP_Most_Recent_gdp_wb

mediation <- upper_high %>%
  dplyr::select(
    Country,
    Third_pillar_size   = Total_tpt,
    Poli_polarization   = Poli_polarization_pct,
    Trust_ivs           = Agree.Most.people.can.be.trusted_trust_ivs,
    Gini_index          = Gini_index_gini,
    Facebook_users      = Facebook_users_per100k_fb,
    Loneliness          = Loneliness_loneliness_owid,
    GDP_Most_Recent     = GDP_Most_Recent_gdp_wb
  ) %>%
  stats::na.omit()

# Descriptives (console)
summary(mediation)
sd(mediation$Third_pillar_size)
sd(mediation$Poli_polarization)
sd(mediation$Gini_index)
sd(mediation$Facebook_users)
sd(mediation$Loneliness)
sd(mediation$Trust_ivs)

descr_mediation <- psych::describe(mediation)
descr_mediation$Variable <- rownames(descr_mediation); rownames(descr_mediation) <- NULL
writexl::write_xlsx(descr_mediation,
                    here::here("outputs","descriptives","06_descriptives_mediation_upper_high.xlsx"))

plot_variable_vs_polarization(
  df      = mediation,
  xvar    = "Trust_ivs",
  xlab    = "Trust in Others (IVS, % agreeing)",
  title   = "Trust vs Political Polarization (All Incomes)",
  outfile = here::here("outputs/figures/scatter_trust_polarization.png")
)

plot_variable_vs_polarization(
  df      = mediation,
  xvar    = "Gini_index",
  xlab    = "Gini Index (Income Inequality)",
  title   = "Income Inequality vs Political Polarization (All Incomes)",
  outfile = here::here("outputs/figures/scatter_gini_polarization.png")
)

plot_variable_vs_polarization(
  df      = mediation,
  xvar    = "Loneliness",
  xlab    = "One-Person Household Share (%)",
  title   = "Loneliness Proxy vs Political Polarization (All Incomes)",
  outfile = here::here("outputs/figures/scatter_loneliness_polarization.png")
)

plot_variable_vs_polarization(
  df      = mediation,
  xvar    = "Facebook_users",
  xlab    = "Facebook Users per 100k Population",
  title   = "Facebook Use vs Political Polarization (All Incomes)",
  outfile = here::here("outputs/figures/scatter_facebook_polarization.png")
)


# 8) Z-scores for mediation subset ------------------------------------------
mediation_z <- mediation %>% dplyr::mutate(dplyr::across(where(is.numeric), z_score))

descr_mediation_z <- psych::describe(mediation_z)
descr_mediation_z$Variable <- rownames(descr_mediation_z); rownames(descr_mediation_z) <- NULL
writexl::write_xlsx(descr_mediation_z,
                    here::here("outputs","descriptives","07_descriptives_mediation_upper_high_zscores.xlsx"))

# -----------------------
# Save analysis-ready subsets for reuse
# -----------------------
dir.create(here::here("data_clean", "analysis_subsets"), showWarnings = FALSE, recursive = TRUE)

saveRDS(polarization_all, here::here("data_clean", "analysis_subsets", "polarization_all.rds"))
saveRDS(pol_upper,        here::here("data_clean", "analysis_subsets", "pol_upper.rds"))
saveRDS(pol_lower,        here::here("data_clean", "analysis_subsets", "pol_lower.rds"))
saveRDS(polarization_gdp, here::here("data_clean", "analysis_subsets", "polarization_gdp.rds"))

saveRDS(mediation,        here::here("data_clean", "analysis_subsets", "mediation_dataset.rds"))
saveRDS(mediation_z,      here::here("data_clean", "analysis_subsets", "mediation_dataset_z.rds"))

# 9) Correlations for mediation subset ---------------------------------------
# Note: pairwise complete obs, Pearson 
if (ncol(only_numeric(mediation)) >= 2) {
  cor_m <- psych::corr.test(only_numeric(mediation), use = "pairwise", method = "pearson")
  writexl::write_xlsx(
    list(
      "cor_matrix" = as.data.frame(cor_m$r),
      "p_values"   = as.data.frame(cor_m$p)
    ),
    here::here("outputs","descriptives","08_corr_mediation_upper_high.xlsx")
  )
}

# 10) Boxplots with outlier labels (z-scores) ----------------------
# (Helpful for spotting countries driving results)
if (requireNamespace("reshape2", quietly = TRUE)) {
  med_long <- melt(mediation_z, id.vars = "Country")
  
  is_outlier <- function(x) {
    x < (quantile(x, 0.25, na.rm = TRUE) - 1.5 * IQR(x, na.rm = TRUE)) |
      x > (quantile(x, 0.75, na.rm = TRUE) + 1.5 * IQR(x, na.rm = TRUE))
  }
  
  med_long$outlier <- ave(med_long$value, med_long$variable, FUN = function(v) {
    if (all(is.na(v))) return(rep(FALSE, length(v)))
    is_outlier(v)
  })
  
  p_box <- ggplot2::ggplot(med_long, ggplot2::aes(x = variable, y = value)) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_text(
      ggplot2::aes(label = ifelse(outlier, Country, "")),
      hjust = -0.2, vjust = 0.2, size = 2.3, color = "red"
    ) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
    ggplot2::labs(x = "Variable (z-score)", y = "Value",
                  title = "Boxplots of Mediation Variables (Upper+High) with Outliers")
  ggplot2::ggsave(here::here("outputs","figures","boxplots_mediation_upper_high_z.png"),
                  p_box, width = 9, height = 5.5, dpi = 300)
}

# 11) Quick ESS vs WVS sanity check (if both present) ------------------------
ess_wvs_pair <- c("mean_stflife_ess", "Satisfaction_life_wvs")
if (all(ess_wvs_pair %in% names(master))) {
  pair_df <- master %>% dplyr::select(all_of(ess_wvs_pair)) %>% stats::na.omit()
  if (nrow(pair_df) > 5) {
    r_ess_wvs <- cor(pair_df[[1]], pair_df[[2]], use = "pairwise.complete.obs")
    cat("\nCorrelation(ESS life satisfaction, WVS satisfaction_life) = ",
        round(r_ess_wvs, 3), "\n")
  }
}

# 12) Session info snapshot --------------------------------------------------
sink(here::here("outputs", "descriptives", "99_sessionInfo.txt"))
print(sessionInfo())
sink()

message("Descriptives complete. See outputs/descriptives and outputs/figures.")