# 07_mediation_psych.R
# -----------------------------------------------------------
# using psych::mediate
# on the frozen z-scored upper+high mediation dataset.
# - trust, trust_gdp
# - gini, gini_gdp
# - facebookusers, facebookusers_gdp
# - loneliness, lonelieness_gdp
# - all_together
# -----------------------------------------------------------

source(here::here("scripts", "01_load_packages.R"))

# Output folder
dir.create(here::here("outputs", "mediation_psych"),
           recursive = TRUE, showWarnings = FALSE)

# Load frozen z-score data (created in 05_descriptives.R)
mediation_zscore <- readRDS(
  here::here("data_clean", "analysis_subsets", "mediation_dataset_z.rds")
)

# Expected columns (z-scored):
#   Country
#   Third_pillar_size, Poli_polarization
#   Trust_ivs, Gini_index, Loneliness, Facebook_users
#   GDP_Most_Recent

# Helper to print + save both text and RDS
save_psych_med <- function(obj, name) {
  out_txt <- here::here("outputs", "mediation_psych", paste0(name, ".txt"))
  capture.output(print(obj, digits = 4, short = FALSE), file = out_txt)
  
  out_rds <- here::here("outputs", "mediation_psych", paste0(name, ".rds"))
  saveRDS(obj, out_rds)
}

# -----------------------------
# TRUST
# -----------------------------
trust <- psych::mediate(
  Poli_polarization ~ Third_pillar_size + (Trust_ivs),
  data = mediation_zscore
)
save_psych_med(trust, "trust")

trust_gdp <- psych::mediate(
  Poli_polarization ~ Third_pillar_size + (Trust_ivs) + GDP_Most_Recent,
  data = mediation_zscore
)
save_psych_med(trust_gdp, "trust_gdp")

# -----------------------------
# GINI
# -----------------------------
gini <- psych::mediate(
  Poli_polarization ~ Third_pillar_size + (Gini_index),
  data = mediation_zscore
)
save_psych_med(gini, "gini")

gini_gdp <- psych::mediate(
  Poli_polarization ~ Third_pillar_size + (Gini_index) + GDP_Most_Recent,
  data = mediation_zscore
)
save_psych_med(gini_gdp, "gini_gdp")

# -----------------------------
# FACEBOOK
# -----------------------------
facebookusers <- psych::mediate(
  Poli_polarization ~ Third_pillar_size + (Facebook_users),
  data = mediation_zscore
)
save_psych_med(facebookusers, "facebookusers")

facebookusers_gdp <- psych::mediate(
  Poli_polarization ~ Third_pillar_size + (Facebook_users) + GDP_Most_Recent,
  data = mediation_zscore
)
save_psych_med(facebookusers_gdp, "facebookusers_gdp")

# -----------------------------
# LONELINESS
# -----------------------------
loneliness <- psych::mediate(
  Poli_polarization ~ Third_pillar_size + (Loneliness),
  data = mediation_zscore
)
save_psych_med(loneliness, "loneliness")

lonelieness_gdp <- psych::mediate(
  Poli_polarization ~ Third_pillar_size + (Loneliness) + GDP_Most_Recent,
  data = mediation_zscore
)
save_psych_med(lonelieness_gdp, "lonelieness_gdp")

# -----------------------------
# ALL TOGETHER
# In psych::mediate you can list multiple mediators by repeating (M)
# and add covariates (GDP) outside parentheses.
# -----------------------------
all_together <- psych::mediate(
  Poli_polarization ~ Third_pillar_size +
    (Trust_ivs) + (Gini_index) + (Facebook_users) + (Loneliness) +
    GDP_Most_Recent,
  data = mediation_zscore
)
save_psych_med(all_together, "all_together")

message("\npsych::mediate runs complete. See outputs/mediation_psych/.\n")