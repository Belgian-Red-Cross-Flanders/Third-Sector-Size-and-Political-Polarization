# 03_import_data.R

# Load helper scripts
source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R"))

# -----------------------------
# IMPORT RAW DATASETS
# -----------------------------

# 1. Third sector size
third_raw <- read.xlsx(here::here("data_raw", "All variables_thirdpillar.xlsx"))
# 4. V-Dem
vdem_raw <- read.csv(here::here("data_raw", "V-Dem-CY-FullOthers_csv_v13", "V-Dem-CY-Full+Others-v13.csv"))
# 5. OECD trust
trust_raw <- read.xlsx(here::here("data_raw", "Trust in Government", "OECD Trust in Government.xlsx"), colNames = FALSE)
# 7. Polarization folder (many datasets)
gdp_raw <- read.csv(here::here("data_raw", "Political Polarization", "API_NY.GDP.MKTP.KD_DS2_en_csv_v2_279473.csv"), skip = 4, header = TRUE)
gini_raw <- read.xlsx(here::here("data_raw", "Political Polarization", "API_SI.POV.GINI_DS2_en_excel_v2_39.xlsx"), sheet = "Data", startRow = 4)
pop_raw <- read.csv(here::here("data_raw", "Political Polarization", "API_SP.POP.TOTL_DS2_en_csv_v2_277159.csv"), skip = 4, header = TRUE)
facebook_raw <- read.csv(here::here("data_raw", "Political Polarization", "facebook-users-by-country-2026.csv"), header = TRUE)
owid4_raw <- read.csv(here::here("data_raw", "Political Polarization", "one-person-households.csv"), header = TRUE) #loneliness, percentage
ivs_raw <- read.csv(here::here("data_raw", "Political Polarization", "self-reported-trust-attitudes.csv"), header = TRUE)
load(here::here("data_raw", "Political Polarization", "WVS_Cross-National_Wave_7_rData_v5_0.RData"))
wvs_raw <- `WVS_Cross-National_Wave_7_v5_0`



# Save raw list ready for cleaning
saveRDS(
  list(
    third_raw = third_raw,
    vdem_raw = vdem_raw,
    trust_raw = trust_raw,
    gdp_raw = gdp_raw,
    gini_raw = gini_raw,
    pop_raw = pop_raw,
    facebook_raw = facebook_raw,
    owid4_raw = owid4_raw,
    ivs_raw = ivs_raw,
    wvs_raw = wvs_raw
    
  ),
  here::here("data_clean", "raw_data.rds")
)
