# 03_import_data.R

# Load helper scripts
source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R"))

# -----------------------------
# IMPORT RAW DATASETS
# -----------------------------

# 1. Third sector size
third_raw <- read.xlsx(here::here("data_raw", "All variables_thirdpillar.xlsx"))
# 2. Democracy Index
dem_index_raw <- read.xlsx(here::here("data_raw", "Democracy Index", "Democracy Index 2023.xlsx"))
# 3. Election turnout (.dta)
elect_trnt_raw <- read_dta(here::here("data_raw", "Election Turnout", "PUBLIC_GD-Turnout.dta"))
# 4. V-Dem
vdem_raw <- read.csv(here::here("data_raw", "V-Dem-CY-FullOthers_csv_v13", "V-Dem-CY-Full+Others-v13.csv"))
# 5. OECD trust
trust_raw <- read.xlsx(here::here("data_raw", "Trust in Government", "OECD Trust in Government.xlsx"), colNames = FALSE)
# 6. Political participation
particip_raw <- read.csv(here::here("data_raw", "Political Participation", "political-participation-index-eiu.csv"))
# 7. Polarization folder (many datasets)
govexp_raw <- read.csv(here::here("data_raw", "Political Polarization", "API_GC.XPN.TOTL.GD.ZS_DS2_en_csv_v2_42639.csv"), skip = 4, header = TRUE)
gdp_raw <- read.csv(here::here("data_raw", "Political Polarization", "API_NY.GDP.MKTP.KD_DS2_en_csv_v2_279473.csv"), skip = 4, header = TRUE)
gini_raw <- read.xlsx(here::here("data_raw", "Political Polarization", "API_SI.POV.GINI_DS2_en_excel_v2_39.xlsx"), sheet = "Data", startRow = 4)
pop_raw <- read.csv(here::here("data_raw", "Political Polarization", "API_SP.POP.TOTL_DS2_en_csv_v2_277159.csv"), skip = 4, header = TRUE)
owid_raw <- read.csv(here::here("data_raw", "Political Polarization", "burden-disease-from-each-mental-illness.csv"), header = TRUE) #mental illness
ess_raw <- read.csv(here::here("data_raw", "Political Polarization", "ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-subset.csv"), header = TRUE)
facebook_raw <- read.xlsx(here::here("data_raw", "Political Polarization", "Facebook Users.xlsx"), sheet = 1)
inclusive_raw <- read.xlsx(here::here("data_raw", "Political Polarization", "global_data_for_website_2020.xlsx"), sheet = "Inclusiveness")
owid2_raw <- read.csv(here::here("data_raw", "Political Polarization", "happiness-cantril-ladder.csv"), header = TRUE) #happiness, scale from 0 to 10
owid3_raw <- read.csv(here::here("data_raw", "Political Polarization", "number-of-internet-users.csv"), header = TRUE) #number of internet users
owid4_raw <- read.csv(here::here("data_raw", "Political Polarization", "one-person-households.csv"), header = TRUE) #loneliness, percentage
owid5_raw <- read.csv(here::here("data_raw", "Political Polarization", "human-development-index.csv"), header = TRUE) 
pew_raw <- read_excel(here::here("data_raw", "Political Polarization", "PEW Data Social Media Use.xlsx"), sheet = "PEW", skip = 2)
ivs_raw <- read.csv(here::here("data_raw", "Political Polarization", "self-reported-trust-attitudes.csv"), header = TRUE)
load(here::here("data_raw", "Political Polarization", "WVS_Cross-National_Wave_7_rData_v5_0.RData"))
wvs_raw <- `WVS_Cross-National_Wave_7_v5_0`



# Save raw list ready for cleaning
saveRDS(
  list(
    third_raw = third_raw,
    dem_index_raw = dem_index_raw,
    elect_trnt_raw = elect_trnt_raw,
    vdem_raw = vdem_raw,
    trust_raw = trust_raw,
    particip_raw = particip_raw,
    govexp_raw = govexp_raw,
    gdp_raw = gdp_raw,
    gini_raw = gini_raw,
    pop_raw = pop_raw,
    owid_raw = owid_raw,
    ess_raw = ess_raw,
    facebook_raw = facebook_raw,
    inclusive_raw = inclusive_raw,
    owid2_raw = owid2_raw,
    owid3_raw = owid3_raw,
    owid4_raw = owid4_raw,
    owid5_raw = owid5_raw,
    pew_raw = pew_raw,
    ivs_raw = ivs_raw,
    wvs_raw = wvs_raw
    
  ),
  here::here("data_clean", "raw_data.rds")
)
