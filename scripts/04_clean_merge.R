# 04_clean_merge.R
# -----------------------------------------------------------
# Clean and merge all raw datasets into one master dataset
# -----------------------------------------------------------

source(here::here("scripts", "01_load_packages.R"))
source(here::here("scripts", "02_functions.R"))

# Load corrections CSV
corrections <- load_country_corrections()

# Load raw datasets
raw <- readRDS(here::here("data_clean", "raw_data.rds"))


# -----------------------
# CLEAN V-DEM 
# -----------------------
vdem <- raw$vdem_raw %>%
  dplyr::mutate(
    Country = standardize_country(country_name, corrections),
    v2cacamps_mean_pct = (v2cacamps_mean / 4) * 100
  ) %>%
  dplyr::select(
    Country,
    year,
    v2xcs_ccsi, #Core civil society index (D) (v2xcs_ccsi)
    v2elsuffrage, #Percentage of population with suffrage (A) (v2elsuffrage) 
    v2eltrnout, #Election turnout (A) (v2eltrnout)
    v2peedueq_mean, #Educational equality (C) (v2peedueq) 
    v2cacamps_mean_pct,
    v2pehealth_mean, #Health equality (C) (v2pehealth)
    v2cacamps_mean, #Political polarization (C) (v2cacamps)
    v2x_civlib, #Civil liberties index (D) (v2x_civlib)
    e_pelifeex #Life expectancy (E) (e_pelifeex)
  ) %>%
  dplyr::filter(year == 2022) %>%
  suffix_vars("vdem")

                

# -----------------------
# CLEAN THIRD PILLAR SIZE
# -----------------------
tpt <- raw$third_raw %>%
  dplyr::select(
    Country,
    Total,
    population,
    participation,
    democ,
    gdpPerCapita,
    World.Bank.Income.Group
  ) %>%
  # standardize country names
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    
    # convert numeric-like columns properly
    Total = as.numeric(as.character(Total)),
    population = as.numeric(as.character(population)),
    participation = as.numeric(as.character(participation)),
    democ = as.numeric(as.character(democ)),
    gdpPerCapita = as.numeric(as.character(gdpPerCapita))
  ) %>%
  # drop rows with missing key values
  na.omit()%>%
  suffix_vars("tpt")



# -----------------------
# CLEAN OECD TRUST
# -----------------------
trust <- raw$trust_raw %>%
  dplyr::rename(
    Country = X1,
    `2015` = X2,
    `2016` = X3,
    `2017` = X4,
    `2018` = X5,
    `2019` = X6,
    `2020` = X7,
    `2021` = X8,
    `2022` = X9
  ) %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    
    # Convert text numbers to numeric
    across(`2015`:`2022`, ~ as.numeric(.x)),
    
    # Compute mean of last 3 years 
    Trust_in_Gov_OECD = rowMeans(dplyr::select(., `2020`:`2022`), na.rm = TRUE)

    # If you prefer 2016–2022 mean (7 years), uncomment and use this instead:
    # Trust_OECD_2016_2022_mean = rowMeans(dplyr::select(., `2016`:`2022`), na.rm = TRUE)
    
  ) %>%
  dplyr::select(Country, Trust_in_Gov_OECD)%>%
  suffix_vars("oecd_trust")



# -----------------------
# CLEAN DEMOCRACY INDEX 
# -----------------------
democ <- raw$dem_index_raw %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections)
  ) %>%
  dplyr::select(
    Country,
    Democracy.Index,
    Rank,
    Change.in.Rank,
    Electoral.process.and.pluralism,
    Functioning.of.government,
    Political.participation,
    Political.culture,
    Civil.liberties
  )%>%
  suffix_vars("dem_index")



# -----------------------
# CLEAN POLITICAL PARTICIPATION (EIU index)
# -----------------------
particip <- raw$particip_raw %>%
  dplyr::filter(Year == 2022) %>%
  dplyr::rename(Country = Entity) %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections)
  ) %>%
  dplyr::select(
    Country,
    Year,
    pol_part_eiu
  )%>%
  suffix_vars("particip_eiu")



# -----------------------
# CLEAN GINI INDEX (WB wide → long → latest non-NA) 
# -----------------------
gini <- raw$gini_raw %>%
  # keep country and all 4-digit year columns
  dplyr::select(
    Country.Name,
    tidyselect::matches("^(19|20)\\d{2}$")
  ) %>%
  # wide → long
  tidyr::pivot_longer(
    cols = tidyselect::matches("^(19|20)\\d{2}$"),
    names_to  = "Year",
    values_to = "Value"
  ) %>%
  # types + standardized country
  dplyr::mutate(
    Year    = as.numeric(Year),
    Value   = as.numeric(Value),
    Country = standardize_country(Country.Name, corrections)
  ) %>%
  # pick most recent non-NA per Country
  dplyr::arrange(Country, dplyr::desc(Year)) %>%
  dplyr::group_by(Country) %>%
  dplyr::filter(!is.na(Value)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  # final output (keep both value and its year)
  dplyr::transmute(
    Country,
    Gini_index = Value,
    Gini_year  = Year
  )%>%
  suffix_vars("gini")



# -----------------------
# CLEAN ELECTION TURNOUT (with vector of used years)
# -----------------------
turnout <- raw$elect_trnt_raw %>%
  dplyr::select(country, year, turnoutreg, compulsory) %>%
  dplyr::filter(year %in% 2015:2020, compulsory == 0) %>%
  dplyr::group_by(country) %>%
  dplyr::summarise(
    Election_turnout_avg = mean(turnoutreg, na.rm = TRUE),
    Turnout_years_used = paste(sort(year[!is.na(turnoutreg)]), collapse = ", "),
    .groups = "drop"
  ) %>%
  dplyr::mutate(
    Country = standardize_country(country, corrections)
  ) %>%
  dplyr::select(
    Country,
    Election_turnout_avg,
    Turnout_years_used
  )%>%
  suffix_vars("el_turnout")



# -----------------------
# CLEAN POPULATION (World Bank)
# -----------------------
pop <- raw$pop_raw %>%
  dplyr::rename(
    Country = Country.Name,
    Population_2022 = X2022
  ) %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    Population_2022 = as.numeric(as.character(Population_2022))
  ) %>%
  dplyr::select(Country, Population_2022)%>%
  suffix_vars("pop_wb")



# -----------------------
# CLEAN GDP (World Bank Constant 2015 USD)
# -----------------------
gdp <- raw$gdp_raw %>%
  dplyr::rename(Country = Country.Name) %>%
  
  # wide → long
  tidyr::pivot_longer(
    cols = tidyselect::starts_with("X"),
    names_to   = "Year",
    names_prefix = "X",
    values_to  = "GDP"
  ) %>%
  
  dplyr::mutate(
    Year = as.numeric(Year),
    GDP  = as.numeric(GDP)
  ) %>%
  
  # get the *most recent* non-NA GDP value per country
  dplyr::arrange(Country, dplyr::desc(Year)) %>%
  dplyr::group_by(Country) %>%
  dplyr::filter(!is.na(GDP)) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  
  # standardize after grouping (best practice)
  dplyr::mutate(
    Country = standardize_country(Country, corrections),
    GDP_Most_Recent = GDP
  ) %>%
  
  # keep both GDP value and the corresponding year
  dplyr::select(
    Country,
    GDP_year = Year,
    GDP_Most_Recent
  )%>%
  suffix_vars("gdp_wb")




# -----------------------
# CLEAN FACEBOOK USERS
# -----------------------
facebook <- raw$facebook_raw %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections)
  ) %>%
  dplyr::select(
    Country,
    Number_of_users
  )%>%
  suffix_vars("fb")



# -----------------------
# CLEAN INCLUSIVENESS INDEX (2020)
# https://belonging.berkeley.edu/inclusiveness-index
# -----------------------
inclusive <- raw$inclusive_raw %>%
  # replace 9999 with NA across all columns
  dplyr::mutate(dplyr::across(dplyr::everything(), ~ ifelse(. == 9999, NA, .))) %>%
  
  # standardize Country names
  dplyr::mutate(
    Country = standardize_country(Country, corrections)
  ) %>%
  
  # keep the two relevant columns, preserving original names
  dplyr::select(
    Country,
    Inclusiveness.index.2020
  )%>%
  suffix_vars("inc_berk")


# Our World in Data 
# -----------------------
# CLEAN OWID MENTAL ILLNESS (latest year)
# -----------------------
owid_mental <- raw$owid_raw %>%
  dplyr::group_by(Entity, Code) %>%
  dplyr::filter(Year == max(Year)) %>%
  dplyr::slice_max(Year, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::rename(
    Country = Entity,
    Depressive_disorders = DALYs.from.depressive.disorders.per.100.000.people.in..both.sexes.aged.age.standardized,
    Schizophrenia = DALYs.from.schizophrenia.per.100.000.people.in..both.sexes.aged.age.standardized,
    Bipolar_disorders = DALYs.from.bipolar.disorder.per.100.000.people.in..both.sexes.aged.age.standardized,
    Eating_disorders = DALYs.from.eating.disorders.per.100.000.people.in..both.sexes.aged.age.standardized,
    Anxiety_disorders = DALYs.from.anxiety.disorders.per.100.000.people.in..both.sexes.aged.age.standardized,
    Mental_illness_year = Year
  ) %>%
  dplyr::select(-Code) %>%
  dplyr::mutate(Country = standardize_country(Country, corrections)) %>%
  drop_owid_regions() %>%
  suffix_vars("mental_owid")
                
# -----------------------
# CLEAN OWID HAPPINESS (latest year)
# -----------------------
owid_happiness <- raw$owid2_raw %>%
  dplyr::group_by(Entity, Code) %>%
  dplyr::filter(Year == max(Year)) %>%
  dplyr::slice_max(Year, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::rename(
    Country = Entity,
    Happiness = Cantril.ladder.score,
    Happiness_year = Year
  ) %>%
  dplyr::select(-Code) %>%
  dplyr::mutate(Country = standardize_country(Country, corrections)) %>%
  drop_owid_regions() %>%
  suffix_vars("happiness_owid")

# -----------------------
# CLEAN OWID INTERNET USERS (latest year)
# -----------------------
owid_internet <- raw$owid3_raw %>%
  dplyr::group_by(Entity, Code) %>%
  dplyr::filter(Year == max(Year)) %>%
  dplyr::slice_max(Year, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::rename(
    Country = Entity,
    Internet_users = Number.of.Internet.users,
    Internet_users_year = Year
  ) %>%
  dplyr::select(-Code) %>%
  dplyr::mutate(Country = standardize_country(Country, corrections)) %>%
  drop_owid_regions() %>%
  suffix_vars("internet_owid")

# -----------------------
# CLEAN OWID ONE-PERSON HOUSEHOLDS (latest year)
# -----------------------
owid_loneliness <- raw$owid4_raw %>%
  dplyr::group_by(Entity, Code) %>%
  dplyr::filter(Year == max(Year)) %>%
  dplyr::slice_max(Year, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::rename(
    Country = Entity,
    Loneliness = Share.of.one.person.households,
    Loneliness_year = Year
  ) %>%
  dplyr::select(-Code) %>%
  dplyr::mutate(Country = standardize_country(Country, corrections)) %>%
  drop_owid_regions() %>%
  suffix_vars("loneliness_owid")

# -----------------------
# CLEAN OWID HDI (latest year)
# -----------------------
owid_hdi <- raw$owid5_raw %>%
  dplyr::group_by(Entity, Code) %>%
  dplyr::filter(Year == max(Year)) %>%
  dplyr::slice_max(Year, with_ties = FALSE) %>%
  dplyr::ungroup() %>%
  dplyr::rename(
    Country = Entity,
    Human_Development_Index = Human.Development.Index,
    HDI_year = Year
  ) %>%
  dplyr::select(-Code) %>%
  dplyr::mutate(Country = standardize_country(Country, corrections)) %>%
  drop_owid_regions() %>%
  suffix_vars("hdi_owid")

# -----------------------
# MERGE ALL CLEAN OWID DATASETS
# -----------------------
owid_list <- list(
  owid_mental,
  owid_happiness,
  owid_internet,
  owid_loneliness,
  owid_hdi
)

owid <- reduce(owid_list, ~ dplyr::full_join(.x, .y, by = "Country"))


# -----------------------
# CLEAN PEW SOCIAL MEDIA USE 
# (Do you ever use online social media sites?)
# -----------------------
pew <- raw$pew_raw %>%
  dplyr::mutate(
    Country = standardize_country(Country, corrections)
  )%>%
  suffix_vars("socialm_pew")



# -----------------------
# CLEAN IVS TRUST 
# -----------------------
ivs <- raw$ivs_raw %>%
  dplyr::rename(
    Country = Entity
  ) %>%
  
  # keep latest year for each country
  dplyr::arrange(Country, dplyr::desc(Year)) %>%
  dplyr::group_by(Country) %>%
  dplyr::slice(1) %>%
  dplyr::ungroup() %>%
  
  # keep only relevant trust variable
  dplyr::select(
    Country,
    Agree.Most.people.can.be.trusted,
    IVS_year = Year
  ) %>%
  
  # standardize country names
  dplyr::mutate(
    Country = standardize_country(Country, corrections)
  ) %>%
  
  # drop rows with missing trust values (original: na.omit)
  tidyr::drop_na(Agree.Most.people.can.be.trusted)%>%
  suffix_vars("trust_ivs")



# -----------------------
# CLEAN GOVERNMENT EXPENDITURE (% of GDP) — multi‑year average 2012–2022
# -----------------------
govexp <- raw$govexp_raw %>%
  dplyr::rename(Country = Country.Name) %>%
  
  # keep only the selected years 
  dplyr::select(
    Country,
    X2012, X2013, X2014, X2015, X2016, X2017,
    X2018, X2019, X2020, X2021, X2022
  ) %>%
  
  # compute multi-year average
  dplyr::mutate(
    Govt_expenditure = rowMeans(dplyr::across(starts_with("X")), na.rm = TRUE),
    Country = standardize_country(Country, corrections)
  ) %>%
  
  # ensure NaN becomes NA
  dplyr::mutate(
    Govt_expenditure = dplyr::na_if(Govt_expenditure, NaN)
  ) %>%
  
  # final shape
  dplyr::select(Country, Govt_expenditure)%>%
  suffix_vars("govexp")



# -----------------------
# CLEAN WVS (World Values Survey)
# This section uses helper functions from 02_functions.R
# -----------------------

# -----------------------
# 1. SELECT VARIABLES FROM RAW WVS
# -----------------------
wvs <- wvs_select_vars(raw$wvs_raw)
# (selects Wave 7 and all Q‑variables we need)

# -----------------------
# 2. MAP ISO‑3 COUNTRY CODES → COUNTRY NAMES
# -----------------------
# via map_wvs_country() from 02_functions.R
wvs <- wvs %>%
  dplyr::mutate(Country = map_wvs_country(Country))

# -----------------------
# 3. STANDARDIZE COUNTRY NAMES
# -----------------------
wvs <- wvs %>%
  dplyr::mutate(Country = standardize_country(Country, corrections))

# -----------------------
# 4. RENAME VARIABLES TO DESCRIPTIVE NAMES
# -----------------------
wvs <- wvs_recode_values(wvs)
# (this also keeps recoding of invalid categories)

# -----------------------
# 5. MEAN‑LEVEL AGGREGATION FOR ALL CONTINUOUS VARIABLES
# -----------------------
wvs_mean <- wvs_aggregate_means(wvs)

# -----------------------
# 6. DERIVED PERCENTAGES (TOLERANCE, TRUST, MEMBERSHIP, ETC.)
# -----------------------
wvs_tol    <- wvs_tolerance(wvs)
wvs_trust  <- wvs_trust(wvs)
wvs_active <- wvs_active_member(wvs)
wvs_party  <- wvs_member_political_party(wvs)

# -----------------------
# 7. COMBINE ALL AGGREGATES
# -----------------------
wvs_data <- wvs_combine_aggregates(
  wvs_mean,
  wvs_tol,
  wvs_trust,
  wvs_active,
  wvs_party
)

# -----------------------
# 8. KEEP FINAL SET OF VARIABLES 
# -----------------------
wvs_data <- wvs_data %>%
  dplyr::select(
    Country,
    Feeling_happiness, Satisfaction_life, Confidence_government, Income_equality,
    Percentage_tolerance, Percentage_trust,
    Family_importance, Friends_importance, Politics_importance, Religion_importance,
    Confidence_political_parties, Confidence_parliament, Confidence_elections,
    Info_social_media,
    Percentage_active_members, Percentage_members, Percentage_poli_members
  )

# -----------------------
# 9. MERGE GREAT BRITAIN + NORTHERN IRELAND INTO “UNITED KINGDOM …”
# -----------------------
wvs_data <- wvs_combine_uk(wvs_data)

# -----------------------
# 10. FINAL STANDARDIZATION (SAFEGUARD)
# -----------------------
wvs_data <- wvs_data %>%
  dplyr::mutate(Country = standardize_country(Country, corrections))%>%
  suffix_vars("wvs")



# -----------------------
# CLEAN ESS LIFE SATISFACTION
# -----------------------

ess <- raw$ess_raw %>%
  # keep two columns
  dplyr::select(cntry, stflife) %>%
  
  # map country codes → names
  dplyr::mutate(
    cntry = unname(map_ess_country(cntry)),
    stflife = as.numeric(stflife)
  ) %>%
  
  # keep only valid values
  dplyr::mutate(
    stflife = dplyr::case_when(
      stflife %in% 0:9 ~ stflife,
      TRUE ~ NA_real_
    )
  ) %>%
  
  # compute mean per country
  dplyr::group_by(cntry) %>%
  dplyr::summarise(mean_stflife = mean(stflife, na.rm = TRUE), .groups = "drop") %>%
  
  # rename & standardize
  dplyr::rename(Country = cntry) %>%
  dplyr::mutate(Country = standardize_country(Country, corrections))%>%
  suffix_vars("ess")




# -----------------------
# MASTER MERGE — THIRD PILLAR AS ANCHOR
# -----------------------
master <- tpt %>%                     # <— anchor dataset 
  dplyr::left_join(vdem,       by = "Country") %>%
  dplyr::left_join(trust,      by = "Country") %>%
  dplyr::left_join(democ,      by = "Country") %>%
  dplyr::left_join(particip,   by = "Country") %>%
  dplyr::left_join(turnout,    by = "Country") %>%
  dplyr::left_join(gini,       by = "Country") %>%
  dplyr::left_join(gdp,        by = "Country") %>%
  dplyr::left_join(pop,        by = "Country") %>%
  dplyr::left_join(facebook,   by = "Country") %>%
  dplyr::left_join(inclusive,  by = "Country") %>%
  dplyr::left_join(owid,       by = "Country") %>%  # merged OWID block
  dplyr::left_join(pew,        by = "Country") %>%
  dplyr::left_join(wvs_data,   by = "Country") %>%
  
  # ESS + IVS are sparse → full_join is correct
  dplyr::left_join(ess,        by = "Country") %>%
  dplyr::left_join(ivs,        by = "Country") %>%
  
  # Clean ordering
  dplyr::arrange(Country)


# -----------------------
# DERIVED INDICATORS (computed once and stored)
# -----------------------
master <- master %>%
  # 1) Polarization percent (already in V-Dem as v2cacamps_mean_pct_vdem) → rename for better understanding
  dplyr::mutate(
    Poli_polarization_pct = v2cacamps_mean_pct_vdem
  ) %>%
  
  # 2) Facebook users per 100k (needs Population_2022)
  dplyr::mutate(
    Facebook_users_per100k_fb = dplyr::if_else(
      !is.na(Number_of_users_fb) & !is.na(Population_2022_pop_wb) & Population_2022_pop_wb > 0,
      (Number_of_users_fb / Population_2022_pop_wb) * 1e5,
      NA_real_
    )
  ) %>%
  
  # 3) Internet users per 100k (OWID total internet users; same denominator)
  dplyr::mutate(
    Internet_users_per100k_internet_owid = dplyr::if_else(
      !is.na(Internet_users_internet_owid) & !is.na(Population_2022_pop_wb) & Population_2022_pop_wb > 0,
      (Internet_users_internet_owid / Population_2022_pop_wb) * 1e5,
      NA_real_
    )
  ) %>%
  
  # 4) PEW: share answering "Yes" (ensure same survey base)
  dplyr::mutate(
    Pew_share_social_media_yes_socialm_pew = dplyr::if_else(
      !is.na(Yes_socialm_pew) & !is.na(Total_socialm_pew) & Total_socialm_pew > 0,
      (Yes_socialm_pew / Total_socialm_pew) * 100,
      NA_real_
    )
  )

# -----------------------
# DIAGNOSTICS
# -----------------------

diag_list <- list(
  vdem      = vdem$Country,
  trust     = trust$Country,
  democ     = democ$Country,
  particip  = particip$Country,
  turnout   = turnout$Country,
  gini      = gini$Country,
  gdp       = gdp$Country,
  pop       = pop$Country,
  facebook  = facebook$Country,
  inclusive = inclusive$Country,
  owid      = owid$Country,
  pew       = pew$Country,
  wvs       = wvs_data$Country,
  ess       = ess$Country,
  ivs       = ivs$Country
)

# 1. For each dataset: which TPT countries are missing?
cat("\n=== MISSING IN DATASETS (relative to Third Pillar) ===\n")
for (nm in names(diag_list)) {
  missing <- setdiff(tpt$Country, diag_list[[nm]])
  if (length(missing) > 0) {
    cat("\n- Missing in", nm, ": ", paste(missing, collapse = ", "), "\n")
  }
}

# 2. Any duplicated country names in master?
dup_countries <- master$Country[duplicated(master$Country)]
if (length(dup_countries) > 0) {
  cat("\n=== DUPLICATED COUNTRIES IN MASTER ===\n")
  print(unique(dup_countries))
} else {
  cat("\nNo duplicated countries in master.\n")
}

# 3. Summary of country counts
cat("\n=== COUNTRY COUNTS ===\n")
cat("TPT anchor countries: ", length(unique(tpt$Country)), "\n")
cat("Master final rows:    ", nrow(master), "\n")

# 4. Countries in master but NOT recognized in tpt (should be 0)
extra <- setdiff(master$Country, tpt$Country)
if (length(extra) > 0) {
  cat("\n=== UNEXPECTED COUNTRIES IN MASTER (not in TPT) ===\n")
  print(extra)
}




# -----------------------
# SAVE OUTPUT
# -----------------------

saveRDS(master, here::here("data_clean", "master_dataset.rds"))
writexl::write_xlsx(master, here::here("data_clean", "master_dataset.xlsx"))

cat("\nMaster dataset created successfully.")
