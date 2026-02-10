rm(list = ls())

# Install Packages
#######################################################################################################
# List of packages to install if not already installed
packages <- c("dplyr", "xlsx", "ggplot2", "ggrepel", "haven", 
              "tidyr", "dbplyr", "dtplyr", "readxl", "psych",
              "writexl", "lavaan", "semPlot", "mediation",
              "gridExtra", "car", "lmtest")

# Check if each package is installed; if not, install it
for (pkg in packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
}

# Standardize Country Names 
#######################################################################################################


# Function to standardize country names 
standardize_country_name <- function(country_name) {
  country_name <- ifelse(country_name == "Antigua and Barb.", "Antigua and Barbuda", country_name)
  country_name <- ifelse(country_name == "Bahamas, The", "Bahamas", country_name)
  country_name <- ifelse(country_name == "The Bahamas", "Bahamas", country_name)
  country_name <- ifelse(country_name == "Bolivia", "Bolivia (Plurinational State of)", country_name)
  country_name <- ifelse(country_name == "Bosnia", "Bosnia and Herzegovina", country_name)
  country_name <- ifelse(country_name == "British US Virgin Islands", "British Virgin Islands", country_name)
  country_name <- ifelse(country_name == "Brunei Darussalam", "Brunei", country_name)
  country_name <- ifelse(country_name == "Cape Verde", "Cabo Verde", country_name)
  country_name <- ifelse(country_name == "People's Republic of China", "China", country_name)
  country_name <- ifelse(country_name == "Congo (Kinshasa)", "Democratic Republic of the Congo", country_name)
  country_name <- ifelse(country_name == "Congo (Kinchasa)", "Democratic Republic of the Congo", country_name)
  country_name <- ifelse(country_name == "Congo, Dem. Rep.", "Democratic Republic of the Congo", country_name)
  country_name <- ifelse(country_name == "Democratic Republic of Congo", "Democratic Republic of the Congo", country_name)
  country_name <- ifelse(country_name == "Congo, Rep.", "Congo", country_name)
  country_name <- ifelse(country_name == "Congo (Brazzaville)", "Congo", country_name)
  country_name <- ifelse(country_name == "Republic of The Congo (Brazzaville)", "Congo", country_name)
  country_name <- ifelse(country_name == "Republic of the Congo", "Congo", country_name)
  country_name <- ifelse(country_name == "Republic of Congo", "Congo", country_name)
  country_name <- ifelse(country_name == "Côte d'Ivoire", "Côte D'Ivoire", country_name)
  country_name <- ifelse(country_name == "Cote d'Ivoire", "Côte D'Ivoire", country_name)
  country_name <- ifelse(country_name == "Cote D'Ivoire", "Côte D'Ivoire", country_name)
  country_name <- ifelse(country_name == "Ivory Coast", "Côte D'Ivoire", country_name)
  country_name <- ifelse(country_name == "Curacao", "Curaçao", country_name)
  country_name <- ifelse(country_name == "Czech Republic", "Czechia", country_name)
  country_name <- ifelse(country_name == "Democratic People's Republic of Korea", "North Korea", country_name)
  country_name <- ifelse(country_name == "Egypt, Arab Rep.", "Egypt", country_name)
  country_name <- ifelse(country_name == "Eswatini", "Swaziland", country_name)
  country_name <- ifelse(country_name == "Federated States of Micronesia", "Micronesia (Federated States of)", country_name)
  country_name <- ifelse(country_name == "Micronesia, Fed. Sts.", "Micronesia (Federated States of)", country_name)
  country_name <- ifelse(country_name == "Micronesia Federated States of", "Micronesia (Federated States of)", country_name)
  country_name <- ifelse(country_name == "Gambia", "Gambia (Republic of The)", country_name)
  country_name <- ifelse(country_name == "Gambia, The", "Gambia (Republic of The)", country_name)
  country_name <- ifelse(country_name == "The Gambia", "Gambia (Republic of The)", country_name)
  country_name <- ifelse(country_name == "Hong Kong SAR, China", "Hong Kong", country_name)
  country_name <- ifelse(country_name == "Hong Kong SAR PRC", "Hong Kong", country_name)
  country_name <- ifelse(country_name == "Iran", "Iran (Islamic Republic of)", country_name)
  country_name <- ifelse(country_name == "Iran Islamic Republic of", "Iran (Islamic Republic of)", country_name)
  country_name <- ifelse(country_name == "Iran, Islamic Rep.", "Iran (Islamic Republic of)", country_name)
  country_name <- ifelse(country_name == "Korea, Dem. People's Rep.", "Democratic People's Republic of Korea", country_name)
  country_name <- ifelse(country_name == "Korea, Rep.", "Republic of Korea", country_name)
  country_name <- ifelse(country_name == "Korea Republic of", "Republic of Korea", country_name)
  country_name <- ifelse(country_name == "Korea", "Republic of Korea", country_name)
  country_name <- ifelse(country_name == "South Korea", "Republic of Korea", country_name)
  country_name <- ifelse(country_name == "Kyrgyz Republic", "Kyrgyzstan", country_name)
  country_name <- ifelse(country_name == "Lao People's Democratic Republic", "Laos", country_name)
  country_name <- ifelse(country_name == "Lao PDR", "Laos", country_name)
  country_name <- ifelse(country_name == "Lao Peoples Dem. Republic", "Laos", country_name)
  country_name <- ifelse(country_name == "Macedonia, North", "North Macedonia", country_name)
  country_name <- ifelse(country_name == "Macedonia", "North Macedonia", country_name)
  country_name <- ifelse(country_name == "Macedonia former Yugoslav Republic (1993-)", "North Macedonia", country_name)
  country_name <- ifelse(country_name == "Republic of Macedonia", "North Macedonia", country_name)
  country_name <- ifelse(country_name == "Moldova", "Republic of Moldova", country_name)
  country_name <- ifelse(country_name == "Moldova Republic of", "Republic of Moldova", country_name)
  country_name <- ifelse(country_name == "Myanmar (Burma)", "Myanmar", country_name)
  country_name <- ifelse(country_name == "Burma/Myanmar", "Myanmar", country_name)
  country_name <- ifelse(country_name == "Netherlands", "Netherlands", country_name)
  country_name <- ifelse(country_name == "Pakistan", "Pakistan", country_name)
  country_name <- ifelse(country_name == "Palestine", "State of Palestine", country_name)
  country_name <- ifelse(country_name == "West Bank and Gaza", "State of Palestine", country_name)
  country_name <- ifelse(country_name == "Republic of Moldova", "Moldova", country_name)
  country_name <- ifelse(country_name == "Romania", "Romania", country_name)
  country_name <- ifelse(country_name == "Russia", "Russian Federation", country_name)
  country_name <- ifelse(country_name == "S. Sudan", "South Sudan", country_name)
  country_name <- ifelse(country_name == "Saint Kitts and Nevis", "St. Kitts and Nevis", country_name)
  country_name <- ifelse(country_name == "Saint Lucia", "St. Lucia", country_name)
  country_name <- ifelse(country_name == "Saint Martin (Dutch)", "Sint Maarten", country_name)
  country_name <- ifelse(country_name == "Sint Maarten (Dutch part)", "Sint Maarten", country_name)
  country_name <- ifelse(country_name == "Saint Vincent and the Grenadines", "St. Vincent and the Grenadines", country_name)
  country_name <- ifelse(country_name == "Saint Vincent and The Grenadines", "St. Vincent and the Grenadines", country_name)
  country_name <- ifelse(country_name == "Slovak Republic", "Slovakia", country_name)
  country_name <- ifelse(country_name == "São Tomé and Príncipe", "Sao Tome and Principe", country_name)
  country_name <- ifelse(country_name == "Syria", "Syrian Arab Republic", country_name)
  country_name <- ifelse(country_name == "Taiwan", "Taiwan (Province of China)", country_name)
  country_name <- ifelse(country_name == "Taiwan ROC", "Taiwan (Province of China)", country_name)
  country_name <- ifelse(country_name == "Tanzania", "United Republic of Tanzania", country_name)
  country_name <- ifelse(country_name == "Tanzania United Republic of", "United Republic of Tanzania", country_name)
  country_name <- ifelse(country_name == "Türkiye", "Turkey", country_name)
  country_name <- ifelse(country_name == "Turkiye", "Turkey", country_name)
  country_name <- ifelse(country_name == "Turkey", "Türkiye", country_name)
  country_name <- ifelse(country_name == "United Kingdom", "United Kingdom of Great Britain and Northern Ireland", country_name)
  country_name <- ifelse(country_name == "UK", "United Kingdom of Great Britain and Northern Ireland", country_name)
  country_name <- ifelse(country_name == "United Republic of Tanzania", "Tanzania", country_name)
  country_name <- ifelse(country_name == "United States", "United States of America", country_name)
  country_name <- ifelse(country_name == "United States Virgin Islands", "Virgin Islands, U.S.", country_name)
  country_name <- ifelse(country_name == "US Virgin Islands", "Virgin Islands, U.S.", country_name)
  country_name <- ifelse(country_name == "UAE", "United Arab Emirates", country_name)
  country_name <- ifelse(country_name == "Venezuela", "Venezuela, Bolivarian Republic of", country_name)
  country_name <- ifelse(country_name == "Venezuela, RB", "Venezuela, Bolivarian Republic of", country_name)
  country_name <- ifelse(country_name == "Virgin Islands (U.S.)", "Virgin Islands, U.S.", country_name)
  country_name <- ifelse(country_name == "Vietnam", "Viet Nam", country_name)
  country_name <- ifelse(country_name == "Yemen, Rep.", "Yemen", country_name)
  country_name <- ifelse(country_name == "Republic of Yemen", "Yemen", country_name)
  country_name <- ifelse(country_name == "U.S.A", "United States of America", country_name)
  country_name <- ifelse(country_name == "USA", "United States of America", country_name)
  
  # Remove any text in parentheses (with surrounding whitespace)
  country_name <- gsub("\\s*\\(.*?\\)", "", country_name, fixed = TRUE)
  
  # Change "&" to "and"
  country_name <- gsub("&", "and", country_name, fixed = TRUE)
  
  # Trim whitespace
  trimws(country_name)
}


# Import V-Dem
#######################################################################################################
# Set working directory
setwd("C:\\Users\\KKAY\\OneDrive - Rode Kruis-Vlaanderen\\General\\Paper 3 - Other outcomes\\V-Dem")
vdem <- read.csv("V-Dem-CY-FullOthers_csv_v13\\V-Dem-CY-Full+Others-v13.csv")

#Core civil society index (D) (v2xcs_ccsi)
#Percentage of population with suffrage (A) (v2elsuffrage) 
#Election turnout (A) (v2eltrnout)
#Educational equality (C) (v2peedueq) 
#Health equality (C) (v2pehealth)
#Political polarization (C) (v2cacamps)
#Civil liberties index (D) (v2x_civlib)
#Life expectancy (E) (e_pelifeex)

#Type A: Variables coded by Project Managers and Research Assistants
#Type B: Variables coded by Country Coordinators or Research Assistants
#Type C: Variables coded by Country Experts
#Type A,C: Variables coded by Country Experts and crosschecked by Research
#Type D: Indices
#Type E: Non-V-Dem variables
library(dplyr)
vdem <- vdem %>%
  select(country_name, country_text_id, year, v2xcs_ccsi, v2elsuffrage, v2eltrnout,
         v2peedueq_mean, v2pehealth_mean, v2cacamps_mean, v2x_civlib, e_pelifeex) %>%
  rename(Country = country_name)

vdem$Country <- as.character(vdem$Country)

vdem$Country <- sapply(vdem$Country, standardize_country_name)

vdem <- vdem[vdem$year == 2022, ]
vdem$v2cacamps_mean <- vdem$v2cacamps_mean/4*100

summary(vdem)
sd(vdem$v2cacamps_mean)
median(vdem$v2cacamps_mean)

# Import third pillar size data 
#######################################################################################################
library(xlsx)
tpt <- read.xlsx("All variables_thirdpillar.xlsx", sheetIndex = 1)

tpt <- tpt %>%
  select(Country, Total, population, participation, democ, gdpPerCapita, World.Bank.Income.Group)

tpt$Country <- sapply(tpt$Country, standardize_country_name)

tpt$Total <- as.character(tpt$Total)
tpt$Total <- as.numeric(tpt$Total)
tpt$population <- as.character(tpt$population)
tpt$population <- as.numeric(tpt$population)
tpt$participation <- as.character(tpt$participation)
tpt$participation <- as.numeric(tpt$participation)
tpt$democ <- as.character(tpt$democ)
tpt$democ <- as.numeric(tpt$democ)
tpt$gdpPerCapita <- as.character(tpt$gdpPerCapita)
tpt$gdpPerCapita <- as.numeric(tpt$gdpPerCapita)

tpt <- na.omit(tpt)
tpt_countries <- as.character(tpt$Country)

summary(tpt)
sd(tpt$Total)
median(tpt$Total)

# Create dataset
data <- full_join(tpt, vdem, by = "Country")
data <- data %>%
  arrange(Country, Total)

data_countries <- as.character(data$Country)
vdem_countries <- as.character(vdem$Country)

# Find countries in data but not in vdem
not_in_vdem <- setdiff(data_countries, vdem_countries)

# Find countries in vdem but not in data
not_in_data <- setdiff(vdem_countries, data_countries)

# data <- data[data$Country %in% tpt_countries, ]

rm(vdem)
rm(tpt)

# Import Trust Data 
#######################################################################################################
# Set working directory
setwd("C:\\Users\\KKAY\\OneDrive - Rode Kruis-Vlaanderen\\General\\Paper 3 - Other outcomes\\Trust in Government")

trust <- read.xlsx("OECD Trust in Government.xlsx", sheetName = "Sheet1", header = FALSE)
trust <- trust %>%
  rename(Country = X1,
         `2015` = X2,
         `2016` = X3,
         `2017` = X4,
         `2018` = X5,
         `2019` = X6,
         `2020` = X7,
         `2021` = X8,
         `2022` = X9)
trust$Mean_3 <- rowMeans(trust[, 3:9], na.rm = TRUE) #Data for most recent year was very incomplete, so took average of last 3 years

trust$Country <- sapply(trust$Country, standardize_country_name)

# Add to dataset
data <- full_join(data, trust, by = "Country")
data <- data %>%
  arrange(Country, Total)

data_countries <- as.character(data$Country)
trust_countries <- as.character(trust$Country)

# Find countries in data but not in vdem
not_in_trust <- setdiff(data_countries, trust_countries)

# Find countries in vdem but not in data
not_in_data <- setdiff(trust_countries, data_countries)

# data <- data[data$Country %in% tpt_countries, ]
rm(trust)

# Import political participation 
#######################################################################################################
# Set working directory
setwd("C:\\Users\\KKAY\\OneDrive - Rode Kruis-Vlaanderen\\General\\Paper 3 - Other outcomes\\Political Participation")

part <- read.csv("political-participation-index-eiu.csv", header = TRUE)

part <- part %>%
  filter(Year == 2022) %>%
  rename(Country = Entity)

part$Country <- sapply(part$Country, standardize_country_name)

# Add to dataset
data <- full_join(data, part, by = "Country")
data <- data %>%
  arrange(Country, Total)

data_countries <- as.character(data$Country)
part_countries <- as.character(part$Country)

# Find countries in data but not in vdem
not_in_part <- setdiff(data_countries, part_countries)

# Find countries in vdem but not in data
not_in_data <- setdiff(part_countries, data_countries)

# data <- data[data$Country %in% tpt_countries, ]
rm(part)

# Import Democracy Index
#######################################################################################################
# Set working directory
setwd("C:\\Users\\KKAY\\OneDrive - Rode Kruis-Vlaanderen\\General\\Paper 3 - Other outcomes\\Democracy Index")

democ <- read.xlsx("Democracy Index 2023.xlsx", sheetName = "Sheet1", header = TRUE)

democ$Country <- sapply(democ$Country, standardize_country_name)

# Add to dataset
data <- full_join(data, democ, by = "Country")
data <- data %>%
  arrange(Country, Total)

data_countries <- as.character(data$Country)
democ_countries <- as.character(democ$Country)

# Find countries in data but not in vdem
not_in_democ <- setdiff(data_countries, democ_countries)

# Find countries in vdem but not in data
not_in_data <- setdiff(democ_countries, data_countries)

# data <- data[data$Country %in% tpt_countries, ]
rm(democ)

# Import election turnout
#######################################################################################################
library(haven)
# Specify the path to your .dta file
file_path <- "C:\\Users\\KKAY\\OneDrive - Rode Kruis-Vlaanderen\\General\\Paper 3 - Other outcomes\\Election Turnout\\PUBLIC_GD-Turnout.dta"

# Read the .dta file into R
turnout <- read_dta(file_path)

turnout <- turnout %>%
  select(country, year, turnoutreg, compulsory) %>%
  rename(Country = country) %>%
  filter(year %in% 2015:2020) %>%
  filter(compulsory %in% 0)

agg_turnout <- turnout %>%
  group_by(Country) %>%
  summarize(avg_turnout = mean(turnoutreg))

agg_turnout$Country <- sapply(agg_turnout$Country, standardize_country_name)

# Add to dataset
data <- full_join(data, agg_turnout, by = "Country")
data <- data %>%
  arrange(Country, Total)

data_countries <- as.character(data$Country)
agg_turnout_countries <- as.character(agg_turnout$Country)

# Find countries in data but not in vdem
not_in_agg_turnout <- setdiff(data_countries, agg_turnout_countries)

# Find countries in vdem but not in data
not_in_data <- setdiff(agg_turnout_countries, data_countries)

# data <- data[data$Country %in% tpt_countries, ]
rm(agg_turnout)
rm(turnout)


# Import World Values Survey
#######################################################################################################
setwd("C:\\Users\\KKAY\\OneDrive - Rode Kruis-Vlaanderen\\General\\Paper 3 - Other outcomes\\Political Polarization")
load("WVS_Cross-National_Wave_7_rData_v5_0.RData")
wvs <- `WVS_Cross-National_Wave_7_v5_0`
rm(`WVS_Cross-National_Wave_7_v5_0`)
wvs$Country <- wvs$B_COUNTRY_ALPHA
wvs <- wvs %>%
  filter(A_WAVE == 7) %>%
  select(A_YEAR, A_WAVE, Country, 
         Q1, #Important in life: family, 1 = very important, 2 = rather important, 3 = not very important, 4 = not at all important
         Q2, #Important in life: friends, 1 = very important, 2 = rather important, 3 =  not very important, 4 = not at all important
         Q4, #Important in life: politics, 1 = very important, 2 = rather important, 3 = not very important, 4 = not at all important 
         Q6, #Important in life: religion, 1 = very important, 2, = rather important, 3 = not very important, 4 = not at all important 
         Q72, #Confidence: the political parties, 1 = a great deal, 2, quite a lot, 3 = not very much, 4 = none at all
         Q73, #Confidence: parliament, 1 = a great deal, 2, quite a lot, 3 = not very much, 4 = none at all
         Q76, #Confidence: election, 1 = a great deal, 2, quite a lot, 3 = not very much, 4 = none at all
         Q98, #Active/inactive membership in political party, 0 = not a member, 1 = inactive member, 2 = active member 
         Q98R, #Membership in political party recoded, 0 = not mentioned, 1 = mentioned 
         Q207, #Information source: Social media, 1 = daily, 2 = weekly, 3 = monthly, 4 = less than monthly, 5 = never
         Q217, #Political actions online: Searching information about politics and political events, 1 = Have done, 2 = Might do, 3 = Would never do 
         Q12, #Important child qualities - tolerance and respect for other people, 1 = important, 2 = not mentioned 
         Q46, #Feeling of happiness, 1 = very happy, 2 = quite happy, 3 = not very happy, 4 = not at all happy
         Q49, #Satisfaction with your life, 1 = completely dissatisfied, 10 = completely satisfied
         Q57, #Most people can be trusted, 1 = most people can be trusted, 2 = need to be very careful 
         Q71, #Confidence in the government, 1 = A great deal, 2 = quite a lot, 3 = not very much, 4 = none at all
         Q106, #Income equality vs larger income differences, 1 = Incomes more equal, 10 = larger income differences 
  ) %>%
  mutate(Country = case_when(
    Country == "ALB" ~ "Albania",
    Country == "LBN" ~ "Lebanon",
    Country == "AND" ~ "Andorra",
    Country == "LBY" ~ "Libya",
    Country == "ARG" ~ "Argentina",
    Country == "LTU" ~ "Lithuania",
    Country == "ARM" ~ "Armenia",
    Country == "MAC" ~ "Macao",
    Country == "AUS" ~ "Australia",
    Country == "MYS" ~ "Malaysia",
    Country == "AUT" ~ "Austria",
    Country == "MDV" ~ "Maldives",
    Country == "AZE" ~ "Azerbaijan",
    Country == "MEX" ~ "Mexico",
    Country == "BGD" ~ "Bangladesh",
    Country == "MAR" ~ "Morocco",
    Country == "BLR" ~ "Belarus",
    Country == "MNG" ~ "Mongolia",
    Country == "BOL" ~ "Bolivia",
    Country == "MNE" ~ "Montenegro",
    Country == "BIH" ~ "Bosnia Herzegovina",
    Country == "MMR" ~ "Myanmar",
    Country == "BRA" ~ "Brazil",
    Country == "NLD" ~ "Netherlands",
    Country == "BGR" ~ "Bulgaria",
    Country == "NZL" ~ "New Zealand",
    Country == "CAN" ~ "Canada",
    Country == "NIC" ~ "Nicaragua",
    Country == "CHL" ~ "Chile",
    Country == "NGA" ~ "Nigeria",
    Country == "CHN" ~ "China",
    Country == "MKD" ~ "North Macedonia",
    Country == "COL" ~ "Colombia",
    Country == "NIR" ~ "Northern Ireland",
    Country == "HRV" ~ "Croatia",
    Country == "NOR" ~ "Norway",
    Country == "CYP" ~ "Cyprus",
    Country == "PAK" ~ "Pakistan",
    Country == "CZE" ~ "Czechia",
    Country == "PER" ~ "Peru",
    Country == "DNK" ~ "Denmark",
    Country == "PHL" ~ "Philippines",
    Country == "ECU" ~ "Ecuador",
    Country == "POL" ~ "Poland",
    Country == "EGY" ~ "Egypt",
    Country == "PRT" ~ "Portugal",
    Country == "EST" ~ "Estonia",
    Country == "PRI" ~ "Puerto Rico",
    Country == "ETH" ~ "Ethiopia",
    Country == "ROU" ~ "Romania",
    Country == "FIN" ~ "Finland",
    Country == "RUS" ~ "Russian Federation",
    Country == "FRA" ~ "France",
    Country == "SRB" ~ "Serbia",
    Country == "GEO" ~ "Georgia",
    Country == "SGP" ~ "Singapore",
    Country == "DEU" ~ "Germany",
    Country == "SVK" ~ "Slovakia",
    Country == "GBR" ~ "Great Britain",
    Country == "SVN" ~ "Slovenia",
    Country == "GRC" ~ "Greece",
    Country == "KOR" ~ "South Korea",
    Country == "GTM" ~ "Guatemala",
    Country == "ESP" ~ "Spain",
    Country == "HKG" ~ "Hong Kong SAR PRC",
    Country == "SWE" ~ "Sweden",
    Country == "HUN" ~ "Hungary",
    Country == "CHE" ~ "Switzerland",
    Country == "ISL" ~ "Iceland",
    Country == "TWN" ~ "Taiwan ROC",
    Country == "IDN" ~ "Indonesia",
    Country == "TJK" ~ "Tajikistan",
    Country == "IRN" ~ "Iran",
    Country == "THA" ~ "Thailand",
    Country == "IRQ" ~ "Iraq",
    Country == "TUN" ~ "Tunisia",
    Country == "ITA" ~ "Italy",
    Country == "TUR" ~ "Turkey",
    Country == "JPN" ~ "Japan",
    Country == "UKR" ~ "Ukraine",
    Country == "JOR" ~ "Jordan",
    Country == "USA" ~ "United States",
    Country == "KAZ" ~ "Kazakhstan",
    Country == "URY" ~ "Uruguay",
    Country == "KEN" ~ "Kenya",
    Country == "VNM" ~ "Vietnam",
    Country == "KGZ" ~ "Kyrgyzstan",
    Country == "ZWE" ~ "Zimbabwe",
    Country == "LVA" ~ "Latvia",
    Country == "VEN" ~ "Venezuela",
    TRUE ~ Country  # Keep the original value if it doesn't match any of the specified conditions
  ))


wvs$Country <- sapply(wvs$Country, standardize_country_name)

wvs <- wvs %>%
  rename(Family_importance = Q1,
         Friends_importance = Q2, 
         Politics_importance = Q4,
         Religion_importance = Q6,
         Tolerance_respect_others = Q12,
         Feeling_happiness = Q46,
         Satisfaction_life = Q49,
         Trust_people = Q57,
         Confidence_government = Q71,
         Confidence_political_parties = Q72,
         Confidence_parliament = Q73,
         Confidence_elections = Q76,
         Active_member = Q98,
         Member_political_party = Q98R,
         Income_equality = Q106,
         Info_social_media = Q207,
         Online_actions = Q217)

wvs <- wvs %>%
  mutate(Tolerance_respect_others = case_when(
    Tolerance_respect_others %in% c(1, 2) ~ Tolerance_respect_others,
    TRUE ~ NA_integer_
  ), 
  Feeling_happiness = case_when(
    Feeling_happiness %in% c(1:4) ~ Feeling_happiness,
    TRUE ~ NA_integer_
  ),
  Satisfaction_life = case_when(
    Satisfaction_life %in% c(1:10) ~ Satisfaction_life,
    TRUE ~ NA_integer_
  ),
  Trust_people = case_when(
    Trust_people %in% c(1,2) ~ Trust_people,
    TRUE ~ NA_integer_
  ),
  Confidence_government = case_when(
    Confidence_government %in% c(1:4) ~ Confidence_government,
    TRUE ~ NA_integer_
  ),
  Income_equality = case_when(
    Income_equality %in% c(1:10) ~ Income_equality,
    TRUE ~ NA_integer_
  ),
  Family_importance = case_when(
    Family_importance %in% c(1:4) ~ Family_importance,
    TRUE ~ NA_integer_
  ),
  Friends_importance = case_when(
    Friends_importance %in% c(1:4) ~ Friends_importance,
    TRUE ~ NA_integer_
  ),
  Politics_importance = case_when(
    Politics_importance %in% c(1:4) ~ Politics_importance,
    TRUE ~ NA_integer_
  ),
  Religion_importance = case_when(
    Religion_importance %in% c(1:4) ~ Religion_importance,
    TRUE ~ NA_integer_
  ),
  Confidence_political_parties = case_when(
    Confidence_political_parties %in% c(1:4) ~ Confidence_political_parties,
    TRUE ~ NA_integer_
  ),
  Confidence_parliament = case_when(
    Confidence_parliament %in% c(1:4) ~ Confidence_parliament,
    TRUE ~ NA_integer_
  ),
  Confidence_elections = case_when(
    Confidence_elections %in% c(1:4) ~ Confidence_elections,
    TRUE ~ NA_integer_
  ),
  Active_member = case_when(
    Active_member %in% c(0:2) ~ Active_member,
    TRUE ~ NA_integer_
  ),
  Member_political_party = case_when(
    Member_political_party %in% c(0, 1) ~ Member_political_party,
    TRUE ~ NA_integer_
  ),
  Info_social_media = case_when(
    Info_social_media %in% c(1:5) ~ Info_social_media,
    TRUE ~ NA_integer_
  ),  
  Online_actions = case_when(
    Online_actions %in% c(1:3) ~ Online_actions,
    TRUE ~ NA_integer_
  ))
    

wvs_aggregated <- wvs %>%
  group_by(A_YEAR, A_WAVE, Country) %>%
  summarise(
    Tolerance_respect_others = mean(Tolerance_respect_others, na.rm = TRUE),  
    Feeling_happiness = mean(Feeling_happiness, na.rm = TRUE),                
    Satisfaction_life = mean(Satisfaction_life, na.rm = TRUE),                 
    Trust_people = mean(Trust_people, na.rm = TRUE),                           
    Confidence_government = mean(Confidence_government, na.rm = TRUE),       
    Income_equality = mean(Income_equality, na.rm = TRUE),
    Family_importance = mean(Family_importance, na.rm = TRUE),
    Friends_importance = mean(Friends_importance, na.rm = TRUE), 
    Politics_importance = mean(Politics_importance, na.rm = TRUE), 
    Religion_importance = mean(Religion_importance, na.rm = TRUE),
    Confidence_political_parties = mean(Confidence_political_parties, na.rm = TRUE),
    Confidence_parliament = mean(Confidence_parliament, na.rm = TRUE),
    Confidence_elections = mean(Confidence_elections, na.rm = TRUE), 
    Info_social_media = mean(Info_social_media, na.rm = TRUE),
    Online_actions = mean(Online_actions, na.rm = TRUE)
  )

wvs_tolerance <- wvs %>%
  group_by(A_YEAR, Country) %>%
  summarise(
    total_ones = sum(Tolerance_respect_others == 1, na.rm = TRUE),
    total_ones_and_twos = sum(Tolerance_respect_others %in% c(1, 2), na.rm = TRUE)
  ) %>%
  mutate(Percentage_tolerance = (total_ones / total_ones_and_twos) * 100)

wvs_trust <- wvs %>%
  group_by(A_YEAR, Country) %>%
  summarise(
    total_ones = sum(Trust_people == 1, na.rm = TRUE),
    total_ones_and_twos = sum(Trust_people %in% c(1, 2), na.rm = TRUE)
  ) %>%
  mutate(Percentage_trust = (total_ones / total_ones_and_twos) * 100)

wvs_active_member <- wvs %>%
  group_by(A_YEAR, Country) %>% 
  summarise(
    total_active = sum(Active_member == 2, na.rm = TRUE), 
    total_member = sum(Active_member %in% c(1,2), na.rm = TRUE),
    total_all = sum(Active_member %in% c(0,1,2), na.rm = TRUE)
  ) %>%
  mutate(Percentage_active_members = (total_active/total_all)*100, 
         Percentage_members = (total_member/total_all)*100)

wvs_member_political_party <- wvs %>% 
  group_by(A_YEAR, Country) %>%
  summarise(
    total_ones = sum(Member_political_party == 1, na.rm = TRUE),
    total_zeroes_and_ones = sum(Member_political_party %in% c(0,1), na.rm = TRUE)
  ) %>%
  mutate(Percentage_poli_members = (total_ones / total_zeroes_and_ones) * 100)
  

wvs_data <- merge(wvs_aggregated, wvs_tolerance, by = c("Country", "A_YEAR"), all = TRUE)
wvs_data <- merge(wvs_data, wvs_trust, by = c("Country", "A_YEAR"), all = TRUE)
wvs_data <- merge(wvs_data, wvs_active_member, by = c("Country", "A_YEAR"), all = TRUE)
wvs_data <- merge(wvs_data, wvs_member_political_party, by = c("Country", "A_YEAR"), all = TRUE)

wvs_data <- wvs_data %>%
  select(Country, Feeling_happiness, Satisfaction_life, Confidence_government, Income_equality, 
         Percentage_tolerance, Percentage_trust, Family_importance,Friends_importance, 
         Politics_importance,Religion_importance, Confidence_political_parties, Confidence_parliament,
         Confidence_elections, Info_social_media, Percentage_active_members, Percentage_members, Percentage_poli_members)


wvs_data$Country <- sapply(wvs_data$Country, standardize_country_name)

gb_row <- wvs_data[wvs_data$Country == "Great Britain", ]
ni_row <- wvs_data[wvs_data$Country == "Northern Ireland", ]
uk_row <- data.frame(
  Country = "United Kingdom of Great Britain and Northern Ireland",
  Feeling_happiness = (gb_row$Feeling_happiness + ni_row$Feeling_happiness) / 2,
  Satisfaction_life = (gb_row$Satisfaction_life + ni_row$Satisfaction_life) / 2,
  Confidence_government = (gb_row$Confidence_government + ni_row$Confidence_government) / 2,
  Income_equality = (gb_row$Income_equality + ni_row$Income_equality) / 2,
  Percentage_tolerance = (gb_row$Percentage_tolerance + ni_row$Percentage_tolerance) / 2,
  Percentage_trust = (gb_row$Percentage_trust + ni_row$Percentage_trust) / 2,
  Family_importance = (gb_row$Family_importance + ni_row$Family_importance) / 2,
  Friends_importance = (gb_row$Friends_importance + ni_row$Friends_importance) / 2,
  Politics_importance = (gb_row$Politics_importance + ni_row$Politics_importance) / 2,
  Religion_importance = (gb_row$Religion_importance + ni_row$Religion_importance) / 2,
  Confidence_political_parties = (gb_row$Confidence_political_parties + ni_row$Confidence_political_parties) / 2,
  Confidence_parliament = (gb_row$Confidence_parliament + ni_row$Confidence_parliament) / 2,
  Confidence_elections = (gb_row$Confidence_elections + ni_row$Confidence_elections) / 2,
  Info_social_media = (gb_row$Info_social_media + ni_row$Info_social_media) / 2, 
  Percentage_active_members = (gb_row$Percentage_active_members + ni_row$Percentage_active_members) / 2,
  Percentage_members = (gb_row$Percentage_members + ni_row$Percentage_members) / 2,
  Percentage_poli_members = (gb_row$Percentage_poli_members + ni_row$Percentage_poli_members) / 2
)

wvs_data <- rbind(wvs_data, uk_row)

# Add to dataset
data <- full_join(data, wvs_data, by = "Country")

data_countries <- as.character(data$Country)
wvs_countries <- as.character(wvs_data$Country)

# Find countries in data but not in vdem
not_in_wvs <- setdiff(data_countries, wvs_countries)

# Find countries in vdem but not in data
not_in_data <- setdiff(wvs_countries, data_countries)

# data <- data[data$Country %in% tpt_countries, ]
rm(wvs)
rm(wvs_aggregated)
rm(wvs_data)
rm(wvs_tolerance)
rm(wvs_trust)
rm(wvs_active_member)
rm(wvs_member_political_party)
rm(ni_row)
rm(uk_row)
rm(gb_row)

# Import integrated values survey
#######################################################################################################
setwd("C:\\Users\\KKAY\\OneDrive - Rode Kruis-Vlaanderen\\General\\Paper 3 - Other outcomes\\Political Polarization")
ivs <- read.csv("self-reported-trust-attitudes.csv", header = TRUE)
ivs <- ivs %>%
  rename(Country = Entity)
#Keep latest year for each country 
ivs <- ivs %>% arrange(Country, desc(Year))
ivs <- ivs %>% group_by(Country) %>% slice(1)
ivs <- ivs %>%
  select(Country, Agree.Most.people.can.be.trusted)

ivs$Country <- sapply(ivs$Country, standardize_country_name)

ivs <- na.omit(ivs)
summary(ivs)
sd(ivs$Agree.Most.people.can.be.trusted)
median(ivs$Agree.Most.people.can.be.trusted)

data <- full_join(data, ivs, by = "Country")

data_countries <- as.character(data$Country)
ivs_countries <- as.character(ivs$Country)

# Find countries in data but not in vdem
not_in_ivs <- setdiff(data_countries, ivs_countries)
not_in_ivs

# Find countries in vdem but not in data
not_in_data <- setdiff(ivs_countries, data_countries)

# data <- data[data$Country %in% tpt_countries, ]
rm(ivs)

# Import government expenditure (% of GDP)
#######################################################################################################
govexp <- read.csv("API_GC.XPN.TOTL.GD.ZS_DS2_en_csv_v2_42639.csv", skip = 4, header = TRUE)
govexp <- govexp %>%
  select(Country.Name, X2012, X2013, X2014, X2015, X2016, X2017, X2018, X2019, X2020, X2021, X2022) %>%
  rename(Country = Country.Name)

govexp$Govt_expenditure <- rowMeans(govexp[, 2:12], na.rm = TRUE)

govexp$Country <- sapply(govexp$Country, standardize_country_name)

govexp$Govt_expenditure[is.nan(govexp$Govt_expenditure)] <- NA

# Add to dataset
data <- full_join(data, govexp, by = "Country")

data_countries <- as.character(data$Country)
govexp_countries <- as.character(govexp$Country)

# Find countries in data but not in vdem
not_in_govexp <- setdiff(data_countries, govexp_countries)

# Find countries in vdem but not in data
not_in_data <- setdiff(govexp_countries, data_countries)

# data <- data[data$Country %in% tpt_countries, ]
rm(govexp)

# Import GINI Index 
#######################################################################################################
library(dbplyr)
library(tidyr)
gini <- read.xlsx("API_SI.POV.GINI_DS2_en_excel_v2_39.xls", sheetName = "Data", startRow = 4, header = TRUE)

gini <- gini %>%
  pivot_longer(
    cols = starts_with("X"),
    names_to = "Year",
    names_prefix = "X",
    values_to = "Value",
    values_drop_na = TRUE
  )

gini$Year <- as.numeric(gini$Year)

gini <- gini %>%
  group_by(Country.Name, Country.Code) %>%
  filter(Year == max(Year)) %>%
  slice_max(Year, with_ties = FALSE) %>%
  ungroup()

gini <- gini %>%
  select(Country.Name, Value)

gini <- gini %>%
  rename(Country = Country.Name,
         Gini_index = Value)

gini$Country <- sapply(gini$Country, standardize_country_name)

data <- full_join(data, gini, by = "Country")

rm(gini)

# Import inclusiveness data
#######################################################################################################
# From https://belonging.berkeley.edu/2020-inclusiveness-index-raw-data-sets
inclusive <- read.xlsx("global_data_for_website_2020.xlsx", header = TRUE, sheetName = "Inclusiveness")
inclusive <- inclusive %>% 
  mutate_all(~ replace(., . == 9999, NA))
inclusive <- inclusive %>%
  select(Country, Inclusiveness.index.2020)

inclusive$Country <- sapply(inclusive$Country, standardize_country_name)

# Add to dataset
data <- full_join(data, inclusive, by = "Country")

data_countries <- as.character(data$Country)
inclusive_countries <- as.character(inclusive$Country)

# Find countries in data but not in vdem
not_in_inclusive <- setdiff(data_countries, inclusive_countries)

# Find countries in vdem but not in data
not_in_data <- setdiff(inclusive_countries, data_countries)

# data <- data[data$Country %in% tpt_countries, ]
rm(inclusive)

# Import Facebook Users by Country data 
#######################################################################################################
# https://wisevoter.com/country-rankings/facebook-users-by-country/

facebook <- read.xlsx("Facebook Users.xlsx", header = TRUE, sheetIndex = 1)

facebook$Country <- sapply(facebook$Country, standardize_country_name)

data <- full_join(data, facebook, by = "Country")

rm(facebook)

# Import Our World In Data sets - global burden, happiness, number of internet users 
#######################################################################################################
owid <- read.csv("burden-disease-from-each-mental-illness.csv", header = TRUE) #mental illness
owid <- owid %>%
  group_by(Entity, Code) %>%
  filter(Year == max(Year)) %>%
  slice_max(Year, with_ties = FALSE) %>%
  ungroup() %>%
  select(-c(Year,Code))
owid2 <- read.csv("happiness-cantril-ladder.csv", header = TRUE) #happiness, scale from 0 to 10
owid2 <- owid2 %>%
  group_by(Entity, Code) %>%
  filter(Year == max(Year)) %>%
  slice_max(Year, with_ties = FALSE) %>%
  ungroup() %>%
  select(-c(Year,Code))
owid3 <- read.csv("number-of-internet-users.csv", header = TRUE) #number of internet users
owid3 <- owid3 %>%
  group_by(Entity, Code) %>%
  filter(Year == max(Year)) %>%
  slice_max(Year, with_ties = FALSE) %>%
  ungroup() %>%
  select(-c(Year,Code))
owid4 <- read.csv("one-person-households.csv", header = TRUE) #loneliness, percentage
owid4 <- owid4 %>%
  group_by(Entity, Code) %>%
  filter(Year == max(Year)) %>%
  slice_max(Year, with_ties = FALSE) %>%
  ungroup() %>%
  select(-c(Year,Code))
owid5 <- read.csv("human-development-index.csv", header = TRUE) 
owid5 <- owid5 %>%
  group_by(Entity, Code) %>%
  filter(Year == max(Year)) %>%
  slice_max(Year, with_ties = FALSE) %>%
  ungroup() %>%
  select(-c(Year,Code))


owid <- full_join(owid, owid2, by = c("Entity"))
owid <- full_join(owid, owid3, by = c("Entity"))
owid <- full_join(owid, owid4, by = c("Entity"))
owid <- full_join(owid, owid5, by = c("Entity"))

owid <- owid %>%
  dplyr::rename(Country = Entity,
         Depressive_disorders = DALYs.from.depressive.disorders.per.100.000.people.in..both.sexes.aged.age.standardized, 
         Schizophrenia = DALYs.from.schizophrenia.per.100.000.people.in..both.sexes.aged.age.standardized,
         Bipolar_disorders = DALYs.from.bipolar.disorder.per.100.000.people.in..both.sexes.aged.age.standardized,
         Eating_disorders = DALYs.from.eating.disorders.per.100.000.people.in..both.sexes.aged.age.standardized,
         Anxiety_disorders= DALYs.from.anxiety.disorders.per.100.000.people.in..both.sexes.aged.age.standardized,
         Happiness = Cantril.ladder.score, 
         Internet_users = Number.of.Internet.users,
         Loneliness = Share.of.one.person.households)


owid$Country <- sapply(owid$Country, standardize_country_name)

data <- full_join(data, owid, by = "Country")

rm(owid)
rm(owid2)
rm(owid3)
rm(owid4)
rm(owid5)

# Import Pew Research Centre Data on Social Media
#######################################################################################################
# Do you ever use online social media sites like Facebook, Twitter, Instagram, [INSERT COUNTRY SPECIFIC EXAMPLES]
library(readxl)

pew <- read_excel("PEW Data Social Media Use.xlsx", sheet = "PEW", skip = 2)

pew$Country <- sapply(pew$Country, standardize_country_name)

data <- full_join(data, pew, by = "Country")

rm(pew)

# Import ESS Data on Life Satisfaction
#######################################################################################################
ess <- read.csv("ESS6e02_6-ESS7e02_3-ESS8e02_3-ESS9e03_2-ESS10-subset.csv", header = TRUE)


country_names <- c(
  AL = "Albania",
  AT = "Austria",
  BE = "Belgium",
  BG = "Bulgaria",
  CH = "Switzerland",
  CY = "Cyprus",
  CZ = "Czechia",
  DE = "Germany",
  DK = "Denmark",
  EE = "Estonia",
  ES = "Spain",
  FI = "Finland",
  FR = "France",
  GB = "United Kingdom",
  GE = "Georgia",
  GR = "Greece",
  HR = "Croatia",
  HU = "Hungary",
  IE = "Ireland",
  IS = "Iceland",
  IL = "Israel",
  IT = "Italy",
  LT = "Lithuania",
  LU = "Luxembourg",
  LV = "Latvia",
  ME = "Montenegro",
  MK = "Macedonia",
  NL = "Netherlands",
  NO = "Norway",
  PL = "Poland",
  PT = "Portugal",
  RO = "Romania",
  RS = "Serbia",
  RU = "Russia",
  SE = "Sweden",
  SI = "Slovenia",
  SK = "Slovak Republic",
  TR = "Turkey",
  UA = "Ukraine",
  XK = "Kosovo"
)
ess <- ess %>%
  select(cntry, stflife) %>%
  mutate(cntry = country_names[cntry])

ess <- ess %>%
  mutate(stflife = as.numeric(stflife)) %>%
  mutate(stflife = case_when(
    stflife %in% 0:9 ~ stflife,
    TRUE ~ NA_real_
  ))

ess <- ess %>%
  group_by(cntry) %>%
  summarise(mean_stflife = mean(stflife, na.rm = TRUE))

ess <- ess %>%
  rename(Country = cntry)

ess$Country <- sapply(ess$Country, standardize_country_name)

data <- full_join(data, ess, by = "Country")

rm(ess)

#Import Population Data
#######################################################################################################

pop <- read.csv("API_SP.POP.TOTL_DS2_en_csv_v2_277159.csv", skip = 4, header = TRUE)

pop <- pop %>%
  rename(Country = Country.Name,
         Population_2022 = X2022) %>%
  select(Country, Population_2022)

pop$Country <- sapply(pop$Country, standardize_country_name)

data <- full_join(data, pop, by = "Country")

rm(pop)

# Import GDP Data
#######################################################################################################
library(tidyr)
library(dbplyr)
gdp <- read.csv("API_NY.GDP.MKTP.KD_DS2_en_csv_v2_279473.csv", skip = 4, header = TRUE) #GDP Constant 2015 $

gdp <- gdp %>%
  rename(Country = Country.Name) %>%
  pivot_longer(
    cols = starts_with("X"), 
    names_to = "Year", 
    values_to = "GDP",
    names_prefix = "X",
    names_transform = list(Year = as.numeric)
  )

# Group by country and filter for the most recent non-NA value
gdp <- gdp %>%
  arrange(Country, desc(Year)) %>%
  group_by(Country) %>%
  filter(!is.na(GDP)) %>%
  slice_head(n = 1) %>%
  ungroup()

# Rename the GDP column to indicate it contains the most recent values
gdp <- gdp %>%
  rename(GDP_Most_Recent = GDP)

# Select and arrange columns as needed
gdp <- gdp %>%
  select(Country, Year, GDP_Most_Recent)

gdp$Country <- sapply(gdp$Country, standardize_country_name)

data <- full_join(data, gdp, by = "Country")

rm(gdp)


#TRANSFORMATIONS ACROSS VARIABLES 
#######################################################################################################
#######################################################################################################
#######################################################################################################
# Calculation Facebook Users per 100,000 inhabitants with 2022 population data 
data$Facebook_users_per100000 <- (data$Number_of_users/data$Population_2022)*100000

# Calculation Internet Users per 100,000 inhabitants with 2022 population data 
data$Internet_users_per100000 <- (data$Internet_users/data$Population_2022)*100000

#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################
#######################################################################################################


# Save all combined data to folder 

write.xlsx(data, "All Data_December.xlsx")

# Rename dataset 
mediation_all <- data %>%
  dplyr::rename(
    Trust_in_govt_oecd = Mean_3,
    Civil_society_index_vdem = v2xcs_ccsi,
    Percent_pop_suffrage_vdem = v2elsuffrage,
    Election_turnout_vdem = v2eltrnout,
    Education_equality_vdem = v2peedueq_mean,
    Poli_polarization_vdem = v2cacamps_mean,
    Civil_liberties_index_vdem = v2x_civlib,
    Life_expectancy_vdem = e_pelifeex,
    Democracy_index_eiu = Democracy.Index,
    Election_turnout_gdturnout = avg_turnout, 
    Democracy_polity = democ,
    Third_pillar_size = Total.x,
    Poli_participation_eiu = pol_part_eiu,
    Feeling_happiness_wvs = Feeling_happiness,
    Satisfaction_life_wvs = Satisfaction_life,
    Confidence_government_wvs = Confidence_government,
    Income_equality_wvs = Income_equality,
    Percentage_tolerance_wvs = Percentage_tolerance,
    Percentage_trust_wvs = Percentage_trust,
    Family_importance_wvs = Family_importance,
    Friends_importance_wvs = Friends_importance,
    Politics_importance_wvs = Politics_importance,
    Religion_importance_wvs = Religion_importance,
    Confidence_political_parties_wvs = Confidence_political_parties,
    Confidence_parliament_wvs = Confidence_parliament,
    Confience_elections_wvs = Confidence_elections,
    Info_social_media_wvs = Info_social_media,
    Percentage_active_members_wvs = Percentage_active_members,
    Percentage_members_wvs = Percentage_members,
    Percentage_poli_members_wvs = Percentage_poli_members,
    Inclusiveness_obi = Inclusiveness.index.2020,
    Govt_expenditure_wb = Govt_expenditure,
    Gdp_per_capita_wb = gdpPerCapita, 
    World_bank_income_group = World.Bank.Income.Group,
    Trust_ivs = Agree.Most.people.can.be.trusted,
    Depressive_disorders_owid = Depressive_disorders,
    Schizophrenia_owid = Schizophrenia,
    Bipolar_disorders_owid = Bipolar_disorders,
    Eating_disorders_owid = Eating_disorders,
    Anxiety_disorders_owid = Anxiety_disorders,
    Happiness_owid = Happiness, 
    Internet_users_owid = Internet_users_per100000,
    Loneliness_owid = Loneliness,
    Human_development_index_owid = Human.Development.Index,
    Social_media_used_pew = Yes,
    Gini_index_wb = Gini_index, 
    Facebook_users_wv = Facebook_users_per100000,
    Life_satisfaction_ess = mean_stflife
  ) %>%
  dplyr::select(
    Country,
    Third_pillar_size,
    Trust_in_govt_oecd,
    Election_turnout_vdem,
    Poli_polarization_vdem,
    Election_turnout_gdturnout, 
    Democracy_polity,
    Democracy_index_eiu,
    Poli_participation_eiu,
    Feeling_happiness_wvs,
    Satisfaction_life_wvs,
    Confidence_government_wvs,
    Income_equality_wvs,
    Percentage_tolerance_wvs,
    Percentage_trust_wvs,
    Trust_ivs,
    Family_importance_wvs,
    Friends_importance_wvs, 
    Politics_importance_wvs, 
    Religion_importance_wvs,
    Confidence_political_parties_wvs,
    Confidence_parliament_wvs,
    Confience_elections_wvs,
    Info_social_media_wvs,
    Percentage_active_members_wvs,
    Percentage_members_wvs,
    Percentage_poli_members_wvs,
    Inclusiveness_obi,
    Depressive_disorders_owid,
    Schizophrenia_owid,
    Bipolar_disorders_owid,
    Eating_disorders_owid,
    Anxiety_disorders_owid,
    Happiness_owid,
    Internet_users_owid,
    Loneliness_owid,
    Human_development_index_owid,
    Social_media_used_pew,
    Gini_index_wb,
    Facebook_users_wv,
    Life_satisfaction_ess,
    Govt_expenditure_wb,
    Gdp_per_capita_wb, 
    World_bank_income_group, 
    Population_2022, 
    GDP_Most_Recent
  )

objects <- ls()
rm(list = setdiff(objects, c("data", "mediation_all")))
objects <- ls()
rm(list = setdiff(objects, c("data", "mediation_all")))

library(ggplot2)
library(ggrepel)
library(dtplyr)
library(psych)
library(writexl)
library(psych)
library(gridExtra)
library(car)
library(lmtest)

# Save clean data to folder 
write.xlsx(mediation_all, "Clean Data All Income Groups.xlsx")

# Modify which countries are included based on income (high income = 39, upper-middle = 34, lower-middle = 34)
income <- table(mediation_all$World_bank_income_group)
income

mediation_upper <- mediation_all %>%
  filter(World_bank_income_group %in% c("Upper-middle income", "High income"))
write.xlsx(mediation_upper, "Clean Data Upper Income Groups.xlsx")

mediation_lower <- mediation_all %>%
  filter(World_bank_income_group %in% c("Low income", "Lower-middle income"))
write.xlsx(mediation_lower, "Clean Data Lower Income Groups.xlsx")


# Correlational Analysis

# Polarization and third pillar - simple linear models

## All incomes - reported in paper as first analysis
polarization_all <- mediation_all[, c("Country", "Third_pillar_size", "Poli_polarization_vdem")]
polarization_all <- na.omit(polarization_all)
summary(polarization_all)
sd(polarization_all$Third_pillar_size)
sd(polarization_all$Poli_polarization_vdem)

#Describing Data 
num_countries_over_10 <- sum(polarization_all$Third_pillar_size > 10)
num_countries_over_10

num_countries_over_75 <- sum(polarization_all$Poli_polarization_vdem > 75)
num_countries_over_75
num_countries_over_95 <- sum(polarization_all$Poli_polarization_vdem > 95)
num_countries_over_95
countries_over_95 <- polarization_all$Country[polarization_all$Poli_polarization_vdem > 95]
print(countries_over_95)

# Analysis 
correlation_polarization_all <- cor(polarization_all$Third_pillar_size, polarization_all$Poli_polarization_vdem, method = "pearson")
pvalue <- cor.test(polarization_all$Third_pillar_size, polarization_all$Poli_polarization_vdem)$p.value
pvalue

polarization_all_lm <- lm(Poli_polarization_vdem ~ Third_pillar_size, data = polarization_all)
summary(polarization_all_lm)

# Assumptions

plot(polarization_all_lm$fitted.values, residuals(polarization_all_lm), 
     main = "Residuals vs Fitted", 
     xlab = "Fitted values", 
     ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(residuals(polarization_all_lm))
qqline(residuals(polarization_all_lm), col = "red")

shapiro.test(residuals(polarization_all_lm))

durbinWatsonTest(polarization_all_lm)

bptest(polarization_all_lm)

# Scatter plot with linear regression line
linear_regression <- ggplot(polarization_all, aes(x = Third_pillar_size, y = Poli_polarization_vdem, label = Country)) +
  geom_point(shape = 20) +
  geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.5, fullrange = TRUE) +
  geom_text_repel(
    hjust = -0.90, vjust = 0.5, size = 2.5, max.overlaps = 3, family = "Times New Roman"
  ) + 
  labs(x = "Third Sector Size (%)", y = "Political Polarization (%)") + 
  annotate("text", x = 18, y = 3.7, 
           label = paste("Correlation: -0.40"),
           family = "Times New Roman") +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 100)) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        text = element_text(family = "Times New Roman"))
ggsave("Scatterplot Linear Regression.jpg", plot = linear_regression, device = "jpg")

## High incomes
polarization_upper <- mediation_upper[, c("Country", "Third_pillar_size", "Poli_polarization_vdem")]
polarization_upper <- na.omit(polarization_upper)

correlation_polarization_upper <- cor(polarization_upper$Third_pillar_size, polarization_upper$Poli_polarization_vdem, method = "pearson")

polarization_upper_lm <- lm(Poli_polarization_vdem ~ Third_pillar_size, data = polarization_upper)
summary(polarization_upper_lm)

# Assumptions

plot(polarization_upper_lm$fitted.values, residuals(polarization_upper_lm), 
     main = "Residuals vs Fitted", 
     xlab = "Fitted values", 
     ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(residuals(polarization_upper_lm))
qqline(residuals(polarization_upper_lm), col = "red")

shapiro.test(residuals(polarization_upper_lm))

durbinWatsonTest(polarization_upper_lm)

bptest(polarization_upper_lm)

# Scatter plot with linear regression line
plothigh <- ggplot(polarization_upper, aes(x = Third_pillar_size, y = Poli_polarization_vdem, label = Country)) +
  geom_point(shape = 20) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.5) +
  geom_text_repel(
    hjust = -0.50, vjust = 0.6, size = 2.5, max.overlaps = 3, family = "Times New Roman"
  ) + 
  labs(x = "Third Sector Size (%)", y = "Political Polarization (%)", title = "High and Upper-Middle Income") + 
  annotate("text", x = 15, y = 3.7, 
           label = paste("Correlation: -0.42"), family = "Times New Roman") +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 100)) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        text = element_text(family = "Times New Roman"))
ggsave("Scatterplot High Income Regression.jpg", plot = plothigh, device = "jpg")

## Low incomes 
polarization_lower <- mediation_lower[, c("Country", "Third_pillar_size", "Poli_polarization_vdem")]
polarization_lower <- na.omit(polarization_lower)

correlation_polarization_lower <- cor(polarization_lower$Third_pillar_size, polarization_lower$Poli_polarization_vdem, method = "pearson")

polarization_lower_lm <- lm(Poli_polarization_vdem ~ Third_pillar_size, data = polarization_lower)
summary(polarization_lower_lm)

# Assumptions

plot(polarization_lower_lm$fitted.values, residuals(polarization_lower_lm), 
     main = "Residuals vs Fitted", 
     xlab = "Fitted values", 
     ylab = "Residuals")
abline(h = 0, col = "red")

qqnorm(residuals(polarization_lower_lm))
qqline(residuals(polarization_lower_lm), col = "red")

shapiro.test(residuals(polarization_lower_lm))

durbinWatsonTest(polarization_lower_lm)

bptest(polarization_lower_lm)

# Scatter plot with linear regression line
plotlow <- ggplot(polarization_lower, aes(x = Third_pillar_size, y = Poli_polarization_vdem, label = Country)) +
  geom_point(shape = 20) +
  geom_smooth(method = "lm", se = FALSE, color = "red", size = 0.5) +
  geom_text_repel(
    hjust = -0.50, vjust = 0.6, size = 2.5, max.overlaps = 3, family = "Times New Roman"
  ) + 
  labs(x = "Third Sector Size (%)", y = "Political Polarization (%)", title = "Low and Lower-Middle Income") + 
  annotate("text", x = 15, y = 3.7, 
           label = paste("Correlation: -0.24"), family = "Times New Roman") +
  scale_x_continuous(limits = c(0, 20)) +
  scale_y_continuous(limits = c(0, 100)) + 
  theme_bw() +
  theme(panel.border = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 10),
        text = element_text(family = "Times New Roman"))
ggsave("Scatterplot Low Income Regression.jpg", plot = plotlow, device = "jpg")


both <- grid.arrange(plothigh, plotlow, ncol = 2) 
ggsave("Scatterplot High and Low Income Regression.jpg", plot = both, device = "jpg")

#Income groups with polarization and the third pillar 
complete_polarization_income <- mediation_all[, c("Country", "Third_pillar_size", "Poli_polarization_vdem", "World_bank_income_group")]
complete_polarization_income <- na.omit(complete_polarization_income)
complete_polarization_income <- complete_polarization_income %>%
  mutate(World_bank_income_group = factor(World_bank_income_group, 
                                          levels = c("Low income", "Lower-middle income", 
                                                     "Upper-middle income", "High income")))

income_plot <- ggplot(complete_polarization_income, aes(x = Third_pillar_size, y = Poli_polarization_vdem)) +
  geom_point(shape = 20, aes(color = World_bank_income_group)) + 
  geom_smooth(method = "lm", se = FALSE, size = 0.5, aes(color = World_bank_income_group)) +
  labs(x = "Third Sector Size (%)", y = "Political Polarization (%)", color = "Income Group") +
  theme_minimal() +
  theme(text = element_text(family = "Times New Roman"),
        legend.position = c(0.8,0.8)) 
ggsave("Scatterplot Income Groups.jpg", plot = income_plot, device = "jpg")

correlation <- complete_polarization_income %>%
  group_by(World_bank_income_group) %>%
  summarize(
    correlation = cor(Third_pillar_size, Poli_polarization_vdem, use = "complete.obs"),
    p_value = cor.test(Third_pillar_size, Poli_polarization_vdem, use = "complete.obs")$p.value,
    lower_ci = cor.test(Third_pillar_size, Poli_polarization_vdem, use = "complete.obs")$conf.int[1],
    upper_ci = cor.test(Third_pillar_size, Poli_polarization_vdem, use = "complete.obs")$conf.int[2]
  )
correlation

income <- table(mediation_all$World_bank_income_group)
income

#GDP per capita (continuous variable instead of categorical)
polarization_gdp <- mediation_all[, c("Country", "GDP_Most_Recent", "Third_pillar_size", "Poli_polarization_vdem")]
polarization_gdp <- na.omit(polarization_gdp)

polarization_gdp_lm <- lm(Poli_polarization_vdem ~ Third_pillar_size + GDP_Most_Recent, data = polarization_gdp)
summary(polarization_gdp_lm)


# Mediation Analysis 

# UPPER-MIDDLE AND HIGH-INCOME GROUPS 
#######################################

mediation <- mediation_upper[, c("Country", "Third_pillar_size", "Poli_polarization_vdem", "Trust_ivs","Gini_index_wb", "Facebook_users_wv","Loneliness_owid", "GDP_Most_Recent")]
mediation <- na.omit(mediation)

#Correlation matrix for final selection of variables 
correlation_mediation <- mediation_upper[, c("Country", "Third_pillar_size", "Poli_polarization_vdem", "Trust_ivs","Gini_index_wb", "Facebook_users_wv","Loneliness_owid", "GDP_Most_Recent")]
correlation_mediation <- na.omit(correlation_mediation)
lowerCor(correlation_mediation)
correlation_final <- as.data.frame(lowerCor(correlation_mediation))
write_xlsx(correlation_final, "Correlation Matrix Final.xlsx")

#Descriptives for final selection of variables 
descriptives_mediation <- mediation_upper[, c("Country", "Third_pillar_size", "Poli_polarization_vdem", "Trust_ivs","Gini_index_wb", "Facebook_users_wv","Loneliness_owid", "GDP_Most_Recent")]
descriptives_mediation <- na.omit(descriptives_mediation)
summary(descriptives_mediation)
sd(descriptives_mediation$Third_pillar_size)
sd(descriptives_mediation$Poli_polarization_vdem)
sd(descriptives_mediation$Facebook_users_wv)
sd(descriptives_mediation$Gini_index_wb)
sd(descriptives_mediation$Loneliness_owid)
sd(descriptives_mediation$Trust_ivs)
descriptives_final <- psych::describe(descriptives_mediation) #descriptive statistics
descriptives_final$Variable <- rownames(descriptives_final)
write_xlsx(descriptives_final, "Descriptives High Income Final.xlsx")

#Describing Data

#Third pillar
num_countries_over_10 <- sum(descriptives_mediation$Third_pillar_size > 10)
num_countries_over_10

#Polarization
num_countries_over_75 <- sum(descriptives_mediation$Poli_polarization_vdem > 75)
num_countries_over_75

#Trust
num_countries_over_50 <- sum(descriptives_mediation$Trust_ivs > 50)
num_countries_over_50
countries_over_50 <- descriptives_mediation$Country[descriptives_mediation$Trust_ivs > 50]
countries_over_50

#Inequality
num_countries_over_34.61 <- sum(descriptives_mediation$Gini_index_wb > 34.61)
num_countries_over_34.61

#Facebook Users
num_countries_over_75000 <- sum(descriptives_mediation$Facebook_users_wv > 75000)
num_countries_over_75000
countries_over_75000 <- descriptives_mediation$Country[descriptives_mediation$Facebook_users_wv > 75000]
countries_over_75000

#Loneliness
num_countries_over_24.94 <- sum(descriptives_mediation$Loneliness_owid > 24.94)
num_countries_over_24.94

#Descriptives for final selection of variables converted to z-scores
z_score <- function(x) {
  if (all(is.na(x))) {
    return(x)
  } else {
    return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  }
}
mediation_variables <- setdiff(names(mediation), c("Country"))
mediation_zscore <- mediation %>%
  mutate(across(all_of(mediation_variables), ~ z_score(.x)))

summary(mediation_zscore)
sd(mediation_zscore$Third_pillar_size)
sd(mediation_zscore$Poli_polarization_vdem)
sd(mediation_zscore$Facebook_users_wv)
sd(mediation_zscore$Gini_index_wb)
sd(mediation_zscore$Loneliness_owid)
sd(mediation_zscore$Trust_ivs)
descriptives_final_zscore <- psych::describe(mediation_zscore) #descriptive statistics
descriptives_final_zscore$Variable <- rownames(descriptives_final_zscore)
write_xlsx(descriptives_final_zscore, "Z-score Descriptives High Income Final.xlsx")

#Boxplots
library(reshape2)

mediation_long <- melt(mediation_zscore, id.vars = "Country")
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
mediation_long$outlier <- ave(mediation_long$value, mediation_long$variable, FUN = is_outlier)

ggplot(mediation_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  geom_text(aes(label = ifelse(outlier, Country, '')),
            hjust = -0.3, vjust = 0.3, size = 3, color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Variable", y = "Value", title = "Boxplots of Mediation Variables with Outliers Labeled")


# Correlation between inequality and belief in inequality # 30 countries
gini_belief <- mediation_upper %>%
  dplyr::select(Country,Gini_index_wb, Income_equality_wvs)
gini_belief <- na.omit(gini_belief)
correlation_inequality <- cor(gini_belief$Gini_index_wb, gini_belief$Income_equality_wvs, method = "pearson")

# Correlation between trust in others and trust in government # 39 countries
trusts <- mediation_upper %>%
  dplyr::select(Country,Trust_ivs, Trust_in_govt_oecd)
trusts <- na.omit(trusts)
correlation_trust <- cor(trusts$Trust_ivs, trusts$Trust_in_govt_oecd, method = "pearson")


# Four mediation analyses 

#Trust in others
trust <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Trust_ivs), data = mediation_zscore)
trust
print(trust, digits = 4, short = FALSE)

#Trust in others with GDP
trust_gdp <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Trust_ivs) + (GDP_Most_Recent), data = mediation_zscore)
trust_gdp
print(trust_gdp, digits = 4, short = FALSE)

# Gini indix (income inequality)
gini <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Gini_index_wb), data = mediation_zscore)
gini
print(gini, digits = 4, short = FALSE)

# Gini indix (income inequality) with GDP
gini_gdp <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Gini_index_wb) + (GDP_Most_Recent), data = mediation_zscore)
gini_gdp
print(gini_gdp, digits = 4, short = FALSE)

# Facebook users
facebookusers <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Facebook_users_wv), data = mediation_zscore)
facebookusers
print(facebookusers, digits = 4, short = FALSE)

# Facebook users with GDP
facebookusers_gdp <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Facebook_users_wv) + (GDP_Most_Recent), data = mediation_zscore)
facebookusers_gdp
print(facebookusers_gdp, digits = 4, short = FALSE)

# Loneliness
loneliness <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Loneliness_owid), data = mediation_zscore)
loneliness
print(loneliness, digits = 4, short = FALSE)

# Loneliness with GDP
lonelieness_gdp <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Loneliness_owid) + (GDP_Most_Recent), data = mediation_zscore)
lonelieness_gdp
print(lonelieness_gdp, digits = 4, short = FALSE)

# Loneliness #Check with GDP?
loneliness_data <- mediation_zscore[, c("Country", "Third_pillar_size", "Poli_polarization_vdem", "Facebook_users_wv", "Gini_index_wb", "Loneliness_owid", "Trust_ivs", "GDP_Most_Recent")]
loneliness_data <- na.omit(loneliness_data)
lonelieness <- mediate(Loneliness_owid ~ Third_pillar_size + (GDP_Most_Recent), data = loneliness_data)
lonelieness
print(lonelieness, digits = 4, short = FALSE)

#All together
all_together <- mediate(Poli_polarization_vdem ~ Third_pillar_size +
                          (Trust_ivs) + (Gini_index_wb) +
                          (Facebook_users_wv) + (Loneliness_owid) +
                          (GDP_Most_Recent), data = mediation_zscore)
all_together
print(all_together, digits = 4, short = FALSE)

#######################################################################################
#######################################################################################
# Re-run analysis with only 51 country data
#######################################################################################
#######################################################################################
library(xlsx)
tpt <- read.xlsx("All variables_thirdpillar.xlsx", sheetIndex = 1)

countries_with_data <- which(tpt$Actual != "")
countries_list <- tpt$Country[countries_with_data]

library(dplyr)
mediation_all <- mediation_all %>%
  filter(Country %in% countries_list)

mediation_upper <- mediation_all %>%
  filter(World_bank_income_group %in% c("Upper-middle income", "High income"))
write.xlsx(mediation_upper, "Clean Data Upper Income Groups_51 countries.xlsx")

mediation_lower <- mediation_all %>%
  filter(World_bank_income_group %in% c("Low income", "Lower-middle income"))
write.xlsx(mediation_lower, "Clean Data Lower Income Groups_51 countries.xlsx")


# Correlational Analysis

# Polarization and third pillar - simple linear models

## All incomes - reported in paper as first analysis
polarization_all <- mediation_all[, c("Country", "Third_pillar_size", "Poli_polarization_vdem")]
polarization_all <- na.omit(polarization_all)
summary(polarization_all)
sd(polarization_all$Third_pillar_size)
sd(polarization_all$Poli_polarization_vdem)

polarization_all_lm <- lm(Poli_polarization_vdem ~ Third_pillar_size, data = polarization_all)
summary(polarization_all_lm)

# Mediation Analysis 

# UPPER-MIDDLE AND HIGH-INCOME GROUPS 
#######################################

mediation <- mediation_upper[, c("Country", "Third_pillar_size", "Poli_polarization_vdem", "Trust_ivs","Gini_index_wb", "Facebook_users_wv","Loneliness_owid", "GDP_Most_Recent")]
mediation <- na.omit(mediation)

#Correlation matrix for final selection of variables 
correlation_mediation <- mediation_upper[, c("Country", "Third_pillar_size", "Poli_polarization_vdem", "Trust_ivs","Gini_index_wb", "Facebook_users_wv","Loneliness_owid", "GDP_Most_Recent")]
correlation_mediation <- na.omit(correlation_mediation)
lowerCor(correlation_mediation)
correlation_final <- as.data.frame(lowerCor(correlation_mediation))
write_xlsx(correlation_final, "Correlation Matrix Final_51 countries.xlsx")

#Descriptives for final selection of variables 
descriptives_mediation <- mediation_upper[, c("Country", "Third_pillar_size", "Poli_polarization_vdem", "Trust_ivs","Gini_index_wb", "Facebook_users_wv","Loneliness_owid", "GDP_Most_Recent")]
descriptives_mediation <- na.omit(descriptives_mediation)
summary(descriptives_mediation)
sd(descriptives_mediation$Third_pillar_size)
sd(descriptives_mediation$Poli_polarization_vdem)
sd(descriptives_mediation$Facebook_users_wv)
sd(descriptives_mediation$Gini_index_wb)
sd(descriptives_mediation$Loneliness_owid)
sd(descriptives_mediation$Trust_ivs)
descriptives_final <- psych::describe(descriptives_mediation) #descriptive statistics
descriptives_final$Variable <- rownames(descriptives_final)
write_xlsx(descriptives_final, "Descriptives High Income Final_51 countries.xlsx")

#Describing Data

#Third pillar
num_countries_over_10 <- sum(descriptives_mediation$Third_pillar_size > 10)
num_countries_over_10

#Polarization
num_countries_over_75 <- sum(descriptives_mediation$Poli_polarization_vdem > 75)
num_countries_over_75

#Trust
num_countries_over_50 <- sum(descriptives_mediation$Trust_ivs > 50)
num_countries_over_50
countries_over_50 <- descriptives_mediation$Country[descriptives_mediation$Trust_ivs > 50]
countries_over_50

#Inequality
num_countries_over_34.61 <- sum(descriptives_mediation$Gini_index_wb > 34.61)
num_countries_over_34.61

#Facebook Users
num_countries_over_75000 <- sum(descriptives_mediation$Facebook_users_wv > 75000)
num_countries_over_75000
countries_over_75000 <- descriptives_mediation$Country[descriptives_mediation$Facebook_users_wv > 75000]
countries_over_75000

#Loneliness
num_countries_over_24.94 <- sum(descriptives_mediation$Loneliness_owid > 24.94)
num_countries_over_24.94

#Descriptives for final selection of variables converted to z-scores
z_score <- function(x) {
  if (all(is.na(x))) {
    return(x)
  } else {
    return((x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE))
  }
}
mediation_variables <- setdiff(names(mediation), c("Country"))
mediation_zscore <- mediation %>%
  mutate(across(all_of(mediation_variables), ~ z_score(.x)))

summary(mediation_zscore)
sd(mediation_zscore$Third_pillar_size)
sd(mediation_zscore$Poli_polarization_vdem)
sd(mediation_zscore$Facebook_users_wv)
sd(mediation_zscore$Gini_index_wb)
sd(mediation_zscore$Loneliness_owid)
sd(mediation_zscore$Trust_ivs)
descriptives_final_zscore <- psych::describe(mediation_zscore) #descriptive statistics
descriptives_final_zscore$Variable <- rownames(descriptives_final_zscore)
write_xlsx(descriptives_final_zscore, "Z-score Descriptives High Income Final_51 countries.xlsx")

#Boxplots
library(reshape2)

mediation_long <- melt(mediation_zscore, id.vars = "Country")
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
mediation_long$outlier <- ave(mediation_long$value, mediation_long$variable, FUN = is_outlier)

ggplot(mediation_long, aes(x = variable, y = value)) +
  geom_boxplot() +
  geom_text(aes(label = ifelse(outlier, Country, '')),
            hjust = -0.3, vjust = 0.3, size = 3, color = "red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "Variable", y = "Value", title = "Boxplots of Mediation Variables with Outliers Labeled")


# Correlation between inequality and belief in inequality # 30 countries
gini_belief <- mediation_upper %>%
  dplyr::select(Country,Gini_index_wb, Income_equality_wvs)
gini_belief <- na.omit(gini_belief)
correlation_inequality <- cor(gini_belief$Gini_index_wb, gini_belief$Income_equality_wvs, method = "pearson")

# Correlation between trust in others and trust in government # 39 countries
trusts <- mediation_upper %>%
  dplyr::select(Country,Trust_ivs, Trust_in_govt_oecd)
trusts <- na.omit(trusts)
correlation_trust <- cor(trusts$Trust_ivs, trusts$Trust_in_govt_oecd, method = "pearson")


# Four mediation analyses 

#Trust in others
trust <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Trust_ivs), data = mediation_zscore)
trust
print(trust, digits = 4, short = FALSE)

#Trust in others with GDP
trust_gdp <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Trust_ivs) + (GDP_Most_Recent), data = mediation_zscore)
trust_gdp
print(trust_gdp, digits = 4, short = FALSE)

# Gini indix (income inequality)
gini <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Gini_index_wb), data = mediation_zscore)
gini
print(gini, digits = 4, short = FALSE)

# Gini indix (income inequality) with GDP
gini_gdp <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Gini_index_wb) + (GDP_Most_Recent), data = mediation_zscore)
gini_gdp
print(gini_gdp, digits = 4, short = FALSE)

# Facebook users
facebookusers <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Facebook_users_wv), data = mediation_zscore)
facebookusers
print(facebookusers, digits = 4, short = FALSE)

# Facebook users with GDP
facebookusers_gdp <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Facebook_users_wv) + (GDP_Most_Recent), data = mediation_zscore)
facebookusers_gdp
print(facebookusers_gdp, digits = 4, short = FALSE)

# Loneliness
loneliness <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Loneliness_owid), data = mediation_zscore)
loneliness
print(loneliness, digits = 4, short = FALSE)

# Loneliness with GDP
lonelieness_gdp <- mediate(Poli_polarization_vdem ~ Third_pillar_size + (Loneliness_owid) + (GDP_Most_Recent), data = mediation_zscore)
lonelieness_gdp
print(lonelieness_gdp, digits = 4, short = FALSE)

# Loneliness #Check with GDP?
loneliness_data <- mediation_zscore[, c("Country", "Third_pillar_size", "Poli_polarization_vdem", "Facebook_users_wv", "Gini_index_wb", "Loneliness_owid", "Trust_ivs", "GDP_Most_Recent")]
loneliness_data <- na.omit(loneliness_data)
lonelieness <- mediate(Loneliness_owid ~ Third_pillar_size + (GDP_Most_Recent), data = loneliness_data)
lonelieness
print(lonelieness, digits = 4, short = FALSE)

#All together
all_together <- mediate(Poli_polarization_vdem ~ Third_pillar_size +
                          (Trust_ivs) + (Gini_index_wb) +
                          (Facebook_users_wv) + (Loneliness_owid) +
                          (GDP_Most_Recent), data = mediation_zscore)
all_together
print(all_together, digits = 4, short = FALSE)

