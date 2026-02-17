# 02_functions.R

# -----------------------------------
# WVS ALPHA-3 → COUNTRY NAME MAPPING
# -----------------------------------
wvs_country_lookup <- c(
  ALB = "Albania", LBN = "Lebanon", AND = "Andorra", LBY = "Libya",
  ARG = "Argentina", LTU = "Lithuania", ARM = "Armenia", MAC = "Macao",
  AUS = "Australia", MYS = "Malaysia", AUT = "Austria", MDV = "Maldives",
  AZE = "Azerbaijan", MEX = "Mexico", BGD = "Bangladesh", MAR = "Morocco",
  BLR = "Belarus", MNG = "Mongolia", BOL = "Bolivia", MNE = "Montenegro",
  BIH = "Bosnia Herzegovina", MMR = "Myanmar", BRA = "Brazil", NLD = "Netherlands",
  BGR = "Bulgaria", NZL = "New Zealand", CAN = "Canada", NIC = "Nicaragua",
  CHL = "Chile", NGA = "Nigeria", CHN = "China", MKD = "North Macedonia",
  COL = "Colombia", NIR = "Northern Ireland", HRV = "Croatia", NOR = "Norway",
  CYP = "Cyprus", PAK = "Pakistan", CZE = "Czechia", PER = "Peru",
  DNK = "Denmark", PHL = "Philippines", ECU = "Ecuador", POL = "Poland",
  EGY = "Egypt", PRT = "Portugal", EST = "Estonia", PRI = "Puerto Rico",
  ETH = "Ethiopia", ROU = "Romania", FIN = "Finland", RUS = "Russian Federation",
  FRA = "France", SRB = "Serbia", GEO = "Georgia", SGP = "Singapore",
  DEU = "Germany", SVK = "Slovakia", GBR = "Great Britain", SVN = "Slovenia",
  GRC = "Greece", KOR = "South Korea", GTM = "Guatemala", ESP = "Spain",
  HKG = "Hong Kong SAR PRC", SWE = "Sweden", HUN = "Hungary", CHE = "Switzerland",
  ISL = "Iceland", TWN = "Taiwan ROC", IDN = "Indonesia", TJK = "Tajikistan",
  IRN = "Iran", THA = "Thailand", IRQ = "Iraq", TUN = "Tunisia",
  ITA = "Italy", TUR = "Turkey", JPN = "Japan", UKR = "Ukraine",
  JOR = "Jordan", USA = "United States", KAZ = "Kazakhstan", URY = "Uruguay",
  KEN = "Kenya", VNM = "Vietnam", KGZ = "Kyrgyzstan", ZWE = "Zimbabwe",
  LVA = "Latvia", VEN = "Venezuela"
)

# -----------------------------------
# ESS ALPHA-2 → COUNTRY NAME MAPPING
# -----------------------------------
ess_country_lookup <- c(
  AL = "Albania", AT = "Austria", BE = "Belgium", BG = "Bulgaria",
  CH = "Switzerland", CY = "Cyprus", CZ = "Czechia", DE = "Germany",
  DK = "Denmark", EE = "Estonia", ES = "Spain", FI = "Finland",
  FR = "France", GB = "United Kingdom", GE = "Georgia", GR = "Greece",
  HR = "Croatia", HU = "Hungary", IE = "Ireland", IS = "Iceland",
  IL = "Israel", IT = "Italy", LT = "Lithuania", LU = "Luxembourg",
  LV = "Latvia", ME = "Montenegro", MK = "Macedonia", NL = "Netherlands",
  NO = "Norway", PL = "Poland", PT = "Portugal", RO = "Romania",
  RS = "Serbia", RU = "Russia", SE = "Sweden", SI = "Slovenia",
  SK = "Slovak Republic", TR = "Turkey", UA = "Ukraine", XK = "Kosovo"
)

# Functions

#' Reads the excel file with manual country name corrections. This file should be located in the data_raw 
#' folder of the project and must contain at least 2 cols:
#' original (incorrect name)
#' corrected (desired name)
#' @return A data frame with country name corrections.
load_country_corrections <- function() {
  # Read correction table from Excel file using a path built with here::here()
  corr <- read.xlsx(here::here("data_raw", "country_corrections.xlsx"))
  corr
}


#' Replaces country names in a vector using a lookup table with
#' `original` and `corrected` values.
#'
#' @param country_vec Character vector of country names.
#' @param corrections Data frame with columns `original` and `corrected`.
#'
#' @return A character vector with standardized country names.
#'
standardize_country <- function(country_vec, corrections) {
  out <- country_vec
  for (i in seq_len(nrow(corrections))) {
    out[out == corrections$original[i]] <- corrections$corrected[i]
  }
  trimws(out)
}

z_score <- function(x) {
  (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
}


#' Converts WVS ISO3 country codes into full country names using the
#' predefined `wvs_country_lookup`. Unknown codes are returned unchanged.
#'
#' @param code_vec Character vector of ISO3 country codes.
#'
#' @return A character vector of country names.
#'
map_wvs_country <- function(code_vec) {
  out <- wvs_country_lookup[code_vec]
  out[is.na(out)] <- code_vec[is.na(out)]
  return(out)
}


#' Converts ESS ISO2 country codes into full country names using the
#' predefined `ess_country_lookup`. Unknown codes are returned unchanged.
#'
#' @param code_vec Character vector of ISO2 country codes.
#'
#' @return A character vector of country names.
map_ess_country <- function(code_vec) {
  out <- ess_country_lookup[code_vec]
  out[is.na(out)] <- code_vec[is.na(out)]
  return(out)
}


# Other WVS processing helper functions

#' Filters WVS data to Wave 7, renames the country variable, and keeps
#' only the variables needed for downstream processing.
#'
#' @param df A WVS data frame.
#'
#' @return A data frame containing selected Wave 7 variables.
wvs_select_vars <- function(df) {
  df %>%
    dplyr::filter(A_WAVE == 7) %>%
    dplyr::rename(Country = B_COUNTRY_ALPHA) %>%
    dplyr::select(
      A_YEAR, A_WAVE, Country, 
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
      
    )
}


#' Recode and clean WVS variables
#'
#' Renames selected WVS variables and applies validity checks, converting
#' out-of-range values to `NA`. This ensures consistent coding across all
#' variables before aggregation.
#'
#' @param df A data frame containing raw WVS variables.
#'
#' @return A data frame with renamed and recoded variables.
wvs_recode_values <- function(df) {
  df %>%
    dplyr::rename(
      Family_importance = Q1,
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
      Online_actions = Q217
    ) %>%
    dplyr::mutate(
      Tolerance_respect_others = dplyr::if_else(Tolerance_respect_others %in% c(1,2), Tolerance_respect_others, NA_integer_),
      Feeling_happiness = dplyr::if_else(Feeling_happiness %in% 1:4, Feeling_happiness, NA_integer_),
      Satisfaction_life = dplyr::if_else(Satisfaction_life %in% 1:10, Satisfaction_life, NA_integer_),
      Trust_people = dplyr::if_else(Trust_people %in% c(1,2), Trust_people, NA_integer_),
      Confidence_government = dplyr::if_else(Confidence_government %in% 1:4, Confidence_government, NA_integer_),
      Income_equality = dplyr::if_else(Income_equality %in% 1:10, Income_equality, NA_integer_),
      Family_importance = dplyr::if_else(Family_importance %in% 1:4, Family_importance, NA_integer_),
      Friends_importance = dplyr::if_else(Friends_importance %in% 1:4, Friends_importance, NA_integer_),
      Politics_importance = dplyr::if_else(Politics_importance %in% 1:4, Politics_importance, NA_integer_),
      Religion_importance = dplyr::if_else(Religion_importance %in% 1:4, Religion_importance, NA_integer_),
      Confidence_political_parties = dplyr::if_else(Confidence_political_parties %in% 1:4, Confidence_political_parties, NA_integer_),
      Confidence_parliament = dplyr::if_else(Confidence_parliament %in% 1:4, Confidence_parliament, NA_integer_),
      Confidence_elections = dplyr::if_else(Confidence_elections %in% 1:4, Confidence_elections, NA_integer_),
      Active_member = dplyr::if_else(Active_member %in% 0:2, Active_member, NA_integer_),
      Member_political_parties = dplyr::if_else(Member_political_party %in% c(0,1), Member_political_party, NA_integer_),
      Info_social_media = dplyr::if_else(Info_social_media %in% 1:5, Info_social_media, NA_integer_),
      Online_actions = dplyr::if_else(Online_actions %in% 1:3, Online_actions, NA_integer_)
    )
}


#' Aggregate WVS variables to country-year means
#'
#' Computes country-year averages for all recoded WVS variables 
#' included in the processed dataset.
#'
#' @param df A data frame with recoded WVS individual-level data.
#'
#' @return A data frame containing mean values per country and year.
wvs_aggregate_means <- function(df) {
  df %>%
    dplyr::group_by(A_YEAR, A_WAVE, Country) %>%
    dplyr::summarise(
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
      Online_actions = mean(Online_actions, na.rm = TRUE),
      .groups="drop"
    )
}


#' Compute tolerance percentage (WVS)
#'
#' Calculates the share of respondents who selected the most tolerant
#' response (value 1) among all valid tolerance responses (1 or 2).
#'
#' @param df A WVS data frame with the variable `Tolerance_respect_others`.
#'
#' @return A data frame with percentage tolerance per country-year.
wvs_tolerance <- function(df) {
  df %>%
    dplyr::group_by(A_YEAR, Country) %>%
    dplyr::summarise(
      total_ones = sum(Tolerance_respect_others == 1, na.rm = TRUE),
      total_ones_and_twos = sum(Tolerance_respect_others %in% c(1,2), na.rm = TRUE),
      Percentage_tolerance = (total_ones / total_ones_and_twos) * 100,
      .groups="drop"
    )
}


#' Compute trust percentage (WVS)
#'
#' Calculates the proportion of respondents who report the highest level
#' of interpersonal trust (value 1) among all valid trust responses (1 or 2).
#'
#' @param df A WVS data frame containing the variable `Trust_people`.
#'
#' @return A data frame with percentage trust per country-year.
wvs_trust <- function(df) {
  df %>%
    dplyr::group_by(A_YEAR, Country) %>%
    dplyr::summarise(
      total_ones = sum(Trust_people == 1, na.rm = TRUE),
      total_ones_and_twos = sum(Trust_people %in% c(1,2), na.rm = TRUE),
      Percentage_trust = (total_ones / total_ones_and_twos) * 100,
      .groups="drop"
    )
}


#' Compute political party membership percentages (WVS)
#'
#' Calculates the share of active members (value 2) and total members
#' (values 1 or 2) relative to all valid responses for political party
#' membership.
#'
#' @param df A WVS data frame containing the variable `Active_member`.
#'
#' @return A data frame with membership percentages per country-year.
wvs_active_member <- function(df) {
  df %>%
    dplyr::group_by(A_YEAR, Country) %>%
    dplyr::summarise(
      total_active = sum(Active_member == 2, na.rm = TRUE),
      total_member = sum(Active_member %in% c(1,2), na.rm = TRUE),
      total_all = sum(Active_member %in% c(0,1,2), na.rm = TRUE),
      Percentage_active_members = (total_active / total_all) * 100,
      Percentage_members = (total_member / total_all) * 100,
      .groups="drop"
    )
}


#' Compute political party membership percentage (WVS)
#'
#' Calculates the share of respondents who report being members of a
#' political party (value 1) among all valid responses (0 or 1).
#'
#' @param df A WVS data frame containing `Member_political_party`.
#'
#' @return A data frame with political party membership percentages per country-year.
wvs_member_political_party <- function(df) {
  df %>%
    dplyr::group_by(A_YEAR, Country) %>%
    dplyr::summarise(
      total_ones = sum(Member_political_party == 1, na.rm = TRUE),
      total_zeroes_and_ones = sum(Member_political_party %in% c(0,1), na.rm = TRUE),
      Percentage_poli_members = (total_ones / total_zeroes_and_ones) * 100,
      .groups="drop"
    )
}


#' Combine all WVS aggregate datasets
#'
#' Merges mean values, tolerance, trust, active membership, and party 
#' membership aggregates into a single country-year dataset.
#'
#' @param wvs_mean Data frame of mean WVS variables.
#' @param tol Data frame with tolerance percentages.
#' @param trust Data frame with trust percentages.
#' @param active Data frame with active/total membership percentages.
#' @param party Data frame with political party membership percentages.
#'
#' @return A merged data frame containing all WVS aggregates.
wvs_combine_aggregates <- function(wvs_mean, tol, trust, active, party) {
  wvs_mean %>%
    dplyr::left_join(tol,    by = c("Country", "A_YEAR")) %>%
    dplyr::left_join(trust,  by = c("Country", "A_YEAR")) %>%
    dplyr::left_join(active, by = c("Country", "A_YEAR")) %>%
    dplyr::left_join(party,  by = c("Country", "A_YEAR"))
}


#' Combine Great Britain and Northern Ireland into UK
#'
#' Replaces separate Great Britain and Northern Ireland rows with a single
#' "United Kingdom of Great Britain and Northern Ireland" row, averaging
#' matching variables.
#'
#' @param df A data frame with a `Country` column and the listed WVS aggregates.
#'
#' @return A data frame with GB/NI replaced by a UK row (if both exist).
#'
wvs_combine_uk <- function(df) {
  gb <- df %>% dplyr::filter(Country == "Great Britain")
  ni <- df %>% dplyr::filter(Country == "Northern Ireland")
  
  if (nrow(gb) == 0 || nrow(ni) == 0) return(df)
  
  uk <- dplyr::mutate(
    gb,
    Country = "United Kingdom of Great Britain and Northern Ireland",
    Feeling_happiness            = (gb$Feeling_happiness + ni$Feeling_happiness) / 2,
    Satisfaction_life            = (gb$Satisfaction_life + ni$Satisfaction_life) / 2,
    Confidence_government        = (gb$Confidence_government + ni$Confidence_government) / 2,
    Income_equality              = (gb$Income_equality + ni$Income_equality) / 2,
    Percentage_tolerance         = (gb$Percentage_tolerance + ni$Percentage_tolerance) / 2,
    Percentage_trust             = (gb$Percentage_trust + ni$Percentage_trust) / 2,
    Family_importance            = (gb$Family_importance + ni$Family_importance) / 2,
    Friends_importance           = (gb$Friends_importance + ni$Friends_importance) / 2,
    Politics_importance          = (gb$Politics_importance + ni$Politics_importance) / 2,
    Religion_importance          = (gb$Religion_importance + ni$Religion_importance) / 2,
    Confidence_political_parties = (gb$Confidence_political_parties + ni$Confidence_political_parties) / 2,
    Confidence_parliament        = (gb$Confidence_parliament + ni$Confidence_parliament) / 2,
    Confidence_elections         = (gb$Confidence_elections + ni$Confidence_elections) / 2,
    Info_social_media            = (gb$Info_social_media + ni$Info_social_media) / 2,
    Percentage_active_members    = (gb$Percentage_active_members + ni$Percentage_active_members) / 2,
    Percentage_members           = (gb$Percentage_members + ni$Percentage_members) / 2,
    Percentage_poli_members      = (gb$Percentage_poli_members + ni$Percentage_poli_members) / 2
  )
  
  df %>%
    dplyr::filter(!Country %in% c("Great Britain", "Northern Ireland")) %>%
    dplyr::bind_rows(uk)
}


# OWID processing functions


#' Remove non-country OWID regions
#'
#' Filters out aggregate OWID entities (e.g., world regions, income groups)
#' based on common keywords in the `Country` column.
#'
#' @param df A data frame containing a `Country` column.
#'
#' @return A data frame with region-level OWID entries removed.
drop_owid_regions <- function(df) {
  df %>%
    dplyr::filter(
      !grepl(
        "income|world|countries|union|europe|asia|caribbean|oceania|eastern|western",
        Country, ignore.case = TRUE
      )
    )
}


#' Add a suffix to variable names
#'
#' Appends a suffix to all columns except `Country`, useful when merging
#' datasets with overlapping variable names.
#'
#' @param df A data frame.
#' @param suffix A character string to append to variable names.
#'
#' @return The data frame with suffixed variable names.
suffix_vars <- function(df, suffix) {
  df %>%
    dplyr::rename_with(
      ~ ifelse(.x == "Country", .x, paste0(.x, "_", suffix))
    )
}



#' Plot third sector size vs. political polarization
#'
#' Creates a scatter plot with linear fit and labels, annotating the Pearson correlation.
#'
#' @param df A data frame with `Third_pillar_size`, `Poli_polarization`, and `Country`.
#' @param title Plot title.
#' @param outfile File path for the saved PNG.
#'
#' @return (Invisibly) the ggplot object.
plot_thirdpillar_vs_polarization <- function(df, title, outfile) {
  
  # Compute correlation
  r <- cor(df$Third_pillar_size, df$Poli_polarization, use = "complete.obs")
  
  # Build the plot (no aesthetic warnings)
  p <- ggplot2::ggplot(df, aes(x = Third_pillar_size, y = Poli_polarization)) +
    geom_point(shape = 20) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.6) +
    
    # Label only in this layer (fixes warning)
    ggrepel::geom_text_repel(
      aes(label = Country),
      size = 2.5,
      max.overlaps = 6,
      family = "Times New Roman"
    ) +
    
    # CORRELATION annotation
    annotate(
      "text",
      x = 18, y = max(df$Poli_polarization, na.rm = TRUE) * 0.05,
      label = paste0("Correlation: ", sprintf("%.2f", r)),
      family = "Times New Roman",
      size = 4
    ) +
    
    # FIXED AXES to supervisor style
    scale_x_continuous(limits = c(0, 20), breaks = seq(0, 20, 5)) +
    scale_y_continuous(limits = c(0, 100)) +
    
    # TITLES CENTERED
    ggplot2::labs(
      title = title,
      x = "Third Sector Size (%)",
      y = "Political Polarization (%)"
    ) +
    
    # CLEAN STYLE THEME
    theme_bw() +
    theme(
      panel.border   = element_blank(),      # remove dark border
      plot.title     = element_text(hjust = 0.5, family = "Times New Roman"),
      text           = element_text(family = "Times New Roman"),
      axis.title     = element_text(size = 11),
      axis.text      = element_text(size = 10)
    )
  
  ggplot2::ggsave(outfile, plot = p, device = "png", width = 7.5, height = 5.2, dpi = 300)
  
  return(p)
}


#' Plot a variable against political polarization
#'
#' Creates a scatter plot with linear fit and labels, annotating the Pearson correlation
#' between `xvar` and `Poli_polarization`.
#'
#' @param df A data frame containing `Poli_polarization`, `Country`, and the column named in `xvar`.
#' @param xvar String; column name in `df` to plot on the x-axis.
#' @param xlab X-axis label.
#' @param title Plot title.
#' @param outfile File path for the saved PNG.
#' @param x_limits Optional numeric vector of length 2 for x-axis limits.
#'
#' @return (Invisibly) the ggplot object.
plot_variable_vs_polarization <- function(df, xvar, xlab, title, outfile,
                                          x_limits = NULL) {
  
  # Extract vectors
  x <- df[[xvar]]
  y <- df$Poli_polarization
  
  # Compute correlation
  r <- cor(x, y, use = "complete.obs")
  
  # Plot
  p <- ggplot2::ggplot(df, aes(x = .data[[xvar]], y = Poli_polarization)) +
    geom_point(shape = 20) +
    geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.6) +
    
    # Labels (no warnings)
    ggrepel::geom_text_repel(
      aes(label = Country),
      size = 2.5,
      max.overlaps = 6,
      family = "Times New Roman"
    ) +
    
    # CORRELATION annotation
    annotate(
      "text",
      x = if (!is.null(x_limits)) x_limits[2] * 0.9 else max(x, na.rm = TRUE) * 0.9,
      y = 5,
      label = paste0("Correlation: ", sprintf("%.2f", r)),
      family = "Times New Roman",
      size = 4
    ) +
    
    # X axis limits (optional)
    {if (!is.null(x_limits)) scale_x_continuous(limits = x_limits) else ggplot2::scale_x_continuous()} +
    
    # Y axis (supervisor style)
    scale_y_continuous(limits = c(0, 100)) +
    
    # Titles centered
    ggplot2::labs(
      title = title,
      x = xlab,
      y = "Political Polarization (%)"
    ) +
    
    # Clean style
    theme_bw() +
    theme(
      panel.border = element_blank(),
      plot.title = element_text(hjust = 0.5, family = "Times New Roman"),
      text = element_text(family = "Times New Roman")
    )
  
  ggplot2::ggsave(outfile, plot = p, device = "png",
                  width = 7.5, height = 5.2, dpi = 300)
  
  return(p)
}

