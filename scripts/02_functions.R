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

only_numeric <- function(df) {
  df[, sapply(df, is.numeric), drop = FALSE]
}

save_corr_matrix <- function(df, outfile) {
  # Remove NAs
  df_clean <- stats::na.omit(df)
  
  # Keep only numeric variables
  num_df <- only_numeric(df_clean)
  
  # Only compute correlation if at least 2 numeric vars
  if (ncol(num_df) >= 2) {
    cor_m <- psych::corr.test(
      num_df,
      use   = "pairwise",
      method = "pearson"
    )
    
    writexl::write_xlsx(
      list(
        "cor_matrix" = as.data.frame(cor_m$r),
        "p_values"   = as.data.frame(cor_m$p)
      ),
      outfile
    )
    
    message("✓ Saved correlation matrix to: ", outfile)
  } else {
    warning("Not enough numeric columns to compute correlations.")
  }
}

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
      Q57, #Most people can be trusted, 1 = most people can be trusted, 2 = need to be very careful 
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
      Trust_people = Q57,
    ) %>%
    dplyr::mutate(
      Trust_people = dplyr::if_else(Trust_people %in% c(1,2), Trust_people, NA_integer_),
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
      Trust_people = mean(Trust_people, na.rm = TRUE),
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

wvs_combine_aggregates <- function(wvs_mean, trust) {
  wvs_mean %>%
    dplyr::left_join(trust,  by = c("Country", "A_YEAR"))
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
    Percentage_trust             = (gb$Percentage_trust + ni$Percentage_trust) / 2,
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

