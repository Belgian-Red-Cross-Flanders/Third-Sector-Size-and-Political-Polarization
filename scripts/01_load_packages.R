# 01_load_packages.R

required_pkgs <- c(
  "dplyr", "tidyr", "ggplot2", "ggrepel", "readxl", "openxlsx",
  "psych", "mediation", "car", "lmtest", "here", "haven", "purrr", 
  "reshape2", "broom", "writexl"
)
#TODO: versions

# Install missing
to_install <- required_pkgs[!required_pkgs %in% installed.packages()]
if (length(to_install) > 0) install.packages(to_install)

# Load
lapply(required_pkgs, library, character.only = TRUE)
