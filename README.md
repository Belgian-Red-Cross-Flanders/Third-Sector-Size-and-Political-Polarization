Third Pillar & Social Outcomes — Cross-Country Analysis
================
February 19th, 2026

## Quantifying the relationship between third sector size and political polarization

This repository contains the data preparation, data descriptives,
regressions, and mediation analyses for the paper on the relationship
between **Third sector size** and **political polarization** across
countries.

The workflow is **reproducible**, uses a **Third Pillar dataset as the
anchor**, and stores analysis‑ready subsets (RDS) so you can run
regressions/mediation without regenerating data.

------------------------------------------------------------------------

## 🧭 Repository Structure

``` plaintext
.
├─ scripts/
│  ├─ 01_load_packages.R
│  ├─ 02_functions.R
│  ├─ 04_clean_merge.R
│  ├─ 05_descriptives.R
│  ├─ 06_regressions.R
│  └─ 07_mediation_psych.R              # (psych::mediate)
│
├─ data_raw/                            # Datasets needed to regenerate all results (raw data)
│  ├─ All variables_thirdpillar.xlsx
│  ├─ country_corrections.xlsx
│  └─ Political Polarization/
│     ├─ API_NY.GDP.MKTP.KD_DS2_en_csv_v2_279473.csv
│     ├─ API_SI.POV.GINI_DS2_en_excel_v2_39.xlsx
│     ├─ API_SP.POP.TOTL_DS2_en_csv_v2_277159.csv
│     ├─ facebook-users-by-country-2026.csv
│     ├─ one-person-households.csv
│     ├─ self-reported-trust-attitudes.csv
│     └─ WVS_Cross-National_Wave_7_rData_v5_0.rdata -- WARNING: This file is too heavy to upload to GitHub. Please download it from https://www.worldvaluessurvey.org/WVSDocumentationWV7.jsp and save it to this folder.
│  └─ Trust in Government/
│     └─ OECD Trust in Government.xlsx
│  └─ V-Dem-CY-FullOthers_csv_v13/
│     └─ V-Dem-CY-Full+Others-v13.csv -- WARNING: This file is too heavy to upload to GitHub. Please download it from https://www.v-dem.net/data/the-v-dem-dataset/ and save it to this folder.
|
├─ data_clean/
│  ├─ master_dataset.rds
│  ├─ master_dataset.xlsx
│  ├─ raw_data.rds                      # (Single serialized source of all raw inputs)
│  └─ analysis_subsets/
│     ├─ polarization_all.rds
│     ├─ pol_upper.rds
│     ├─ pol_lower.rds
│     ├─ polarization_gdp.rds
│     ├─ mediation_dataset.xlsx
│     ├─ mediation_dataset.rds
│     └─ mediation_dataset_z.rds
│
├─ outputs/
│  ├─ descriptives/                     # missingness, describe(), correlations, etc.
│  ├─ figures/                          # all saved ggplot figures
│  ├─ regressions/                      # tidy/glance/augment, diagnostics, txt summaries
│  └─ mediation_psych/                  # psych::mediate outputs
│
└─ README.md
```

> Paths are **relative** and managed with the `{here}` package. No
> hard‑coded absolute paths.

------------------------------------------------------------------------

## 🚀 How to Reproduce

### 0) Requirements

- R ≥ 4.2
- Packages used throughout the project (installed automatically in
  `01_load_packages.R`) at the specific version used.

------------------------------------------------------------------------

## 1) Prepare, Clean & Merge (single master)

Run:

``` r
source(here::here(\scripts\ \01_load_packages.R\))
source(here::here(\scripts\ \02_functions.R\))
source(here::here(\scripts\ \04_clean_merge.R\))
```

This script (04_clean_merge.R):

- Loads **data_clean/raw_data.rds** (all raw sources pre‑combined).  
- Cleans each dataset, standardizes country names, suffixes variables
  (`_vdem`, `_tpt`, `_gini`, etc.).  
- Anchors all joins on **Third Pillar (tpt)**.  
- Processes all datasets.  
- Derives canonical variables *(computed once)*:
  - `Poli_polarization_pct`
  - `Facebook_users_per100k_fb`
- Performs diagnostics (coverage, duplicates).
- Saves:
  - `data_clean/master_dataset.rds`
  - `data_clean/master_dataset.xlsx`

------------------------------------------------------------------------

## 2) Descriptives & Freezing Analysis Subsets

Run:

``` r
source(here::here(\scripts\ \05_descriptives.R\))
```

This script:

- Loads `master_dataset.rds`.

- Computes **missingness**, **global**, and **income‑group**
  descriptives.

- Builds analysis subsets:

  - `polarization_all`  
  - `pol_upper`  
  - `pol_lower`

- Builds the **mediation subset** (`mediation`) and its z‑score version
  (`mediation_z`).

- **Saves all analysis‑ready subsets** to:

<!-- -->

    data_clean/analysis_subsets/
        polarization_all.rds
        pol_upper.rds
        pol_lower.rds
        polarization_gdp.rds
        mediation_dataset.rds
        mediation_dataset_z.rds

- Exports tables to `outputs/descriptives/`.
- Saves plots to `outputs/figures/`.

------------------------------------------------------------------------

## 3) Regressions

Run:

``` r
source(here::here(\scripts\ \06_regressions.R\))
```

This script uses **frozen RDS subsets**, not the full master.

It performs:

- Core regressions:
  - `Poli_polarization ~ Third_pillar_size`
  - for **all**, **upper+high**, **low+lower** incomes.
- GDP‑adjusted regressions.
- Regressions for the future mediators in use:
  - Add **Trust**, **Gini**, **Loneliness**, **Facebook** one at a time.

It produces:

- Shapiro–Wilk, Durbin–Watson, Breusch–Pagan tests  
- Residual vs Fitted and QQ plots  
- **Automatic narrative interpretation** for each model

Outputs live in:  
`outputs/regressions/` and `outputs/figures/`.

------------------------------------------------------------------------

## 4) Mediation Analyses

### `psych::mediate()` usage for mediation

``` r
source(here::here(\scripts\ \07_mediation_psych.R\))
```

Consumes:

    data_clean/analysis_subsets/mediation_dataset_z.rds

Produces outputs in:

- `outputs/mediation_psych/`

------------------------------------------------------------------------

## 📦 Data Sources

All raw data are serialized inside **data_clean/raw_data.rds**.

Sources include:

- Third Pillar (tpt)
- V‑Dem
- OECD Trust in Government
- World Bank: Gini, Population, GDP
- Facebook Users
- OWID (one‑person households)
- WVS (trust), IVS (trust)

The indicator for trust that is finally used in the project is that of
IVS.

All country names standardized.  
Variables are suffix‑tagged by source (e.g., `_vdem`, `_owid`, `_tpt`,
`_ivs`).

------------------------------------------------------------------------

## 🧪 Conventions

- Anchor dataset = **Third Pillar** (`tpt`)
- All joins via `left_join()`
- Avoids name collisions using suffixes
- Derived indicators computed **once** in `04_clean_merge.R`
- Frozen RDS subsets used for regression/mediation
- Automatic diagnostics accompany each regression

------------------------------------------------------------------------

## 📊 Key Outputs

- `master_dataset.rds` — all indicators, cleaned structure  
- `analysis_subsets/*.rds` — ready-to-model subsets  
- `outputs/descriptives/` — summary tables, missingness  
- `outputs/figures/` — all plots  
- `outputs/regressions/` — OLS models + diagnostics  
- `outputs/mediation_psych/` — psych::mediate summaries

------------------------------------------------------------------------
