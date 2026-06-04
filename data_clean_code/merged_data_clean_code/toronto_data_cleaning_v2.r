library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(tidyr)

# ============================================================
# MSE718 Term Project - Toronto Municipal Building Energy
# Data Cleaning & Master Panel Construction (R version)
# ============================================================
# Input:  7 xlsx files (2017-2024, excluding 2021)
# Output: Toronto_Master_Panel_Ready_v2.csv
# ============================================================

# ============================================================
# 1. FILE PATHS
# Put all xlsx files in the same folder, or adjust paths below
# ============================================================

DATA_DIR <- "."

files <- list(
  "2017" = file.path(DATA_DIR, "toronto_2017_property_energy_summary_from_2024_address_postal_with_EUI_GHG.xlsx"),
  "2018" = file.path(DATA_DIR, "toronto_2018_property_energy_summary_from_2024_address_postal_with_EUI_GHG.xlsx"),
  "2019" = file.path(DATA_DIR, "toronto_2019_property_energy_summary_from_2024_address_postal_with_EUI_GHG.xlsx"),
  "2020" = file.path(DATA_DIR, "toronto_2020_property_energy_summary_from_2024_address_postal_with_EUI_GHG.xlsx"),
  "2022" = file.path(DATA_DIR, "toronto_2022_property_energy_summary_final_with_EUI_GHG.xlsx"),
  "2023" = file.path(DATA_DIR, "toronto_2023_property_energy_summary_final_with_EUI_GHG.xlsx"),
  "2024" = file.path(DATA_DIR, "toronto_2024_property_energy_summary_final_with_EUI_GHG.xlsx")
)

# ============================================================
# 2. CONVERSION & EMISSION FACTORS
# ============================================================

GAS_TO_EKWH <- 10.68
GAS_TO_CO2E <- 1.921

ELEC_FACTORS <- c(
  "2017" = 0.019,
  "2018" = 0.030,
  "2019" = 0.029,
  "2020" = 0.033,
  "2022" = 0.038,
  "2023" = 0.030,
  "2024" = 0.030
)

# ============================================================
# 3. PROPERTY TYPE STANDARDIZATION
# 20 raw types -> 6 standard categories
# ============================================================

TYPE_MAP <- c(
  "Fire Station" = "Fire Station",
  "Fire stations and associated offices and facilities" = "Fire Station",
  
  "Library" = "Library",
  "Public libraries" = "Library",
  
  "Office" = "Office",
  "Administrative offices and related facilities, including municipal council chambers" = "Office",
  
  "Community Center and Social Meeting Hall" = "Community Center",
  "Community centres" = "Community Center",
  "Social/Meeting Hall" = "Community Center",
  
  "Other - Recreation" = "Recreation",
  "Indoor recreational facilities" = "Recreation",
  "Indoor swimming pools" = "Recreation",
  "Indoor sports arenas" = "Recreation",
  
  "Police Station" = "Police Station",
  "Police stations and associated offices and facilities" = "Police Station"
)

# ============================================================
# 4. LOAD, RECALCULATE & MERGE ALL YEARS
# ============================================================

cat("Loading and processing files...\n")

dfs <- list()

for (yr in names(files)) {
  path <- files[[yr]]
  
  df <- read_excel(path, sheet = "Summary")
  ef <- ELEC_FACTORS[[yr]]
  
  df <- df %>%
    mutate(
      Total_Energy_ekWh = Electric_Grid_kWh + Natural_Gas_m3 * GAS_TO_EKWH,
      EUI_ekWh_sqft = Total_Energy_ekWh / GFA_sqft,
      Total_GHG_kg = Electric_Grid_kWh * ef + Natural_Gas_m3 * GAS_TO_CO2E,
      GHG_Intensity_kg_sqft = Total_GHG_kg / GFA_sqft,
      ELEC_factor_used = ef
    )
  
  cat(" ", yr, ":", nrow(df), "rows | ELEC factor =", ef, "\n")
  dfs[[yr]] <- df
}

df_raw <- bind_rows(dfs)
cat("\nRaw merged:", nrow(df_raw), "rows\n")

# ============================================================
# 5. STANDARDIZE BUILDING TYPE
# ============================================================

df_raw <- df_raw %>%
  mutate(
    Building_Type = recode(Property_Type_Raw, !!!TYPE_MAP, .default = NA_character_)
  )

dropped <- df_raw %>%
  filter(is.na(Building_Type)) %>%
  count(Property_Type_Raw, sort = TRUE)

cat("\nDropped types:\n")
print(dropped)

df <- df_raw %>%
  filter(!is.na(Building_Type)) %>%
  copy()

cat("After type filter:", nrow(df), "rows\n")

# ============================================================
# 6. DATA QUALITY FILTERS
# ============================================================

n0 <- nrow(df)

df <- df %>%
  filter(EUI_ekWh_sqft > 0)

cat("\nAfter EUI > 0 filter:", nrow(df), "rows (removed", n0 - nrow(df), ")\n")

n1 <- nrow(df)

df <- df %>%
  filter(Electric_Grid_kWh > 0)

cat("After elec > 0 filter:", nrow(df), "rows (removed", n1 - nrow(df), ")\n")

n2 <- nrow(df)

p99_eui <- quantile(df$EUI_ekWh_sqft, 0.99, na.rm = TRUE)
p99_ghg <- quantile(df$GHG_Intensity_kg_sqft, 0.99, na.rm = TRUE)

df <- df %>%
  filter(EUI_ekWh_sqft <= p99_eui) %>%
  filter(GHG_Intensity_kg_sqft <= p99_ghg)

cat("After 99th pct filter:", nrow(df), "rows (removed", n2 - nrow(df), ")\n")
cat("  EUI cap: ", round(p99_eui, 2), "ekWh/sqft\n")
cat("  GHG cap: ", round(p99_ghg, 4), "kg/sqft\n")

# ============================================================
# 7. LOG TRANSFORMS & YEAR SCALING
# ============================================================

df <- df %>%
  mutate(
    log_EUI = log(EUI_ekWh_sqft),
    log_GHG_intensity = log(GHG_Intensity_kg_sqft),
    log_GFA = log(GFA_sqft),
    log_Hours = log(Weekly_Operating_Hours),
    Year_scaled = Year - 2017
  )

# ============================================================
# 8. EXPORT
# ============================================================

final_cols <- c(
  "Year", "Year_scaled",
  "Property_Name", "Portfolio_Manager_ID",
  "Building_Type",
  "GFA_sqft", "Weekly_Operating_Hours",
  "Electric_Grid_kWh", "Natural_Gas_m3", "ELEC_factor_used",
  "EUI_ekWh_sqft", "GHG_Intensity_kg_sqft",
  "log_EUI", "log_GHG_intensity", "log_GFA", "log_Hours"
)

df_final <- df %>%
  select(all_of(final_cols))

output_path <- "Toronto_Master_Panel_Ready_v2.csv"
write.csv(df_final, output_path, row.names = FALSE, na = "")

# ============================================================
# 9. FINAL SUMMARY
# ============================================================

cat("\n", strrep("=", 60), "\n", sep = "")
cat("FINAL DATASET SUMMARY\n")
cat(strrep("=", 60), "\n", sep = "")
cat("Total observations :", nrow(df_final), "\n")
cat("Columns            :", ncol(df_final), "\n")

cat("\nBy year:\n")
print(df_final %>% count(Year))

cat("\nBy building type:\n")
print(df_final %>% count(Building_Type, sort = TRUE))

cat("\nELEC factor used per year:\n")
print(df_final %>% group_by(Year) %>% summarise(ELEC_factor_used = first(ELEC_factor_used), .groups = "drop"))

cat("\nKey variable stats:\n")
print(summary(df_final[, c("EUI_ekWh_sqft", "GHG_Intensity_kg_sqft", "log_EUI", "log_GHG_intensity")]))

cat("\nMissing values:\n")
print(colSums(is.na(df_final)))

cat("\nOutput:", output_path, "\n")
cat(strrep("=", 60), "\n", sep = "")
cat("Next: run toronto_energy_brms_analysis_v2.R in RStudio\n")
cat(strrep("=", 60), "\n", sep = "")