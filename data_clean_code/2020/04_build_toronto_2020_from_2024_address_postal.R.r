library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(tidyr)

# ==========================================
# FILE PATHS
# ==========================================
file_2020 <- "~/Desktop/raw data/annual-energy-consumption-data-2020.xlsx"
file_2024_master <- "~/Desktop/cleaned data/toronto_2024_property_energy_summary_final_with_EUI_GHG.xlsx"

# ==========================================
# CONVERSION / EMISSION FACTORS
# ==========================================
GAS_TO_EKWH  <- 10.68
GAS_TO_CO2E  <- 1.921
ELEC_TO_CO2E <- 0.033   # Ontario 2020, team’s current yearly series

SQM_TO_SQFT <- 10.7639

# ==========================================
# ADDRESS / POSTAL CLEANING FUNCTIONS
# ==========================================
clean_address <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all("\\.", "") %>%
    str_replace_all(",", "") %>%
    str_replace_all("#", "") %>%
    str_replace_all("\\bSTREET\\b", "ST") %>%
    str_replace_all("\\bAVENUE\\b", "AVE") %>%
    str_replace_all("\\bROAD\\b", "RD") %>%
    str_replace_all("\\bDRIVE\\b", "DR") %>%
    str_replace_all("\\bBOULEVARD\\b", "BLVD") %>%
    str_replace_all("\\bCOURT\\b", "CRT") %>%
    str_replace_all("\\bPLACE\\b", "PL") %>%
    str_replace_all("\\bLANE\\b", "LN") %>%
    str_replace_all("\\bNORTH\\b", "N") %>%
    str_replace_all("\\bSOUTH\\b", "S") %>%
    str_replace_all("\\bEAST\\b", "E") %>%
    str_replace_all("\\bWEST\\b", "W") %>%
    str_replace_all("\\bUNIT\\s+[A-Z0-9-]+\\b", "") %>%
    str_replace_all("\\bSUITE\\s+[A-Z0-9-]+\\b", "") %>%
    str_replace_all("\\s+", " ") %>%
    str_trim()
}

clean_postal <- function(x) {
  x %>%
    str_to_upper() %>%
    str_replace_all(" ", "") %>%
    str_trim()
}

# ==========================================
# READ 2024 MASTER
# ==========================================
master_2024 <- read_excel(file_2024_master, sheet = "Summary") %>%
  mutate(
    Address_Norm = clean_address(Street_Address),
    Postal_Norm  = clean_postal(Postal_Code)
  )

# Build unique address+postal master list from 2024
address_master_2024 <- master_2024 %>%
  group_by(Address_Norm, Postal_Norm) %>%
  summarise(
    Portfolio_Manager_ID_2024 = first(Portfolio_Manager_ID),
    .groups = "drop"
  )

cat("Unique address+postal pairs in 2024 master:", nrow(address_master_2024), "\n")

# ==========================================
# READ 2020 RAW FILE
# 2020 is a single-sheet wide table with metadata rows on top
# ==========================================
raw_2020 <- read_excel(
  file_2020,
  sheet = 1,
  skip = 5,
  col_names = FALSE
)

# Keep first 36 columns used by the reporting template
raw_2020 <- raw_2020[, 1:36]

colnames(raw_2020) <- c(
  "Operation_Name",
  "Operation_Type",
  "Address",
  "City",
  "Postal_Code",
  "Total_Floor_Area",
  "Area_Unit",
  "Avg_hrs_wk",
  "Annual_Flow_ML",
  "Electric_Grid_kWh",
  "Electric_Grid_Unit",
  "Natural_Gas_m3",
  "Natural_Gas_Unit",
  "Fuel_Oil_1_2_Qty",
  "Fuel_Oil_1_2_Unit",
  "Fuel_Oil_4_6_Qty",
  "Fuel_Oil_4_6_Unit",
  "Propane_Qty",
  "Propane_Unit",
  "Coal_Qty",
  "Coal_Unit",
  "Wood_Qty",
  "Wood_Unit",
  "District_Heating_Qty",
  "District_Heating_Unit",
  "District_Heating_Renewable",
  "District_Heating_Emission_Factor",
  "District_Cooling_Qty",
  "District_Cooling_Unit",
  "District_Cooling_Renewable",
  "District_Cooling_Emission_Factor",
  "GHG_Emissions_Reported_kg",
  "Energy_Intensity_Reported_ekWh_sqft",
  "Energy_Intensity_Reported_ekWh_ML",
  "Building_Operation_Identifier",
  "Comments"
)

# ==========================================
# CLEAN 2020 AND MATCH TO 2024 BY ADDRESS + POSTAL
# ==========================================
clean_2020 <- raw_2020 %>%
  mutate(
    Address_Norm = clean_address(Address),
    Postal_Norm  = clean_postal(Postal_Code),
    Province     = "Ontario",
    Country      = "Canada"
  ) %>%
  filter(!is.na(Operation_Name), Operation_Name != "") %>%
  filter(!is.na(Address_Norm), Address_Norm != "") %>%
  filter(!is.na(Postal_Norm), Postal_Norm != "") %>%
  semi_join(address_master_2024, by = c("Address_Norm", "Postal_Norm")) %>%
  left_join(address_master_2024, by = c("Address_Norm", "Postal_Norm"))

# ==========================================
# CONVERT AREA TO SQFT
# ==========================================
clean_2020 <- clean_2020 %>%
  mutate(
    GFA_sqft = case_when(
      Area_Unit == "Square feet"   ~ as.numeric(Total_Floor_Area),
      Area_Unit == "Square meters" ~ as.numeric(Total_Floor_Area) * SQM_TO_SQFT,
      TRUE ~ NA_real_
    )
  )

# ==========================================
# CLEAN NUMERIC FIELDS
# ==========================================
clean_2020 <- clean_2020 %>%
  mutate(
    Electric_Grid_kWh       = as.numeric(Electric_Grid_kWh),
    Natural_Gas_m3          = as.numeric(Natural_Gas_m3),
    Weekly_Operating_Hours  = as.numeric(Avg_hrs_wk)
  ) %>%
  mutate(
    Electric_Grid_kWh = ifelse(is.na(Electric_Grid_kWh), 0, Electric_Grid_kWh),
    Natural_Gas_m3    = ifelse(is.na(Natural_Gas_m3), 0, Natural_Gas_m3)
  )

# ==========================================
# CALCULATE EUI / GHG
# ==========================================
final_table_2020 <- clean_2020 %>%
  mutate(
    Year = 2020,
    Total_Energy_ekWh = Electric_Grid_kWh + Natural_Gas_m3 * GAS_TO_EKWH,
    EUI_ekWh_sqft = Total_Energy_ekWh / GFA_sqft,
    Total_GHG_kg = Electric_Grid_kWh * ELEC_TO_CO2E +
      Natural_Gas_m3 * GAS_TO_CO2E,
    GHG_Intensity_kg_sqft = Total_GHG_kg / GFA_sqft
  ) %>%
  transmute(
    Year = Year,
    Property_Name = Operation_Name,
    Portfolio_Manager_ID = Portfolio_Manager_ID_2024,
    Street_Address = Address,
    City = City,
    Province = Province,
    Postal_Code = Postal_Code,
    Country = Country,
    Property_Type_Raw = Operation_Type,
    GFA_sqft = GFA_sqft,
    Weekly_Operating_Hours = Weekly_Operating_Hours,
    Electric_Grid_kWh = Electric_Grid_kWh,
    Natural_Gas_m3 = Natural_Gas_m3,
    Total_Energy_ekWh = Total_Energy_ekWh,
    EUI_ekWh_sqft = EUI_ekWh_sqft,
    Total_GHG_kg = Total_GHG_kg,
    GHG_Intensity_kg_sqft = GHG_Intensity_kg_sqft
  ) %>%
  arrange(Property_Name)

# ==========================================
# EXPORT
# ==========================================
dir.create("~/Desktop/cleaned data", recursive = TRUE, showWarnings = FALSE)

write.csv(
  final_table_2020,
  "~/Desktop/cleaned data/toronto_2020_property_energy_summary_from_2024_address_postal_with_EUI_GHG.csv",
  row.names = FALSE,
  na = ""
)

write_xlsx(
  list(Summary = final_table_2020),
  "~/Desktop/cleaned data/toronto_2020_property_energy_summary_from_2024_address_postal_with_EUI_GHG.xlsx"
)

# ==========================================
# QUICK CHECKS
# ==========================================
cat("Number of matched 2020 properties:", nrow(final_table_2020), "\n\n")

cat("Preview:\n")
print(head(final_table_2020, 10))

cat("\nSummary of EUI:\n")
print(summary(final_table_2020$EUI_ekWh_sqft))

cat("\nSummary of GHG intensity:\n")
print(summary(final_table_2020$GHG_Intensity_kg_sqft))