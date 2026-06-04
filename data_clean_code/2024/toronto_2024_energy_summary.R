library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(tidyr)

# ==========================================
# FILE PATH
# ==========================================
file_path <- "~/Desktop/raw data/annual-energy-consumption-data-2024.xlsx"

# ==========================================
# CONVERSION / EMISSION FACTORS
# ==========================================
GAS_TO_EKWH  <- 10.68   # 1 m3 natural gas -> ekWh
GAS_TO_CO2E  <- 1.921   # kg CO2e per m3 natural gas
ELEC_TO_CO2E <- 0.030   # kg CO2e per kWh, Ontario 2024

# ==========================================
# READ 3 TABS
# ==========================================
properties    <- read_excel(file_path, sheet = "Properties")
property_ids  <- read_excel(file_path, sheet = "Property IDs")
meter_entries <- read_excel(file_path, sheet = "Meter Entries")

# ==========================================
# KEEP ONLY SELECTED PROPERTY TYPES
# ==========================================
keep_types <- c(
  "Office",
  "Community Center and Social Meeting Hall",
  "Pre-school/Daycare",
  "Police Station",
  "Library",
  "Fire Station",
  "Other - Recreation"
)

# ==========================================
# STEP 1: KEEP TORONTO + GFA > 1000 + TARGET TYPES
# ==========================================
toronto_properties <- properties %>%
  filter(`City/Municipality` == "Toronto") %>%
  select(
    `Property Name`,
    `Portfolio Manager ID`,
    `Street Address`,
    `City/Municipality`,
    `State/Province`,
    `Postal Code`,
    `Country`,
    `Property Type - Self-Selected`,
    `Gross Floor Area`,
    `GFA Units`
  ) %>%
  filter(`Gross Floor Area` > 1000) %>%
  filter(`Property Type - Self-Selected` %in% keep_types)

# Optional check: confirm area units
cat("GFA unit check:\n")
print(table(toronto_properties$`GFA Units`, useNA = "ifany"))

# ==========================================
# STEP 2: GET WEEKLY AVERAGE HOURS
# ==========================================
weekly_hours <- property_ids %>%
  filter(`Custom ID 3 Name` == "Weekly average hours") %>%
  select(
    `Portfolio Manager ID`,
    `Custom ID 3 Number`
  ) %>%
  distinct() %>%
  rename(Weekly_Operating_Hours = `Custom ID 3 Number`)

# ==========================================
# STEP 3: FILTER METER ENTRIES FOR MATCHED IDS, 2024, GAS + ELECTRICITY
# ==========================================
target_ids <- toronto_properties %>%
  distinct(`Portfolio Manager ID`)

meter_filtered <- meter_entries %>%
  semi_join(target_ids, by = "Portfolio Manager ID") %>%
  filter(`Meter Type` %in% c("Natural Gas", "Electric - Grid")) %>%
  mutate(`Start Date` = as.Date(`Start Date`)) %>%
  filter(format(`Start Date`, "%Y") == "2024")

# ==========================================
# STEP 4: SUM ANNUAL CONSUMPTION
# ==========================================
energy_summary <- meter_filtered %>%
  group_by(`Portfolio Manager ID`, `Meter Type`) %>%
  summarise(
    Annual_Consumption = sum(`Usage/Quantity`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    metric_name = case_when(
      `Meter Type` == "Electric - Grid" ~ "Electric_Grid_kWh",
      `Meter Type` == "Natural Gas"     ~ "Natural_Gas_m3"
    )
  ) %>%
  select(-`Meter Type`) %>%
  pivot_wider(
    names_from = metric_name,
    values_from = Annual_Consumption
  )

# ==========================================
# STEP 5: MERGE AND CALCULATE EUI / GHG
# ==========================================
final_table <- toronto_properties %>%
  left_join(weekly_hours, by = "Portfolio Manager ID") %>%
  left_join(energy_summary, by = "Portfolio Manager ID") %>%
  mutate(
    Electric_Grid_kWh = ifelse(is.na(Electric_Grid_kWh), 0, Electric_Grid_kWh),
    Natural_Gas_m3    = ifelse(is.na(Natural_Gas_m3), 0, Natural_Gas_m3),
    Year              = 2024,
    Total_Energy_ekWh = Electric_Grid_kWh + Natural_Gas_m3 * GAS_TO_EKWH,
    EUI_ekWh_sqft     = Total_Energy_ekWh / `Gross Floor Area`,
    Total_GHG_kg      = Electric_Grid_kWh * ELEC_TO_CO2E +
      Natural_Gas_m3 * GAS_TO_CO2E,
    GHG_Intensity_kg_sqft = Total_GHG_kg / `Gross Floor Area`
  ) %>%
  transmute(
    Year = Year,
    Property_Name = `Property Name`,
    Portfolio_Manager_ID = `Portfolio Manager ID`,
    Street_Address = `Street Address`,
    City = `City/Municipality`,
    Province = `State/Province`,
    Postal_Code = `Postal Code`,
    Country = Country,
    Property_Type_Raw = `Property Type - Self-Selected`,
    GFA_sqft = `Gross Floor Area`,
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
# STEP 6: EXPORT
# ==========================================
dir.create("~/Desktop/cleaned data", recursive = TRUE, showWarnings = FALSE)

write.csv(
  final_table,
  "~/Desktop/cleaned data/toronto_2024_property_energy_summary_final_with_EUI_GHG.csv",
  row.names = FALSE,
  na = ""
)

write_xlsx(
  list(Summary = final_table),
  "~/Desktop/cleaned data/toronto_2024_property_energy_summary_final_with_EUI_GHG.xlsx"
)

# ==========================================
# QUICK CHECKS
# ==========================================
cat("Number of Toronto properties after all filters:", nrow(final_table), "\n\n")

cat("Property type counts:\n")
print(table(final_table$Property_Type_Raw, useNA = "ifany"))

cat("\nPreview:\n")
print(head(final_table, 10))

cat("\nSummary of EUI:\n")
print(summary(final_table$EUI_ekWh_sqft))

cat("\nSummary of GHG intensity:\n")
print(summary(final_table$GHG_Intensity_kg_sqft))