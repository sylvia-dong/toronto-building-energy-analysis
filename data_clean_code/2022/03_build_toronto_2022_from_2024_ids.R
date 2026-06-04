
library(readxl)
library(dplyr)
library(stringr)
library(writexl)
library(tidyr)

# ==========================================
# FILE PATHS
# ==========================================
# Use 2022-1, because it is the portfolio that aligns with the 2024 file structure
file_2022 <- "~/Desktop/raw data/annual-energy-consumption-data-2022 - 1.xlsx"
file_2024_master <- "~/Desktop/cleaned data/toronto_2024_property_energy_summary_final_with_EUI_GHG.xlsx"

# ==========================================
# CONVERSION / EMISSION FACTORS
# ==========================================
GAS_TO_EKWH  <- 10.68   # 1 m3 natural gas -> ekWh
GAS_TO_CO2E  <- 1.921   # kg CO2e per m3 natural gas
ELEC_TO_CO2E <- 0.038   # kg CO2e per kWh, Ontario 2022 (team's current historical series)

# ==========================================
# READ FILES
# ==========================================

properties_2022 <- read_excel(file_2022, sheet = "Properties", skip = 5)
property_ids_2022 <- read_excel(file_2022, sheet = "Property IDs", skip = 5)
meter_entries_2022 <- read_excel(file_2022, sheet = "Meter Entries", skip = 5)

master_2024 <- read_excel(file_2024_master, sheet = "Summary")

# ==========================================
# STEP 1: USE 2024 PORTFOLIO MANAGER IDs AS MASTER LIST
# ==========================================
target_ids <- master_2024 %>%
  distinct(Portfolio_Manager_ID) %>%
  rename(`Portfolio Manager ID` = Portfolio_Manager_ID)

# ==========================================
# STEP 2: KEEP MATCHED PROPERTIES FROM 2022
# ==========================================
properties_matched_2022 <- properties_2022 %>%
  semi_join(target_ids, by = "Portfolio Manager ID") %>%
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
  )

# Optional checks
cat("Matched 2022 properties:", nrow(properties_matched_2022), "\n")
cat("GFA unit check:\n")
print(table(properties_matched_2022$`GFA Units`, useNA = "ifany"))

# ==========================================
# STEP 3: GET WEEKLY AVERAGE HOURS FROM 2022
# ==========================================
weekly_hours_2022 <- property_ids_2022 %>%
  semi_join(target_ids, by = "Portfolio Manager ID") %>%
  filter(`Custom ID 3 Name` == "Weekly average hours") %>%
  select(
    `Portfolio Manager ID`,
    `Custom ID 3 Number`
  ) %>%
  distinct() %>%
  rename(Weekly_Operating_Hours = `Custom ID 3 Number`)

# ==========================================
# STEP 4: FILTER 2022 METER ENTRIES
# ==========================================
meter_filtered_2022 <- meter_entries_2022 %>%
  semi_join(target_ids, by = "Portfolio Manager ID") %>%
  filter(`Meter Type` %in% c("Natural Gas", "Electric - Grid")) %>%
  mutate(`Start Date` = as.Date(`Start Date`)) %>%
  filter(format(`Start Date`, "%Y") == "2022")

# ==========================================
# STEP 5: SUM ANNUAL CONSUMPTION FOR 2022
# ==========================================
energy_summary_2022 <- meter_filtered_2022 %>%
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
# STEP 6: MERGE AND CALCULATE EUI / GHG
# ==========================================
final_table_2022 <- properties_matched_2022 %>%
  left_join(weekly_hours_2022, by = "Portfolio Manager ID") %>%
  left_join(energy_summary_2022, by = "Portfolio Manager ID") %>%
  mutate(
    Electric_Grid_kWh = ifelse(is.na(Electric_Grid_kWh), 0, Electric_Grid_kWh),
    Natural_Gas_m3    = ifelse(is.na(Natural_Gas_m3), 0, Natural_Gas_m3),
    Year              = 2022,
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
# STEP 7: EXPORT
# ==========================================
dir.create("~/Desktop/cleaned data", recursive = TRUE, showWarnings = FALSE)

write.csv(
  final_table_2022,
  "~/Desktop/cleaned data/toronto_2022_property_energy_summary_final_with_EUI_GHG.csv",
  row.names = FALSE,
  na = ""
)

write_xlsx(
  list(Summary = final_table_2022),
  "~/Desktop/cleaned data/toronto_2022_property_energy_summary_final_with_EUI_GHG.xlsx"
)

# ==========================================
# QUICK CHECKS
# ==========================================
cat("Number of matched 2022 properties:", nrow(final_table_2022), "\n\n")

cat("Property type counts:\n")
print(table(final_table_2022$Property_Type_Raw, useNA = "ifany"))

cat("\nPreview:\n")
print(head(final_table_2022, 10))

cat("\nSummary of EUI:\n")
print(summary(final_table_2022$EUI_ekWh_sqft))

cat("\nSummary of GHG intensity:\n")
print(summary(final_table_2022$GHG_Intensity_kg_sqft))
