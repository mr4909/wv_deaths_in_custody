# ============================================================
# Title:    Pull and Prepare UCLA Behind Bars Data
# Author:   Mari Roberts
# Created:  2025-05-17
# Purpose:  Fetch latest and historical prison death data
# Source:   https://github.com/uclalawcovid19behindbars/data
# ============================================================

# ---- Load all UCLA Behind Bars CSVs into global environment ----
load_ucla_csvs()

# ---- Load Whitaker data ----
# Define the path to the Excel file
excel_path <- "data/DCR-Deaths-in-Custody-Jan-2020-to-March-2025.xlsx"

# Define the sheet names
sheet_names <- paste0("Table ", 1:6)

# Read and combine the sheets, adding a column to indicate the source table
wvcbp_deaths <- lapply(sheet_names, function(sheet) {
  read_excel(excel_path, sheet = sheet) |>
    clean_names() |>
    mutate(
      source_table = sheet,
      date_of_death = mdy(date_of_death),
      booking_date = mdy(booking_date)
    )
}) |>
  bind_rows()

facility_lookup <- read_csv("data/facility_lookup.csv")

# ---- Merge facility info into wvcbp_deaths ----
wvcbp_deaths <- wvcbp_deaths |>
  left_join(facility_lookup, by = "facility") |> 
  filter(!is.na(date_of_death)) |> 
  mutate(across(c(race, gender, facility, manner_of_death, cause_of_death, 
                  offender_status, county, source_table, facility_name, type), as.factor))

# ---- Filter and clean West Virginia datasets ----
wv_historical_state_counts <- get_wv_subset("historical_state_counts")
wv_latest_facility_counts <- get_wv_subset("latest_facility_counts")
wv_latest_state_counts <- get_wv_subset("latest_state_counts")
wv_latest_state_jurisdiction_counts <- get_wv_subset("latest_state_jurisdiction_counts")

# ---- Save as .rds files in the data/ folder ----
saveRDS(wvcbp_deaths, file = "data/wvcbp_deaths.rds")
write.csv(wvcbp_deaths, "data/wvcbp_deaths.csv", row.names = FALSE, na = "")
saveRDS(wv_historical_state_counts, file = "data/wv_historical_state_counts.rds")
saveRDS(wv_latest_facility_counts, file = "data/wv_latest_facility_counts.rds")
saveRDS(wv_latest_state_counts, file = "data/wv_latest_state_counts.rds")
saveRDS(wv_latest_state_jurisdiction_counts, file = "data/wv_latest_state_jurisdiction_counts.rds")

