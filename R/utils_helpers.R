# ============================================================
# Title:    Utility Functions and Themes
# Author:   Mari Roberts
# Created:  2025-05-18
# Purpose:  Load data, apply custom themes, and produce exploratory charts
# ============================================================

# ------------------------
# Data Loading Functions
# ------------------------

#' @title Load UCLA Behind Bars CSVs into Global Environment
#'
#' @description Loads all `.csv` files from the specified subfolders in a local 
#' data directory and assigns each as a data frame to the global environment.
#' Assumes files have been manually downloaded from:
#' \url{https://github.com/uclalawcovid19behindbars/data}
#'
#' @param data_dir Path to the local data directory. Defaults to `"data"`.
#' @param folders Character vector of subfolder names. Defaults to 
#' `c("latest-data", "historical-data", "anchored-data")`.
#'
#' @return No return value. Assigns data frames to the global environment.
#' @export
load_ucla_csvs <- function(data_dir = "data",
                           folders = c("latest-data", "historical-data", "anchored-data")) {
  walk(folders, function(folder) {
    folder_path <- file.path(data_dir, folder)
    
    if (!dir.exists(folder_path)) {
      stop(
        glue::glue("Folder not found: '{folder_path}'.\n",
                   "Please manually download the data from:\n",
                   "https://github.com/uclalawcovid19behindbars/data\n",
                   "and place it in the appropriate folder.")
      )
    }
    
    csv_files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)
    
    walk(csv_files, function(file) {
      object_name <- tools::file_path_sans_ext(basename(file))
      assign(object_name, read_csv(file, show_col_types = FALSE), envir = .GlobalEnv)
    })
  })
}

#' @title Get West Virginia Subset
#'
#' @description Takes the name of a data frame and returns a cleaned version 
#' filtered to West Virginia (`state == "West Virginia"`).
#'
#' @param df_name A character string naming an existing object (e.g., `"latest_facility_counts"`).
#'
#' @return A tibble filtered to West Virginia.
#' @export
get_wv_subset <- function(df_name) {
  get(df_name) |>
    clean_names() |>
    filter(state == "West Virginia")
}

# ------------------------
# Data Cleaning
# ------------------------

#' @title Prepare Deaths Data
#'
#' @description Cleans and transforms raw deaths-in-custody data. Adds year,
#' age groups, parsed dates, and drops rows with missing death dates.
#'
#' @param data A raw tibble of deaths data.
#'
#' @return A cleaned tibble with added `year`, `age`, and parsed date fields.
#' @export
prepare_deaths_data <- function(data) {
  data |>
    clean_names() |>
    mutate(
      date_of_death = ymd(date_of_death),
      booking_date = ymd(booking_date),
      age_at_time_of_death = parse_number(age_at_time_of_death),
      age = cut(
        age_at_time_of_death,
        breaks = c(0, 20, 30, 40, 50, 60, 70, 80, Inf),
        labels = c("Under 20", "20-29", "30-39", "40-49",
                   "50-59", "60-69", "70-79", "80 and Over"),
        right = FALSE
      ),
      year = year(date_of_death)
    ) |>
    filter(!is.na(date_of_death))
}

# ------------------------
# Highcharter Theme
# ------------------------

# Custom highchart theme (not exported)
hc_custom_theme <- hc_theme(
  chart = list(style = list(fontFamily = "Source Sans 3, sans-serif")),
  title = list(style = list(fontFamily = "Source Sans 3, sans-serif", fontWeight = "bold", fontSize = "18px")),
  xAxis = list(
    labels = list(style = list(fontFamily = "Source Sans 3, sans-serif")),
    title = list(style = list(fontFamily = "Source Sans 3, sans-serif"))
  ),
  yAxis = list(
    labels = list(style = list(fontFamily = "Source Sans 3, sans-serif")),
    title = list(style = list(fontFamily = "Source Sans 3, sans-serif"))
  ),
  legend = list(
    itemStyle = list(fontFamily = "Source Sans 3, sans-serif"),
    itemHoverStyle = list(fontWeight = "bold")
  ),
  #colors = c("#3F8BCA", "#00BDB2", "#F4B811", "#DE663E", "#FF912B")
  colors = c("#1b4793", "#fac92c", "#f7b0be", "#2178ae", "#ef3c23", "#ed8e83", "#f15a42")
  # colors = c("#ff5738", "#edba3d", "#95b0dd", "#024555")
  # colors = c("#f66529")
)

# ------------------------
# Reactable Theme
# ------------------------

#' @title Reactable Theme
#'
#' @description Applies consistent Source Sans 3 font, spacing, and minimal styling.
#'
#' @param ... Additional arguments passed to `reactableTheme()`
#'
#' @return A `reactableTheme` object.
#' @export
reactable_theme <- function(...) {
  reactable::reactableTheme(
    style = list(fontFamily = "Source Sans 3, sans-serif", fontSize = "14px"),
    headerStyle = list(
      backgroundColor = "#f8f8f8",
      fontWeight = 600,
      borderBottom = "1px solid #e0e0e0",
      textTransform = "none",
      padding = "8px 12px"
    ),
    cellStyle = list(
      padding = "6px 12px",
      borderBottom = "1px solid #f0f0f0"
    ),
    highlightColor = "#f4f4f4",
    stripedColor = "#fafafa",
    ...
  )
}

# ------------------------
# Highcharter Plot Functions
# ------------------------

#' @title Plot Deaths by Variable
#'
#' @description Creates a bar chart of deaths grouped by a categorical variable,
#' sorting categories in descending order of frequency and cleaning label formatting.
#'
#' @param df Cleaned deaths data.
#' @param var_name Name of the variable to group by (e.g., `"gender"`).
#'
#' @return A `highchart` object.
#' @export
plot_deaths_by_var <- function(df, var_name) {
  var_sym <- sym(var_name)
  
  label_clean <- var_name |>
    str_replace_all("_", " ") |>
    str_to_title()
  
  df |>
    count(!!var_sym, sort = TRUE) |>
    mutate(!!var_sym := fct_reorder(!!var_sym, n)) |>
    hchart("column", hcaes(x = !!var_sym, y = n)) |>
    hc_title(text = paste("Deaths by", label_clean)) |>
    hc_xAxis(title = list(text = label_clean)) |>
    hc_yAxis(title = list(text = "Number of Deaths"), min = 0) |>
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = paste0("<b>Deaths:</b> {point.y}")
    ) |>
    hc_exporting(enabled = TRUE) |> 
    hc_add_theme(hc_custom_theme)
}

#' @title Plot Deaths by Year
#'
#' @description Plots annual trend of deaths in custody.
#'
#' @param df Cleaned deaths data.
#'
#' @return A `highchart` object.
#' @export
plot_deaths_by_year <- function(df) {
  df |>
    count(year) |>
    hchart("line", hcaes(x = year, y = n), name = "Annual Deaths") |> 
    hc_title(text = "Deaths Over Time (All Facilities)") |>
    hc_xAxis(title = list(text = "Year")) |>
    hc_yAxis(title = list(text = "Number of Deaths"), min = 0) |>
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = paste0("<b>Deaths:</b> {point.y}")
    ) |>
    hc_exporting(enabled = TRUE) |> 
    hc_add_theme(hc_custom_theme)
}

#' @title Plot Deaths by Facility Over Time
#'
#' @description Line chart showing annual deaths for facilities with at least `min_total` deaths.
#'
#' @param df Cleaned deaths data.
#' @param min_total Minimum number of deaths required to include a facility.
#'
#' @return A `highchart` object.
#' @export
plot_facility_trends <- function(df, min_total = 5) {
  top_facilities <- df |>
    count(facility_name) |>
    filter(n >= min_total) |>
    pull(facility_name)
  
  df |>
    filter(facility_name %in% top_facilities) |>
    count(year, facility_name) |>
    hchart("line", hcaes(x = year, y = n, group = facility_name)) |>
    hc_title(text = "Deaths Over Time by Facility") |>
    hc_xAxis(title = list(text = "Year")) |>
    hc_yAxis(title = list(text = "Number of Deaths"), min = 0) |>
    hc_tooltip(
      useHTML = TRUE,
      pointFormat = paste0("<b>Deaths:</b> {point.y}")
    ) |>
    hc_exporting(enabled = TRUE) |> 
    hc_add_theme(hc_custom_theme)
}

# ------------------------
# Net Change Calculation
# ------------------------

#' @title Calculate Net Change in Deaths
#'
#' @description Computes the change in number of deaths by facility between two years.
#'
#' @param df Cleaned deaths data.
#' @param start_year Starting year (e.g., 2020).
#' @param end_year Ending year (e.g., 2024).
#'
#' @return A tibble with net change in deaths by facility.
#' @export
calculate_net_change <- function(df, start_year = 2020, end_year = 2024) {
  df |>
    filter(year %in% c(start_year, end_year)) |>
    count(facility_name, year) |>
    pivot_wider(names_from = year, values_from = n, values_fill = 0) |>
    mutate(net_change = !!sym(as.character(end_year)) - !!sym(as.character(start_year))) |>
    arrange(desc(net_change))
}

# ------------------------
# Summary Sentences
# ------------------------

#' @title Generate Summary Sentence for Most Common Group with Percentage
#'
#' @description
#' Summarizes the most common value of a given variable in a deaths-in-custody dataset 
#' between two years, including the share of total deaths.
#'
#' @param df A data frame with a `year` column and the target variable.
#' @param var Variable name to summarize (e.g., "race", "offender_status").
#' @param start_year First year to include in the analysis (default = 2020).
#' @param end_year Last year to include in the analysis (default = 2024).
#'
#' @return A character sentence describing the most common category and percentage.
#' @export
generate_summary_sentence <- function(df, var, start_year = 2020, end_year = 2024) {
  df_filtered <- df %>%
    filter(year >= start_year, year <= end_year)
  
  counts <- df_filtered %>%
    count(.data[[var]]) %>%
    arrange(desc(n))
  
  top_value <- counts[[var]][1]
  top_count <- counts$n[1]
  total_count <- sum(counts$n)
  pct <- round(100 * top_count / total_count)
  
  # Clean label formatting
  label <- str_trim(top_value)
  
  # Construct phrasing
  if (var == "age") {
    phrase <- paste0(label, " years old")
    verb <- "were"
  } else if (var == "offender_status") {
    phrase <- paste0('classified as "', label, '"')
    verb <- "were"
  } else if (var %in% c("manner_of_death", "cause_of_death")) {
    phrase <- tolower(label)
    phrase <- case_when(
      phrase == "natural" ~ "natural causes",
      phrase %in% c("pending", "unknown", "other/unknown", "other unknown") ~ paste("an", phrase, "cause"),
      TRUE ~ phrase
    )
    verb <- "died from"
  } else {
    phrase <- label
    verb <- "were"
  }
  
  paste0("From ", start_year, " to ", end_year, 
         ", most people who died in custody ", verb, " ", phrase, 
         " (", pct, "% of deaths).")
}

#' @title Add breakdown for Other/Unknown and All Other Illnesses
#' @description Prints a sentence summarizing counts and percentages for
#' "Other/unknown" and "All other illnesses" causes of death.
#'
#' @param df A data frame with a column called `cause_of_death`.
#' @return A sentence summarizing deaths by vague/unspecified causes.
#' @export
generate_additional_cod_note <- function(df) {
  total <- nrow(df)
  
  count_other <- df %>%
    filter(str_to_lower(cause_of_death) %in% c("other/unknown", "other unknown")) %>%
    nrow()
  
  count_illness <- df %>%
    filter(str_to_lower(cause_of_death) == "all other illnesses") %>%
    nrow()
  
  pct_other <- round(100 * count_other / total)
  pct_illness <- round(100 * count_illness / total)
  
  sentence <- paste0(
    "However, ", count_other, " deaths (", pct_other, "%) were classified as ",
    "\"Other/unknown\" and ", count_illness, " deaths (", pct_illness, "%) as ",
    "\"All other illnesses.\""
  )
  
  sentence
}


