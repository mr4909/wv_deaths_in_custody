# ============================================================
# Title:    Global Setup – Libraries
# Author:   Mari Roberts
# Created:  2025-05-17
# Purpose:  Set global options, load required packages
# ============================================================

# ------------------------
# Package Loading
# ------------------------

# Install the package (only once; comment out after installed)
# devtools::install_github("mr4909/behindbarstools")

# I forked the original uclalawcovid19behindbars/behindbarstools package
# and removed `rgeos` from the DESCRIPTION file so it would install on R 4.4,
# where `rgeos` is no longer supported.

# The package successfully installs after patching, but key functions such as `read_scrape_data()`
# do not work as expected due to reliance on a remote server (not live?).

# Instead of troubleshooting further due to time constraints, I downloaded the raw data CSVs directly
# from the public repository at: https://github.com/uclalawcovid19behindbars/data

# These files are organized locally in a `data/` directory:
# - data/anchored-data/         (e.g., state_population_counts.csv)
# - data/historical-data/       (e.g., historical_facility_counts.csv)
# - data/latest-data/           (e.g., latest_facility_counts.csv)

required_packages <- c(
  "tidyverse", 
  "janitor",
  "quarto",
  "htmltools",
  "htmlwidgets",
  "scales",
  "gt",
  "readxl",
  "reactable",
  "highcharter"
)

# Load packages quietly
invisible(lapply(required_packages, library, character.only = TRUE))

# "#d7790f"
# colors = c("#3F8BCA", "#00BDB2", "#F4B811", "#DE663E", "#FF912B")