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

colors = c("#1b4793")

colors = c("#0d102f", 
           "#2b2f86",
           "#6666ae",
           "#cdcae5",
           "#22409a",
           
           
           "#eb1c24",
           "#f06666",
           "#f9cbcb",
           
           "#19331a",
           "#00a14b",
           "#77c04d",
           "#356734",
           "#99cb98",
           "#d1e7c3"
           )





