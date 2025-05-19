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


# colors = c("#d7790f", "#3F8BCA", "#00BDB2", "#F4B811", "#DE663E", "#FF912B",
#            "#1b4793", "#fc7255")

# https://www.pinterest.com/pin/472878029644220127/
colors = c("#1b4793", "#2178ae", "#fac92c",
           "#f7b0be", "#ed8e83", "#f15a42",  "#ef3c23",
           "#18bc9c")

colors = c("#024555", "#ff5738", "#edba3d", "#95b0dd")

colors = c("#3d1a8e", "#f393af", "#f66529", "#68c7c9", "#fccd0d", "#f3e5d8")

