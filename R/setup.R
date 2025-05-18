# ============================================================
# Title:    Global Setup – Libraries and Branding
# Author:   Mari Roberts
# Created:  2025-05-17
# Purpose:  Set global options, load required packages, and define
#           UCLA Behind Bars branding colors and font references.
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
  "readxl"
)

# Load packages quietly
invisible(lapply(required_packages, library, character.only = TRUE))

# ------------------------
# UCLA Behind Bars Branding – Colors and Fonts
# ------------------------

ucla_colors <- list(
  white        = "#ffffff",
  black        = "#000000",
  gray         = "#e1e0e5",  # mischka-gray
  blue         = "#5d5e79",  # comet-blue
  forest       = "#283224",  # black-forest
  ivory        = "#fbfbf7",
  mist         = "#c8c8b9",  # moon-mist
  orange       = "#d7790f",  # ochre
  green        = "#82caa4",  # summer-green
  darkblue     = "#4c6788",  # wedgewood
  olive        = "#84806f",  # olive-haze
  saratoga     = "#555526",
  font_color   = "#000000",  
  font_inverse = "#ffffff"   
)

# Font stack will be applied via custom CSS
# Titles: Plantin
# Body: Neue Haas Grotesk Display
# UCLA label: Champion Gothic Featherweight
# Behind Bars Data Project label: Champion Gothic Middleweight