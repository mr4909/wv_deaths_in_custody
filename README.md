# WV Deaths in Custody – Public Data Access

This repository hosts a Quarto-based website for exploring and downloading data on deaths in custody reported by the West Virginia Division of Corrections and Rehabilitation (DCR). The dataset was compiled from public records obtained through FOIA requests and published by the **West Virginia Center on Budget and Policy (WVCBP)**.

## Live Site

You can view the full site here:  
[https://wv-deaths-in-custody.netlify.app/](https://wv-deaths-in-custody.netlify.app/)

---

## Project Structure

```
├── data/
│ ├── wvcbp_deaths.rds # Processed dataset for internal use
│ ├── wvcbp_deaths.csv # Downloadable CSV exposed on the site
│ ├── anchored-data/ # Public-facing denominators
│ └── historical-data/ # Multi-year counts by facility/state
├── R/
│ ├── setup.R # Global packages and options
│ └── utils_helpers.R # Helper functions
├── download_data.qmd # Public data page with reactable table
├── analysis.qmd # Full analysis results
├── key_findings.qmd # Executive summary
├── index.qmd # Homepage
├── about.qmd # About the project
├── contact.qmd # Contact information
├── styles.css # Site-wide custom styles
├── _quarto.yml # Site configuration
└── README.md # This file
```

## Live Page

You can view the published site here (replace with your actual link):

[https://wv-deaths-in-custody.netlify.app/](https://wv-deaths-in-custody.netlify.app/)

## Data Access

The dataset available for download includes:

- Date of death and booking date  
- Age, gender, and race  
- Facility name and type  
- Manner and cause of death  
- Legal status at time of death (e.g., pretrial, sentenced)

Download directly here:  
[**Download CSV**](data/wvcbp_deaths_in_custody.csv)

## Sources and FOIA References

This dataset is based on responses to FOIA requests submitted to DCR by WVCBP and journalists.

- [September 2024 FOIA Response – August deaths](https://wvpolicy.org/wp-content/uploads/2024/10/2024-09-25-DCR-Response-to-Whitaker-Sara-N.-FOIA-request-inmate-count-09-03-24-deaths-PDs-25.pdf)  
- [January 2024 FOIA Response – Deaths 2009–present](https://wvpolicy.org/wp-content/uploads/2024/04/2024.01.16-DCR-Response-to-Tony-Mike-Gazette-FOIA-request-DCR-inmate-deaths-2009-present-1.pdf)  
- [February 2025 FOIA Response – Inspector General reports](https://wvpolicy.org/wp-content/uploads/2025/02/2025.02.14-DHS-FOIA-Response-re-Inspector-General-reports.pdf)

Additional context:  
[**Are fewer people dying in West Virginia jails, or is the state simply not counting?**](https://wvpolicy.org/are-fewer-people-dying-in-west-virginia-jails-or-is-the-state-simply-not-counting/)

## Setup

To render the site locally:

1. Install required packages.
2. Source setup files.
 ```r
 source("R/setup.R")
 ```
3. Render the site:
 ```r
 quarto::quarto_render()
 ```

