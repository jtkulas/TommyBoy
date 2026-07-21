# Quarto Shiny Address → Map → Census Dashboard

Files:
- index.qmd : main Quarto Shiny dashboard
- R/helpers.R : helper functions for geocoding and ACS fetch

Prerequisites
1. R (4.0+ recommended)
2. Quarto (https://quarto.org) if you want to render/serve the Quarto document
3. A US Census API key (you mentioned you already have one)

Install required R packages (run in R or RStudio):
install.packages(c("shiny","leaflet","httr","jsonlite","tidycensus","dplyr","tidyr","DT","shinycssloaders","sf"))

Set your Census API key (replace YOUR_KEY):
library(tidycensus)
census_api_key("YOUR_KEY", install = TRUE) # install = TRUE persists it to .Renviron
# Restart R session after installing the key to pick up .Renviron, or run:
# Sys.setenv(CENSUS_API_KEY = "YOUR_KEY")

Run the app
- Option A (Quarto serve): from the repository root run
  quarto preview

- Option B (Run via R):
  - Open index.qmd in RStudio and click "Run Document"
  - Or run a minimal shiny view:
    library(quarto) # optional
    # Or convert to a Shiny app by calling shiny::runApp() with a folder containing index.qmd

Notes and troubleshooting
- If tidycensus returns errors about missing API key, ensure CENSUS_API_KEY is set in your environment or use census_api_key(..., install = FALSE) then Sys.setenv.
- The app uses the US Census Geocoder and may fail for international addresses; it is intended for U.S. addresses.
