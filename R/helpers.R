# Helper functions for the Quarto dashboard
library(httr)
library(jsonlite)
library(tidycensus)
library(dplyr)
library(tidyr)
library(sf)

# geocode_census: calls the US Census geocoding API for a one-line address
geocode_census <- function(address, benchmark = "Public_AR_Census2020") {
  if(missing(address) || nchar(trimws(address)) == 0) stop("address required")
  base <- "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"
  res <- httr::GET(url = base, query = list(address = address, benchmark = benchmark, format = "json"))
  if(res$status_code != 200) stop("Geocoder request failed: ", res$status_code)
  parsed <- content(res, as = "parsed", simplifyVector = FALSE)
  return(parsed$result)
}

# get_acs_for_tract: given FIPS parts, fetch ACS variables and tract geometry (geometry=TRUE)
get_acs_for_tract <- function(state_fips, county_fips, tract_code, vars, year = 2021) {
  # vars: named vector of variable codes, e.g. c(total_pop = "B01003_001", ...)
  if(missing(vars)) {
    vars <- c(total_pop = "B01003_001")
  }
  # fetch ACS with geometry so we can draw the tract polygon
  # tidycensus get_acs allows passing 'state','county','tract'
  # tract parameter expects 6-digit tract code (padded)
  tract_code <- sprintf("%06s", tract_code)
  # first get geometry with one variable (to avoid multi-row geometry duplication issues)
  geometry_df <- tidycensus::get_acs(geography = "tract",
                                     variables = vars[1],
                                     state = state_fips,
                                     county = county_fips,
                                     tract = tract_code,
                                     year = year,
                                     geometry = TRUE,
                                     survey = "acs5")
  # Now get the other variables (without geometry) and join
  var_codes <- unname(vars)
  acs_raw <- tidycensus::get_acs(geography = "tract",
                                 variables = var_codes,
                                 state = state_fips,
                                 county = county_fips,
                                 tract = tract_code,
                                 year = year,
                                 geometry = FALSE,
                                 survey = "acs5")
  # acs_raw will have rows per variable. Pivot wider.
  acs_wide <- acs_raw %>%
    select(GEOID, variable, estimate, moe) %>%
    tidyr::pivot_wider(names_from = variable, values_from = c(estimate, moe), names_sep = "__")

  # Map variable codes back to names for readability
  # create summary table with metric names and estimates
  metric_df <- tibble::tibble(
    metric = names(vars),
    var_code = unname(vars)
  ) %>%
    rowwise() %>%
    mutate(estimate = acs_wide[[paste0("estimate__", var_code)]],
           moe = acs_wide[[paste0("moe__", var_code)]]) %>%
    ungroup()

  # Return geometry and a human-friendly table
  list(
    geometry = geometry_df,
    summary_table = metric_df %>% select(metric, estimate, moe)
  )
}
