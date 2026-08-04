# Minimal, robust single-address geocoder for the Census onelineaddress endpoint.
# Returns a named list: list(status=..., lat=..., lon=..., label=..., raw=...) for diagnostics.

library(httr)
library(jsonlite)

geocode_census_one <- function(address, benchmark = "Public_AR_Current", timeout_seconds = 10) {
  if (missing(address) || nchar(trimws(address)) == 0) return(list(status = "error", message = "empty address"))
  base <- "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"

  resp <- tryCatch({
    httr::GET(
      url = base,
      query = list(address = address, benchmark = benchmark, format = "json"),
      httr::user_agent("TommyBoy/1.0 (geocode_census_one)"),
      httr::timeout(timeout_seconds)
    )
  }, error = function(e) {
    list(status = "error", message = paste0("HTTP error: ", e$message))
  })

  if (is.list(resp) && !inherits(resp, "response")) return(resp)
  if (httr::status_code(resp) != 200) return(list(status = "error", message = paste0("HTTP status ", httr::status_code(resp))))

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch({
    jsonlite::fromJSON(txt, simplifyVector = FALSE)
  }, error = function(e) {
    NULL
  })
  if (is.null(parsed)) return(list(status = "error", message = "JSON parse error"))

  matches <- parsed$result$addressMatches
  if (is.null(matches) || length(matches) == 0) return(list(status = "nomatch", message = "no addressMatches returned", raw = parsed$result))

  m <- matches[[1]]
  coords <- NULL
  if (!is.null(m$coordinates)) coords <- m$coordinates
  if (is.null(coords) && !is.null(m$coordinates$x) && !is.null(m$coordinates$y)) coords <- list(x = m$coordinates$x, y = m$coordinates$y)
  if (is.null(coords)) return(list(status = "error", message = "match found but coordinates missing", raw = m))

  lon <- suppressWarnings(as.numeric(coords$x))
  lat <- suppressWarnings(as.numeric(coords$y))
  if (!is.finite(lat) || !is.finite(lon)) return(list(status = "error", message = "coordinates not numeric/finite", raw = m))

  label <- if (!is.null(m$matchedAddress)) m$matchedAddress else address
  return(list(status = "ok", lat = lat, lon = lon, label = label, raw = m))
}

# Returns a small list with population and NAME given an 11-digit tract GEOID, or informative status.
# Uses the Census API directly (no tidycensus). Requires CENSUS_API_KEY set in env or passed via key.
fetch_acs_population <- function(tract_geoid, year = 2021, key = Sys.getenv("CENSUS_API_KEY", "")) {
  if (missing(tract_geoid) || is.null(tract_geoid) || nchar(trimws(tract_geoid)) < 11) {
    return(list(status = "error", message = "invalid tract GEOID"))
  }
  # GEOID: state(2) + county(3) + tract(6)
  state <- substr(tract_geoid, 1, 2)
  county <- substr(tract_geoid, 3, 5)
  tract <- substr(tract_geoid, 6, 11)

  if (nchar(tract) < 6) tract <- sprintf("%06s", tract)

  base <- sprintf("https://api.census.gov/data/%s/acs/acs5", as.character(year))
  # Request total population (B01003_001) and NAME
  query <- list(get = "B01003_001,NAME", `for` = paste0("tract:", tract), `in` = paste0("state:", state, "+county:", county))
  if (nzchar(key)) query$key <- key

  resp <- tryCatch({
    httr::GET(base, query = query, httr::timeout(15))
  }, error = function(e) {
    list(status = "error", message = paste0("HTTP error: ", e$message))
  })
  if (is.list(resp) && !inherits(resp, "response")) return(resp)
  if (httr::status_code(resp) != 200) return(list(status = "error", message = paste0("Census API status: ", httr::status_code(resp))))

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch({
    jsonlite::fromJSON(txt, simplifyVector = TRUE)
  }, error = function(e) {
    NULL
  })
  if (is.null(parsed) || length(parsed) < 2) return(list(status = "error", message = "Census API returned no data"))

  # parsed is a matrix-like: first row header, second row values
  hdr <- parsed[1, ]
  vals <- parsed[2, ]
  # Find B01003_001 and NAME
  pop_idx <- which(hdr == "B01003_001")
  name_idx <- which(hdr == "NAME")
  pop <- if (length(pop_idx) > 0) as.numeric(vals[[pop_idx]]) else NA
  name <- if (length(name_idx) > 0) as.character(vals[[name_idx]]) else NA

  return(list(status = "ok", population = pop, name = name, state = state, county = county, tract = tract))
}
