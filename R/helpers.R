# Helper functions for geocoding and simple Census API access
# Keep lightweight: only depend on httr and jsonlite to avoid heavy system deps

library(httr)
library(jsonlite)

# geocode_census_one: calls US Census onelineaddress geocoder and returns structured result
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
  parsed <- tryCatch({ jsonlite::fromJSON(txt, simplifyVector = FALSE) }, error = function(e) NULL)
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
  list(status = "ok", lat = lat, lon = lon, label = label, raw = m)
}

# geographies_for_coords: call Census coordinates->geographies endpoint to get tract GEOID etc.
# Returns list(status='ok', geographies=...) or structured error/nomatch
geographies_for_coords <- function(lat, lon, benchmark = "Public_AR_Census2020", vintage = "Census2020_Census2020", timeout_seconds = 10) {
  if (missing(lat) || missing(lon) || !is.finite(lat) || !is.finite(lon)) return(list(status = "error", message = "invalid lat/lon"))
  base <- "https://geocoding.geo.census.gov/geocoder/geographies/coordinates"

  resp <- tryCatch({
    httr::GET(
      url = base,
      query = list(x = lon, y = lat, benchmark = benchmark, vintage = vintage, format = "json"),
      httr::user_agent("TommyBoy/1.0 (geographies_for_coords)"),
      httr::timeout(timeout_seconds)
    )
  }, error = function(e) {
    list(status = "error", message = paste0("HTTP error: ", e$message))
  })

  if (is.list(resp) && !inherits(resp, "response")) return(resp)
  if (httr::status_code(resp) != 200) {
    body <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
    return(list(status = "error", message = paste0("HTTP status ", httr::status_code(resp), "; body: ", body)))
  }

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch({ jsonlite::fromJSON(txt, simplifyVector = FALSE) }, error = function(e) NULL)
  if (is.null(parsed)) return(list(status = "error", message = "JSON parse error"))

  geogs <- parsed$result$geographies
  if (is.null(geogs) || length(geogs) == 0) return(list(status = "nomatch", message = "no geographies returned", raw = parsed$result))

  list(status = "ok", geographies = geogs)
}

# fetch_acs_population: given 11-digit tract GEOID, query Census API for total population and NAME
# Returns list(status='ok', population=..., name=...) or structured error
fetch_acs_population <- function(tract_geoid, year = 2021, key = Sys.getenv("CENSUS_API_KEY", "")) {
  if (missing(tract_geoid) || is.null(tract_geoid) || nchar(trimws(tract_geoid)) < 11) return(list(status = "error", message = "invalid tract GEOID"))

  state <- substr(tract_geoid, 1, 2)
  county <- substr(tract_geoid, 3, 5)
  tract <- substr(tract_geoid, 6, 11)
  if (nchar(tract) < 6) tract <- sprintf("%06s", tract)

  base <- sprintf("https://api.census.gov/data/%s/acs/acs5", as.character(year))

  # Try with the estimate variable (B01003_001E) which the ACS API accepts
  query <- list(get = "B01003_001E,NAME")
  query[["for"]] <- paste0("tract:", tract)
  query[["in"]] <- paste0("state:", state, "+county:", county)
  if (nzchar(key)) query$key <- key

  resp <- tryCatch({ httr::GET(base, query = query, httr::timeout(15)) }, error = function(e) list(status = "error", message = paste0("HTTP error: ", e$message)))
  if (is.list(resp) && !inherits(resp, "response")) return(resp)

  status_code <- httr::status_code(resp)
  if (status_code != 200) {
    # capture body for diagnostics
    body <- tryCatch(httr::content(resp, as = "text", encoding = "UTF-8"), error = function(e) "")
    # If variable unknown, try fallback to B01003_001 (non-E)
    if (grepl("unknown variable", body, ignore.case = TRUE)) {
      query2 <- list(get = "B01003_001,NAME")
      query2[["for"]] <- paste0("tract:", tract)
      query2[["in"]] <- paste0("state:", state, "+county:", county)
      if (nzchar(key)) query2$key <- key
      resp2 <- tryCatch({ httr::GET(base, query = query2, httr::timeout(15)) }, error = function(e) list(status = "error", message = paste0("HTTP error: ", e$message)))
      if (is.list(resp2) && !inherits(resp2, "response")) return(resp2)
      if (httr::status_code(resp2) == 200) {
        txt2 <- httr::content(resp2, as = "text", encoding = "UTF-8")
        parsed2 <- tryCatch({ jsonlite::fromJSON(txt2, simplifyVector = TRUE) }, error = function(e) NULL)
        if (!is.null(parsed2) && length(parsed2) >= 2) {
          hdr2 <- parsed2[1, ]
          vals2 <- parsed2[2, ]
          pop_idx2 <- which(hdr2 %in% c("B01003_001E", "B01003_001"))
          name_idx2 <- which(hdr2 == "NAME")
          pop2 <- if (length(pop_idx2) > 0) as.numeric(vals2[[pop_idx2]]) else NA
          name2 <- if (length(name_idx2) > 0) as.character(vals2[[name_idx2]]) else NA
          return(list(status = "ok", population = pop2, name = name2, state = state, county = county, tract = tract))
        }
      } else {
        body2 <- tryCatch(httr::content(resp2, as = "text", encoding = "UTF-8"), error = function(e) "")
        return(list(status = "error", message = paste0("Census API status: ", httr::status_code(resp2), "; body: ", body2)))
      }
    }

    # If 400 and no key, give helpful message
    if (status_code == 400 && !nzchar(key)) return(list(status = "error", message = paste0("Census API returned status 400. The Census API may require an API key for this endpoint. Set CENSUS_API_KEY in your environment. Body: ", body)))

    return(list(status = "error", message = paste0("Census API status: ", status_code, "; body: ", body)))
  }

  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch({ jsonlite::fromJSON(txt, simplifyVector = TRUE) }, error = function(e) NULL)
  if (is.null(parsed) || length(parsed) < 2) return(list(status = "error", message = "Census API returned no data"))

  hdr <- parsed[1, ]
  vals <- parsed[2, ]
  pop_idx <- which(hdr %in% c("B01003_001E", "B01003_001"))
  name_idx <- which(hdr == "NAME")
  pop <- if (length(pop_idx) > 0) as.numeric(vals[[pop_idx]]) else NA
  name <- if (length(name_idx) > 0) as.character(vals[[name_idx]]) else NA

  list(status = "ok", population = pop, name = name, state = state, county = county, tract = tract)
}
