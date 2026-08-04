# Robust geocode helper: returns a list with status, lat, lon, label, raw, message
library(httr)
library(jsonlite)

geocode_census_one <- function(address, benchmark = "Public_AR_Current", timeout_seconds = 10) {
  if (missing(address) || nchar(trimws(address)) == 0) {
    return(list(status = "error", message = "empty address"))
  }
  
  base <- "https://geocoding.geo.census.gov/geocoder/locations/onelineaddress"
  
  resp <- tryCatch(
    httr::GET(
      url = base,
      query = list(address = address, benchmark = benchmark, format = "json"),
      httr::user_agent("TommyBoy/1.0 (geocode_census_one)"),
      httr::timeout(timeout_seconds)
    ),
    error = function(e) return(list(status = "error", message = paste0("HTTP error: ", e$message)))
  )
  
  if (is.list(resp) && !inherits(resp, "response")) {
    # propagate the error object returned from tryCatch above
    return(resp)
  }
  
  if (httr::status_code(resp) != 200) {
    return(list(status = "error", message = paste0("HTTP status ", httr::status_code(resp))))
  }
  
  txt <- httr::content(resp, as = "text", encoding = "UTF-8")
  parsed <- tryCatch(jsonlite::fromJSON(txt, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(parsed)) return(list(status = "error", message = "JSON parse error"))
  
  matches <- parsed$result$addressMatches
  if (is.null(matches) || length(matches) == 0) {
    return(list(status = "nomatch", message = "no addressMatches returned", raw = parsed$result))
  }
  
  m <- matches[[1]]
  coords <- NULL
  if (!is.null(m$coordinates)) coords <- m$coordinates
  if (is.null(coords) && !is.null(m$coordinates$x) && !is.null(m$coordinates$y)) coords <- list(x = m$coordinates$x, y = m$coordinates$y)
  if (is.null(coords)) return(list(status = "error", message = "match found but coordinates missing", raw = m))
  
  lon <- suppressWarnings(as.numeric(coords$x))
  lat <- suppressWarnings(as.numeric(coords$y))
  if (!is.finite(lat) || !is.finite(lon)) {
    return(list(status = "error", message = "coordinates not numeric/finite", raw = m))
  }
  
  label <- if (!is.null(m$matchedAddress)) m$matchedAddress else address
  list(status = "ok", lat = lat, lon = lon, label = label, raw = m)
}