# app.R - standalone Shiny app that geocodes an address, shows it on a leaflet map,
# displays lat/lon, matched address, and a small Census tract population (if available).

library(shiny)
library(leaflet)

# Load geocode and Census helper
source("R/helpers.R")

ui <- fluidPage(
  titlePanel("Address → Map with info"),
  sidebarLayout(
    sidebarPanel(
      textInput("address", "Type an address (one line):", value = "1600 Pennsylvania Ave NW, Washington, DC 20500"),
      actionButton("go", "Locate address"),
      br(), br(),
      tags$b("Parsed location"),
      br(),
      verbatimTextOutput("latlon", placeholder = TRUE),
      verbatimTextOutput("matched", placeholder = TRUE),
      verbatimTextOutput("tract_info", placeholder = TRUE),
      br(),
      tags$small("Debug log:"),
      verbatimTextOutput("debug", placeholder = TRUE)
    ),
    mainPanel(
      leafletOutput("map", height = "600px")
    )
  )
)

server <- function(input, output, session) {

  # simple in-memory debug messages
  msgs <- reactiveVal(character())
  log <- function(...) {
    new <- paste0(Sys.time(), " - ", paste0(..., collapse = " "))
    msgs(c(msgs(), new))
    message(new)
  }

  output$debug <- renderText({ paste(msgs(), collapse = "\n") })

  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>% setView(lng = -98.5795, lat = 39.8283, zoom = 4)
  })

  # display parsed lat/lon and matched address
  latlon_val <- reactiveVal(NULL)
  matched_val <- reactiveVal(NULL)
  tract_val <- reactiveVal(NULL)

  output$latlon <- renderText({
    v <- latlon_val()
    if (is.null(v)) return("lat: -, lon: -")
    paste0("lat: ", v$lat, " , lon: ", v$lon)
  })
  output$matched <- renderText({
    v <- matched_val()
    if (is.null(v)) return("matched: -")
    paste0("matched: ", v)
  })
  output$tract_info <- renderText({
    v <- tract_val()
    if (is.null(v)) return("tract: -")
    if (!is.null(v$status) && v$status == "ok") {
      paste0("Tract GEOID: ", v$geoid, "\nPopulation (ACS5): ", v$population, "\nName: ", v$name)
    } else if (!is.null(v$status) && v$status == "no_key") {
      paste0("Tract GEOID: ", v$geoid, " (Census key not set; population not fetched)")
    } else {
      paste0("Tract lookup: ", v$message)
    }
  })

  # Nominatim fallback
  nominatim_one <- function(address) {
    url <- "https://nominatim.openstreetmap.org/search"
    res <- tryCatch(
      httr::GET(url, query = list(q = address, format = "json", limit = 1), httr::user_agent("TommyBoy-App/1.0"), httr::timeout(10)),
      error = function(e) NULL
    )
    if (is.null(res) || httr::status_code(res) != 200) return(NULL)
    parsed <- httr::content(res, as = "parsed")
    if (length(parsed) == 0) return(NULL)
    d <- parsed[[1]]
    list(lat = as.numeric(d$lat), lon = as.numeric(d$lon), label = d$display_name, raw = d)
  }

  # Extract tract GEOID from the 'raw' match object returned by Census geocoder
  extract_tract_geoid <- function(raw_match) {
    if (is.null(raw_match)) return(NULL)
    geogs <- raw_match$geographies
    if (!is.null(geogs) && !is.null(geogs$`Census Tracts`) && length(geogs$`Census Tracts`) > 0) {
      ct <- geogs$`Census Tracts`[[1]]
      if (!is.null(ct$GEOID)) return(ct$GEOID)
    }
    return(NULL)
  }

  observeEvent(input$go, {
    addr <- trimws(input$address)
    log("Locate button clicked. Address:", addr)
    if (addr == "" || is.null(addr)) { log("Empty address"); showNotification("Please enter an address.", type = "warning"); return() }

    # Primary: use Census geocoder
    res_obj <- NULL
    res_obj <- tryCatch(geocode_census_one(addr), error = function(e) { log("geocode_census_one error:", e$message); list(status = "error", message = e$message) })

    if (!is.null(res_obj) && is.list(res_obj) && !is.null(res_obj$status) && res_obj$status == "ok") {
      # success
      log(paste0("Census geocoder returned:lat=", res_obj$lat, " lon=", res_obj$lon, " label=", res_obj$label))
      latlon_val(list(lat = res_obj$lat, lon = res_obj$lon))
      matched_val(res_obj$label)

      # Update map
      leafletProxy("map") %>% clearMarkers() %>% addMarkers(lng = res_obj$lon, lat = res_obj$lat, popup = res_obj$label) %>% setView(lng = res_obj$lon, lat = res_obj$lat, zoom = 16)

      # Try to extract tract GEOID and fetch population if possible
      tract_geoid <- extract_tract_geoid(res_obj$raw)
      if (!is.null(tract_geoid)) {
        log(paste0("Found tract GEOID: ", tract_geoid))
        # Try to fetch population via Census API if key is present
        key <- Sys.getenv("CENSUS_API_KEY", "")
        if (nzchar(key)) {
          pop_res <- fetch_acs_population(tract_geoid, year = 2021, key = key)
          if (!is.null(pop_res) && is.list(pop_res) && identical(pop_res$status, "ok")) {
            tract_val(list(status = "ok", geoid = tract_geoid, population = pop_res$population, name = pop_res$name))
          } else {
            tract_val(list(status = "error", message = if (!is.null(pop_res$message)) pop_res$message else "Census fetch failed", geoid = tract_geoid))
          }
        } else {
          tract_val(list(status = "no_key", geoid = tract_geoid))
        }
      } else {
        tract_val(list(status = "nomatch", message = "No tract GEOID in geocoder response"))
      }

      showNotification("Location found and map updated.", type = "message")
      return()
    }

    # If Census geocoder didn't return ok, try Nominatim fallback
    log("Census geocoder did not return ok; attempting Nominatim fallback.")
    res_nom <- nominatim_one(addr)
    if (!is.null(res_nom)) {
      log(paste0("Nominatim returned lat=", res_nom$lat, " lon=", res_nom$lon, " label=", res_nom$label))
      latlon_val(list(lat = res_nom$lat, lon = res_nom$lon))
      matched_val(res_nom$label)
      tract_val(NULL)
      leafletProxy("map") %>% clearMarkers() %>% addMarkers(lng = res_nom$lon, lat = res_nom$lat, popup = res_nom$label) %>% setView(lng = res_nom$lon, lat = res_nom$lat, zoom = 16)
      showNotification("Location found (fallback) and map updated.", type = "message")
      return()
    }

    log("All geocoding attempts failed.")
    showNotification("No geocode result.", type = "warning")
  })

  # initial message
  observe({
    if (length(msgs()) == 0) {
      if (exists("geocode_census_one")) log("App started: geocode_census_one() available.") else log("App started: geocode_census_one() NOT available.")
    }
  })
}

shinyApp(ui, server)
