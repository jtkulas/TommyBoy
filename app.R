# app.R - standalone Shiny app that geocodes an address and shows it on a leaflet map.
# This version uses the robust geocode_census_one() helper and displays debug info in-UI.

library(shiny)
library(leaflet)

# Load the geocode helper
source("R/helpers.R")

ui <- fluidPage(
  titlePanel("Address → Map (test app)"),
  sidebarLayout(
    sidebarPanel(
      textInput("address", "Type an address (one line):", value = "1600 Pennsylvania Ave NW, Washington, DC 20500"),
      actionButton("go", "Locate address"),
      br(),
      tags$small("Geocoding uses the US Census Geocoder (primary) with a Nominatim fallback)."),
      br(),
      br(),
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
  
  # Nominatim fallback (in-case the Census geocoder fails)
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
  
  observeEvent(input$go, {
    addr <- trimws(input$address)
    log("Locate button clicked. Address:", addr)
    if (addr == "" || is.null(addr)) { log("Empty address"); showNotification("Please enter an address.", type = "warning"); return() }
    
    # Primary: use Census geocoder
    res <- NULL
    res <- tryCatch(geocode_census_one(addr), error = function(e) { log("geocode_census_one error:", e$message); NULL })
    if (!is.null(res)) {
      log("Census geocoder returned:", paste0("lat=", res$lat, " lon=", res$lon, " label=", res$label))
    } else {
      log("geocode_census_one() returned NULL; trying Nominatim fallback.")
      res <- nominatim_one(addr)
      if (!is.null(res)) log("Nominatim returned:", paste0("lat=", res$lat, " lon=", res$lon, " label=", res$label))
    }
    
    if (is.null(res)) {
      log("All geocoding attempts failed.")
      showNotification("No geocode result.", type = "warning")
      return()
    }
    
    # Coerce lat/lon to numeric and validate
    lat <- suppressWarnings(as.numeric(res$lat))
    lon <- suppressWarnings(as.numeric(res$lon))
    if (!is.finite(lat) || !is.finite(lon)) {
      # try raw coordinates
      if (!is.null(res$raw) && !is.null(res$raw$coordinates)) {
        lon <- suppressWarnings(as.numeric(res$raw$coordinates$x))
        lat <- suppressWarnings(as.numeric(res$raw$coordinates$y))
      }
    }
    
    if (!is.finite(lat) || !is.finite(lon)) {
      log("Coordinates invalid after parsing.")
      showNotification("Geocoder returned invalid coordinates.", type = "error")
      return()
    }
    
    label <- if (!is.null(res$label)) as.character(res$label) else addr
    leafletProxy("map") %>% clearMarkers() %>% addMarkers(lng = lon, lat = lat, popup = label) %>% setView(lng = lon, lat = lat, zoom = 16)
    log("Map updated.")
    showNotification("Location found and map updated.", type = "message")
  })
  
  # initial message
  observe({
    if (length(msgs()) == 0) {
      if (exists("geocode_census_one")) log("App started: geocode_census_one() available.") else log("App started: geocode_census_one() NOT available.")
    }
  })
}

shinyApp(ui, server)
