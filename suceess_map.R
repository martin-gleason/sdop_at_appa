
# Libraries ---------------------------------------------------------------
library(here)
library(leaflet)
library(leaflet)
library(sf)
library(RColorBrewer)

map_data <- read_rds(here("inputs", "map_data.RDS"))


# Load Data ---------------------------------------------------------------


data_pal <- colorFactor(palette = "magma", domain = map_data$zip)

# Create a colorblind-safe palette for success rates
success_pal <- colorNumeric(
  palette = "RdYlBu", # Red-Yellow-Blue (colorblind safe)
  domain = c(0, 1),
  reverse = TRUE # Red = low success, Blue = high success
)

# Create the leaflet map
success_map <- leaflet(map_data) |>
  addTiles() |>
  
  # Add the polygons with success rate coloring
  addPolygons(
    fillColor = ~ ifelse(
      is.na(success_rate),
      "#808080",
      success_pal(success_rate)
    ),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    
    # Interactive features
    highlight = highlightOptions(
      weight = 4,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.9,
      bringToFront = TRUE
    ),
    
    # Popup information
    popup = ~ paste0(
      "<strong>Zip Code: </strong>",
      zip,
      "<br/>",
      "<strong>Total Cases: </strong>",
      ifelse(is.na(total_cases), "No data", total_cases),
      "<br/>",
      "<strong>Success Rate: </strong>",
      ifelse(
        is.na(success_rate),
        "No data",
        paste0(round(success_rate * 100, 1), "%")
      )
    ),
    
    # Hover labels
    label = ~ paste(
      "Zip:",
      zip,
      "| Success Rate:",
      ifelse(
        is.na(success_rate),
        "No data",
        paste0(round(success_rate * 100, 1), "%")
      )
    ),
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto"
    )
  ) |>
  
  # Add legend
  addLegend(
    pal = success_pal,
    values = ~success_rate,
    opacity = 0.7,
    title = "Success Rate",
    position = "bottomright",
    labFormat = labelFormat(suffix = "%", transform = function(x) x * 100),
    na.label = "No Data"
  ) |>
  
  # Set view to Chicago
  setView(lng = -87.65, lat = 41.85, zoom = 11) |>
  
  # Add a title
  addControl(
    html = "<h3>Cook County Probation Success Rates by Zip Code, Jan 2025 to June 2025</h3>",
    position = "topright"
  )

# Display the map
success_map
