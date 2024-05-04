library(ceramic)
library(spData)
library(raster)
library(sf)
library(terra)
library(tidyverse)
library(tmap)
library(patchwork)
library(shiny)
library(dplyr)
library(leaflet)
theme_set(theme_minimal())


########################## Leaflet data loading ################################
wi_hospitals = read_sf("../data/Wisconsin_Hospitals.geojson")
crital_acccess = read_sf("../data/Wisconsin_Critical_Access_Hospitals.geojson")
counties = read_sf("../data/County_Boundaries_24K.geojson")

####################### Leaflet Data Manipulation #############################
# data(world)
# data(us_states)
# 
# 
# wisco = us_states %>% 
#   filter(NAME == "Wisconsin")
# 
# hospitals = tm_shape(wisco) +
#   tm_borders() +
#   tm_shape(wi_hospitals) +
#   tm_dots(col = "red") 
# 
# critAccess = tm_shape(wisco) +
#   tm_borders() +
#   tm_shape(crital_acccess) +
#   tm_bubbles(col = "blue", size = 0.5, alpha = 0.5)
# 
# county_border = tm_shape(counties) +
#   tm_borders()

# combined_plot = hospitals + critAccess + county_border
# combined_plot


ui <- fluidPage(
  titlePanel("Wisconsin Hospital Information"),
  leafletOutput("map"),
  verbatimTextOutput("hospital_info")
)


server <- function(input, output, session) {
  
  selectedHospital <- reactiveVal(NULL)
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      addPolygons(data = counties, weight = 0.3, fill = FALSE) %>%
      setView(lng = -88.78, lat = 44.78, zoom = 6) %>%
      addCircleMarkers(data = wi_hospitals,
                       radius = ~ifelse(SUB_TYPE_DESC == "CRITICAL ACCESS HOSPITALS", 3, 5),
                       color = ~ifelse(SUB_TYPE_DESC == "CRITICAL ACCESS HOSPITALS", "red", "blue"),
                       label = ~NAME,
                       popup = ~paste("<b>Name:</b>", NAME, 
                                      "<br><b>Address:</b>", ADDRESS, 
                                      "<br><b>City:</b>", CITY, 
                                      "<br><b>State:</b>", STATE, 
                                      "<br><b>Zip:</b>", ZIP, 
                                      "<br><b>County:</b>", COUNTY,
                                      "<br><b>Type:</b>", TYPE_DESC,
                                      "<br><b>Sub Type:</b>", SUB_TYPE_DESC),
                       
                       layerId = ~NAME
      )
  })
  
  observeEvent(input$map_marker_click, {
    selectedHospital(input$map_marker_click$id)
  })
  
  output$hospital_info <- renderPrint({
    if (!is.null(selectedHospital())) {
      hospital <- subset(wi_hospitals, NAME == selectedHospital())
      if (nrow(hospital) > 0) {
        # info <- hospital[, c("NAME", "ADDRESS", "CITY", "STATE", "ZIP", "COUNTY", "TYPE_DESC", "SUB_TYPE_DESC")]
        info <- as.data.frame(hospital[, c("NAME", "ADDRESS", "CITY", "STATE", "ZIP", "COUNTY", "TYPE_DESC", "SUB_TYPE_DESC")])
        print(info$NAME)
      } else {
        print("No details available.")
      }
    } else {
      print("None Selected")
    }
  })
  
}


shinyApp(ui, server)
