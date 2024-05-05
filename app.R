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
library(ggplot2)
library(ggthemes)
library(reshape2)
library(stringr)


############################## Data loading ###################################
# Leaflet graph data
wi_hospitals = read_sf("./data/Wisconsin_Hospitals.geojson")
crital_acccess = read_sf("./data/Wisconsin_Critical_Access_Hospitals.geojson")
counties = read_sf("./data/County_Boundaries_24K.geojson")
# Static Graph Data
hospital_charges_data <- read.csv("./data/inpatientCharges.csv")
# 
wi_counties <- read_sf("./data/wi_counties_geojson.geojson")
icu <- read.csv('./data/icu_beds.csv')


####################### ICU Bed Data Manipulation #############################

icu_wisc = icu %>%
  filter(State == "Wisconsin") %>%
  filter(ICU.Beds > 0) %>%
  na.omit()

icu_wisc <- icu_wisc %>%
  rename(county_nam = County) 

merged_data <- wi_counties %>%
  left_join(icu_wisc, by = "county_nam")

merged_data <- merged_data %>%
  mutate(ICU_Beds_Per_10000 = (ICU.Beds / Total.Population) * 10000)

###############################################################################

wisconsin_hospital_data <- hospital_charges_data %>%
  filter(`Provider.State` == "WI") %>%
  mutate(
    `Average Covered Charges` = as.numeric(str_remove_all(`Average.Covered.Charges`, "\\$|,")),
    `Average Total Payments` = as.numeric(str_remove_all(`Average.Total.Payments`, "\\$|,")),
    `Average Medicare Payments` = as.numeric(str_remove_all(`Average.Medicare.Payments`, "\\$|,"))
  ) %>%
  group_by(`Provider.City`) %>%
  summarise(
    `Average Covered Charges` = mean(`Average Covered Charges`, na.rm = TRUE),
    `Average Total Payments` = mean(`Average Total Payments`, na.rm = TRUE),
    `Average Medicare Payments` = mean(`Average Medicare Payments`, na.rm = TRUE)
  )

wisconsin_hospital_data_long <- wisconsin_hospital_data %>%
  pivot_longer(
    cols = c("Average Covered Charges", "Average Total Payments", "Average Medicare Payments"),
    names_to = "Measurement",
    values_to = "Value"
  )


##############################################################################


plot_charges = function(data) {
  ggplot(data, aes(x = `Provider.City`, y = Value, fill = Measurement)) +
    geom_col(position = position_dodge()) +
    labs(
      x = "Provider City (Wisconsin)",
      y = "Average Cost",
      fill = "Type"
    ) +
    scale_fill_manual(values = c("Average Covered Charges" = "blue", "Average Total Payments" = "red", "Average Medicare Payments" = "green")) +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      legend.position = "top",
      panel.grid.major.x = element_line(size = 0.5, linetype = "dashed"),
      panel.grid.major.y = element_line(size = 0.5, linetype = "dashed")
    )
}


ui <- fluidPage(
  titlePanel(
    h1("Wisconsin Hospital Information", style={'color:white; 
                                              background-color:#77C3EC;
                                              border-radius:5px;
                                              padding:5px'})
  ),
  leafletOutput("map"),
  verbatimTextOutput("hospital_info"),
  h3("Average Healthcare Cost by Provider City in Wisconsin", style={'color:white; 
                                              background-color:#77C3EC;
                                              border-radius:5px;
                                              padding:5px'}),
  plotOutput("charges_graph", brush = brushOpts(id = "plot_brush", direction="x")),
  fluidRow(
    column(6,
           h3("ICU Beds per County", style={'color:white; 
                                              background-color:#77C3EC;
                                              border-radius:5px;
                                              padding:5px'}),
           plotOutput("icu_graph")  
           ),
    column(6,
           h3("Average Healthcare Cost", style={'color:white; 
                                              background-color:#77C3EC;
                                              border-radius:5px;
                                              padding:5px'}),
           plotOutput("single_charges"),
           )
  )
)


server <- function(input, output, session) {
  
  selectedHospital <- reactiveVal(NULL)
  
  selected_city <- reactiveVal(NULL)
  
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      data <- brushedPoints(wisconsin_hospital_data_long, brush)
      if (nrow(data) > 0) {
        selected_city(unique(data$Provider.City))
      }
    }
  })
  
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
        info <- as.data.frame(hospital[, c("NAME", "ADDRESS", "CITY", "STATE", "ZIP", "COUNTY", "TYPE_DESC", "SUB_TYPE_DESC")])
        print(info$NAME)
      } else {
        print("No details available.")
      }
    } else {
      print("None Selected")
    }
  })
  
  output$icu_graph = renderPlot({
    ggplot(data = merged_data) +
      geom_sf(aes(fill = ICU_Beds_Per_10000), color = "white") +
      scale_fill_viridis_c(name = "Beds Scale", option = "plasma", direction = 1) +
      labs(subtitle="Number of ICU beds per 10,000 people") +
      theme_minimal() +
      # theme(legend.position = c(0.65, 0.9),
      theme(legend.position = c(0.9, 0.85),
            plot.title = element_text(hjust = 0.5),
            plot.subtitle = element_text(hjust = 0.5))
  })
  
  output$charges_graph = renderPlot({
    plot_charges(wisconsin_hospital_data_long)
  })
  
  output$single_charges = renderPlot({
    if (!is.null(selected_city())) {

      charges = wisconsin_hospital_data_long |>
        filter(Provider.City %in% selected_city())
      
      plot_charges(charges)
    }
    else{
      charges = wisconsin_hospital_data_long |>
        filter(Provider.City == "APPLETON")
      
      plot_charges(charges)
    }
  })
  
}


shinyApp(ui, server)
