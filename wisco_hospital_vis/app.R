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
data(world)
data(us_states)


wisco = us_states %>% 
  filter(NAME == "Wisconsin")

hospitals = tm_shape(wisco) +
  tm_borders() +
  tm_shape(wi_hospitals) +
  tm_dots(col = "red") 

critAccess = tm_shape(wisco) +
  tm_borders() +
  tm_shape(crital_acccess) +
  tm_bubbles(col = "blue", size = 0.5, alpha = 0.5)

county_border = tm_shape(counties) +
  tm_borders()


combined_plot = hospitals + critAccess + county_border
# combined_plot



