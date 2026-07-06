#########################################################################
#                               A2N40.                                  #
#          an app in R Shiny to visualize hospital accessibility        #
#                                                                       #
#          (c) 2026 Benjamin Voellger, under the MIT license            #
#########################################################################

#load dependencies
library(shiny)            #the stuff underneath
library(sf)               #simple features for R to deal with spatial vector data
library(osmdata)          #open street map
library(mapview)          #more map stuff 
library(leaflet)          #even more map stuff
#library(pheatmap)         #pretty heatmaps
#library(viridis)          #the paint bucket
#library(data.table)       #contains the fread function

#read your data
#hospital coordinates
hospitals <- read.csv("./gis_data/hospitals.csv", sep = ";", dec = ",")

#to download and unpack a .zip container with shapefiles of German administrative units at several levels
#(level 0 - country; level 1 - federal states; level 2 - governmental districts ("Regierungsbezirke"); level 3 - districts ("Landkreise"))
#leave the following two lines uncommented at the first run of the app:
#download.file(url = "http://biogeo.ucdavis.edu/data/diva/adm/DEU_adm.zip", destfile = "./gis_data/germany.zip")
#unzip(zipfile = "./gis_data/germany.zip")

#read the unpacked level 3 (districts) shapefile:

#to calculate travelling time polygons with the help of openrouteservice.org and to save the polygons as shapefiles
#register with openrouteservice.org, request a token there, and
#leave the following three lines uncommented at the first run of the app (modify and repeat ors_isochrones() and saveRDS() calls according to your needs):
#library(openrouteservice)
#yourPolygon <- ors_isochrones(c(yourLongitude, yourLatitude), profile = "driving-car", range = yourDrivingTimeInSeconds, api_key = "yourTokenGoesHere", output = "sf")
#saveRDS(yourPolygon, "yourDestinationFile.shape")

#read previously calculated shapefiles representing travelling times to index and neighbouring hospitals
#40 minutes by car
#travellingTimeCOG2400 <- readRDS("./gis_data/center_of_germany.shape")

#proposed providers
SWL_2400 <- readRDS("./gis_data/SWL_2400.shape")
PF_2400 <- readRDS("./gis_data/PF_2400.shape")

#current providers
DEU_NCH_2400 <- readRDS("./gis_data/DEU_NCH_PB_LU_2400.shape")

#districts with population density
fedstates <- read_sf("./gis_data/VG2500_LAN.shp")
districts <- read_sf("./gis_data/VG2500_KRS.shp")
districts_WGS84 <- st_transform(districts, crs = st_crs(DEU_NCH_2400))
pop_area <- read.csv("./gis_data/DEU_BevKreisebene_2023.csv", sep = ";", dec = ",")
districts_WGS84$pop <- pop_area$pop
districts_WGS84$area <- pop_area$sqkm

#define frontend
ui <- navbarPage(
  fluid = TRUE,
  #set theme
  theme = bslib::bs_theme(bootswatch = "lux"),
  #set page title
  title = div(h6("Access to Neurosurgery")),
  #set tab title
  windowTitle = "Access to Neurosurgical Departments",
  #define menu
  navbarMenu(title = "Menu",
    #catchment area map
    tabPanel("Departments in Germany",
             title = "Departments in Germany",
             #draw map, print legend
             leafletOutput("map", height = 950),
             textOutput("map_legend"),
             tags$head(tags$style("#map_legend{color: black;
                                 font-size: 10px;
                                 font-style: normal;
                                 }"
             )
             )
    )
  )
)

#define backend
server <- function(input, output, session){ 

  #accessibility
  #setup a map, centered at the index hospital, on a slick background, with a metric scalebar
  m <- mapview()@map %>%
    leaflet(districts_WGS84) %>%
    setView(lat= 51.2, lng = 10.5, zoom = 5) %>%
    #addTiles() %>%
    addProviderTiles("CartoDB.Positron") %>%
    addScaleBar(position = "bottomright", options = scaleBarOptions(imperial = FALSE))
  #fill map
  output$map <- renderLeaflet({
    m %>%
      addPolygons(
        group = "Population Density by District",
        data = districts_WGS84,
        color = "#888888", opacity = 1, weight = 0.1, fillOpacity = (sqrt(districts_WGS84$pop/districts_WGS84$area)*0.01)
      ) %>%
      addPolygons(
         group = "Proposed Providers",
         data = SWL_2400,
         color = "#dc267f", opacity = 0.6, weight = 0.3, fillOpacity = 0.3
      ) %>%
      addPolygons(
        group = "Proposed Providers",
        data = PF_2400,
        color = "#dc267f", opacity = 0.6, weight = 0.3, fillOpacity = 0.3
      ) %>%
      addPolygons(
        group = "Grid of Existing Providers",
        data = DEU_NCH_2400,
        color = "#648fff", opacity = 0.6, weight = 0.3, fillOpacity = 0.3
      ) %>%
    #mark hospitals  
    addAwesomeMarkers(group = "Proposed Providers", lat = as.numeric(hospitals[199,5]), lng = as.numeric(hospitals[199,6]), label = hospitals[199,7], icon = awesomeIcons(library = "fa", icon = "hospital-o", iconColor = "black", markerColor = "white")) %>%
    addAwesomeMarkers(group = "Proposed Providers", lat = as.numeric(hospitals[209,5]), lng = as.numeric(hospitals[209,6]), label = hospitals[209,7], icon = awesomeIcons(library = "fa", icon = "hospital-o", iconColor = "black", markerColor = "white")) %>%
    addLayersControl(
      overlayGroups = c("Population Density by District", "Grid of Existing Providers", "Proposed Providers"),#,) "Baden-Wurttemberg", "Bavaria", "Berlin", "Brandenburg", "Bremen", "Hamburg", "Hesse", "Lower Saxony", "Mecklenburg-West Pomerania", "North Rhine-Westphalia", "Rhineland-Palatinate", "Saarland", "Saxony", "Saxony-Anhalt", "Schleswig-Holstein", "Thuringia"),
      options = layersControlOptions(collapsed = TRUE, position = "topright")
    ) %>%
    #place comments after the bracket above and at the beginning of the following three lines to start with excess visuals
    hideGroup("Proposed Providers") %>%
    hideGroup("Population Density by District") #%>%
  })
  #define legend
  output$map_legend <- renderText({
    "Car travelling isochrones (CTI; 40 minutes) to hospitals licensed (blue polygons) or proposed (grey circles, pink polygons) to offer elective neurosurgery services. Expand checkboxes in the upper right corner of the map to toggle layer visibility."
  })
}

#run app
shinyApp(ui, server)