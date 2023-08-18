## Make maps
library(readxl)
library(sf)
library(rgdal)
library(leaflet)
library(tidyverse)

# Inlezen data
rat_res <- read_xlsx("./data/2013 - 2019 BMK.xlsx")
rat_spat <- SpatialPointsDataFrame(coords = rat_res[,c(6,7)],
                       data = rat_res[,-c(6,7)],
                       proj4string = CRS("+init=epsg:31370"))
rat_spat <- st_as_sf(rat_spat)
rat_spat <- st_transform(rat_spat, 4326)
rat_spat <- st_jitter(rat_spat, factor = 0.01)


DSN <- "./data/Wsbekken.shp"
Shape_bekken <- readOGR(dsn = DSN, layer = "Wsbekken")
Shape_bekken <- st_as_sf(Shape_bekken)
bekken <- st_transform(Shape_bekken, 4326)

pal <- colorFactor(c("deeppink", "red", "grey", "khaki1", "orange",
                     "turquoise1", "blue", "yellow", "purple"),
                   levels = c("M1W", "M1M1", "WW", "M3W", "M1M3",
                     "M2W", "M2M2", "M3M3", "M1M2"))
jaren <- as.character(unique(rat_spat$jaar))

kaart <- leaflet(bekken, 
        options = leafletOptions(minZoom = 7)) %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolygons(fillColor = "white", 
              color = "steelblue", 
              weight = 2, 
              fillOpacity = 0.5)
for(j in jaren){
  d = rat_spat[rat_spat$jaar == j,]
  kaart = kaart %>% 
    addCircleMarkers(data = d,
                     fillColor = ~pal(rat_spat$mutatie),
                     fillOpacity = 1,
                     radius = ~ifelse(rat_spat$mutatie == "WW", 3, 5),
                     group = j,
                     stroke = FALSE)
}

kaart %>% 
  addLayersControl(overlayGroups = jaren) %>% 
  addLegend("bottomleft",
            pal = pal,
            values = ~rat_spat$mutatie,
            title = "Mutatie") %>% 
  setView(4.2813167, 50.76, zoom = 8)

