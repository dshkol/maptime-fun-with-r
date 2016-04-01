library(leaflet)
library(dplyr)

## Leaflet section

leaflet() %>% setView(lng = -123.1,lat = 49.27, zoom = 13) %>% addTiles()


# Basemaps - by default Open Street Map

leaflet() %>% setView(lng = -123.1,lat = 49.27, zoom = 13) %>% addProviderTiles("Stamen.Toner")

leaflet() %>% setView(lng = -123.1,lat = 49.27, zoom = 13) %>% addProviderTiles("CartoDB.DarkMatter")

leaflet() %>% setView(lng = -123.1,lat = 49.27, zoom = 13) %>% addProviderTiles("Esri.WorldImagery")

# full list at http://leaflet-extras.github.io/leaflet-providers/preview/index.html



# add markers

leaflet(data = listings) %>% addTiles() %>% addMarkers(~longitude, ~latitude, popup = ~as.character(paste0("<b>Price: $",price,"per night</br>","<b>Type: ",room_type,"</br>")))

# With marker clusters
leaflet(data = listings) %>% addTiles() %>% addMarkers(~longitude, ~latitude, popup = ~as.character(paste0("<b>Price: $",price," per night</br>","<b>Type: ",room_type,"</br>")),clusterOptions = markerClusterOptions())

# Circle markers
pal <- colorFactor(c("red", "navy","green"), domain = c("Entire home/apt","Private room", "Shared room"))

leaflet(listings) %>% addTiles() %>% addCircleMarkers(color = ~pal(room_type), stroke = FALSE, fillOpacity = 0.5, popup = ~as.character(paste0("<b>Price: $",price," per night</br>","<b>Type: ",room_type,"</br>")), clusterOptions = markerClusterOptions())

# Polygons



# Layers https://rstudio.github.io/leaflet/showhide.html