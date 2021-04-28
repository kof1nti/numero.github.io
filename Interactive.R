library(tidyverse)
library(leaflet)
library(sp)
library(rgdal)
library(sf)
library(ggplotlyExtra)
library(mapview)
library(webshot)
webshot::install_phantomjs()



leaflet####

vis = readOGR("poscds.shp")
proj4string(vis) <- CRS("+init=epsg:27700")
vis <- spTransform(vis,  CRS("+init=epsg:4326") )



vissf <- st_as_sf(vis)
st_crs(vissf) = "+init=epsg:3857"
vissftrans <- st_transform(vissf, crs = 4326)
class(vissftrans)
st_crs(vissftrans)
p@data

vis %>% 
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(label = vis$Ward, 
             popup = ifelse(vis$Postal.Sec=="Yes",
                            "real", 
                            "Not real")) %>%
  setView(lat = 69.4721754741923, lng = -171.460627228886, zoom = 10)%>%
  addMiniMap(
    toggleDisplay = TRUE,
    tiles = providers$Stamen.TonerLite
  ) 

m = mapview(vis)
mapshot(m, url = paste0(getwd(), "/map.html"))

webshot::install_phantomjs()

saveWidget(m, "map.html", selfcontained = FALSE)
webshot("map.html", file = "vis.png",
        cliprect = "viewport")


library(seleniumPipes)
library(purrr)
library(devd)

## 'leaflet' objects (image above)
t <- leaflet() %>% addTiles()
mapshot(t, file = "~/Rplot.png")

## 'mapview' objects (image below)
t2 <- mapview(vis)
mapshot(t2, file = "~/vis.png")


####stplanr

library(osrm)
library(stplanr)

trip <- route(
  from = c(-0.11, 51.514),
  to = c(-0.10, 51.506),
  route_fun = osrmRoute,
  returnclass = "sf"
)
mapview::mapview(trip)


trip2 <- route(
  from = "Leeds",
  to = "Bradford",
  route_fun = osrmRoute,
  returnclass = "sf"
)

mapview::mapview(trip2)

desire_lines <- travel_network[2:6, ]


routes <- route(
  l = desire_lines,
  route_fun = osrmRoute,
  returnclass = "sf"
)
mapview::mapview(routes) +
  mapview::mapview(desire_lines, color = "red")
#######################
vis <- as.data.frame(vis)

mapshot(m, url = paste0(getwd(), "/map.html"))#####also creates html
mapshot(m, url = "map.html",file = paste0(getwd(), "/map.png"))####does not
mapshot(m, file = paste0(getwd(), "/map.png"),
        remove_controls = c("homeButton", "layersControl"))####does not

mapshot(m, url = paste0(getwd(), "/map.html"), ###creates html 
        file = paste0(getwd(), "/map.png"))   ####does not






server <- function(input, output, session) {
  
  map <- reactiveValues(dat = 0)
  
  output$map <- renderLeaflet({
    map$dat <- leaflet() %>% 
      addTiles()
  })
  
  output$vis <- downloadHandler(
    filename = "map.png",
    
    content = function(file) {
      mapshot(map$dat, file = file)
    }
  )
}

## create standalone .html
mapshot(m, url = paste0(getwd(), "/map.html"))

## create standalone .png; temporary .html is removed automatically unless
## 'remove_url = FALSE' is specified
mapshot(m, file = paste0(getwd(), "/map.png"))
mapshot(m, file = paste0(getwd(), "/map.png"),
        remove_controls = c("homeButton", "layersControl"))

## create .html and .png
mapshot(m, url = paste0(getwd(), "/map.html"),
        file = paste0(getwd(), "/map.png"))

## End(Not run)



library(plotly)
library(readr)
library(dplyr)

vis = readOGR("poscds.shp")
vis <- as.data.frame(vis)
vis_chart <- ggplot(vis, aes(x = Ward, 
                             y = Postal.Sec,
                             text = paste0("<b>Boro: </b>", Borough.Di,"<br>",
                                           "<b>Sec: </b>", Postal.Sec,"<br>",
                                           "<b>wd: </b>", Ward),
                             group = 1)) +
  xlab("Boro score") + 
  ylab("Sec score") +
  theme_minimal(base_size = 14, base_family = "London") +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)


vis_interactive <- ggplotly(vis_chart, tooltip = "text") %>%
  config(displayModeBar = FALSE)

print(vis_interactive)
 
############################
leaflet() %>% 
  setView(lng = -171.460627228886, lat = 69.4721754741923, zoom = 11) %>%
  addTiles()  

leaflet() %>% 
  setView(lng = -122.2705383, lat = 37.8698807, zoom = 11) %>%
  addTiles()

leaflet() %>%
  setView(lng = -122.2705383, lat = 37.8698807, zoom = 11) %>%
  addProviderTiles("CartoDB.Positron")

m <- leaflet() %>% addTiles()
mapshot(m, file = "~/map.png")


m2 <- mapview(vis)
mapshot(m2, file = "~/vis.png")

title: "vis Document"
output: 
  html_document:
  self_contained: no


title: "Test Document"
output: map

library(png)
