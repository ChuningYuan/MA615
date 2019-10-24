library(tidyverse)
library(magrittr)
library(readxl)
library(ggmap)
library(shiny)
library(leaflet)
library(maps)
library(htmlwidgets) 

# data = read.csv("tmphbl23vi6.csv")
# 

# 
# data %<>% filter(YEAR == 2019)
# write.csv(data,"data.csv")
library(readr)
data<- read.csv("Charging station.csv")
Charging = data %>% dplyr::select(X, Y,Station_Name,ZIP)
# State boundaries from the maps package. The fill option must be TRUE.
bounds <- map('state', c('Massachusetts'), fill=TRUE, plot=FALSE)
# A custom icon.
icons <- awesomeIcons(
  icon = 'disc',
  iconColor = 'black',
  library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
  markerColor = 'blue',
  squareMarker = TRUE
)
# Create the Leaflet map widget and add some map layers.
# We use the pipe operator %>% to streamline the adding of
# layers to the leaflet object. The pipe operator comes from 
# the magrittr package via the dplyr package.
sub_da = dplyr::filter(Charging,Charging$ZIP == 2109)
map <- leaflet(data = sub_da) %>%
  setView(mean(sub_da$X),mean(sub_da$Y), zoom = 12) %>% 
  addProviderTiles("CartoDB.Positron", group = "Map") %>%
  addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
  addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%
  # Marker data are from the sites data frame. We need the ~ symbols
  # to indicate the columns of the data frame.
  addMarkers(~X, ~Y, label = ~Station_Name, group = "Charging") %>% 
  # addAwesomeMarkers(~lon_dd, ~lat_dd, label = ~locality, group = "Charging",con=icons) %>%
  addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
  addScaleBar(position = "bottomleft") %>%
  addLayersControl(
    baseGroups = c("Map", "Satellite", "Relief"),
    overlayGroups = c("Charging", "States"),
    options = layersControlOptions(collapsed = T)
  )
invisible(print(map))

############################################################################################################################################
############################################################################################################################################
library(shiny)

ui = fluidPage(
  title = "Charging Station Distribution",
  leafletOutput(outputId = "Plot",width = 1600, height = 800),
  
  
  hr(),
  
  fluidRow(
    column(12,
           h4("Select the zipcode"),
           selectInput("zip","ZIP",unique(Charging$ZIP)))
  )
)


server <- function(input, output, session) {
  
  output$Plot = renderLeaflet({
    bounds <- map('state', c('Massachusetts'), fill=TRUE, plot=FALSE)

    icons <- awesomeIcons(
      icon = 'disc',
      iconColor = 'black',
      library = 'ion', # Options are 'glyphicon', 'fa', 'ion'.
      markerColor = 'blue',
      squareMarker = TRUE
    )

    sub_da = dplyr::filter(Charging,Charging$ZIP == input$zip)
    leaflet(data = sub_da) %>%
      setView(mean(sub_da$X),mean(sub_da$Y), zoom = 16) %>% 
      addProviderTiles("CartoDB.Positron", group = "Map") %>%
      addProviderTiles("Esri.WorldImagery", group = "Satellite")%>% 
      addProviderTiles("Esri.WorldShadedRelief", group = "Relief")%>%

      addMarkers(~X, ~Y, label = ~Station_Name, group = "Charging") %>% 

      addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
      addScaleBar(position = "bottomleft") %>%
      addLayersControl(
        baseGroups = c("Map", "Satellite", "Relief"),
        overlayGroups = c("Charging", "States"),
        options = layersControlOptions(collapsed = T)
      )
  })
}

shinyApp(ui = ui, server = server)








