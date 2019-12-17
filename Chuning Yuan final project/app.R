#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(readr)
library(leaflet)
library(tidyverse)
facilities <- read.csv("facilities.csv")
unique2 <- read.csv("unique2.csv")
unique2 = unique2 %>% mutate(price.c = as.numeric(as.character(cut(price,breaks = 4,labels = F))))
facilities = facilities %>% filter(jurisdiction %in% c("Recreation And Parks","School District (Sfusd)",
                                                       "War Memorial","Municipal Transportation Agency"))

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    title = "Airbnb SanFransico",
    
    hr(),
    
    fluidRow(
        column(6,
               h4("Airbnb in SanFransico"),
               sliderInput('sampleSize', 'Sample Size', min = 1, max = max(1,nrow(unique2)), value = min(1,nrow(unique2) ),step = 1),
        br(),
        column(7, iffset=1,
               selectInput('neighborhood', 'Area',choices = unique(unique2$neighborhood)),
               selectInput('jurisdiction', 'Facility type',choices=unique(facilities$jurisdiction)),
               sliderInput('Pointsize', 'Select how big each point is', min = 1, max = 10, value = 4,step = 1)
        ),
    leafletOutput("map"),
    ),
),
)
    

# Define server logic required to draw a histogram
server <- function(input, output, session) {

    
    
    output$map = renderLeaflet({

        unique2 = unique2 %>% dplyr::filter(neighborhood %in% input$neighborhood)
        facilities = facilities %>% dplyr::filter(jurisdiction %in% input$jurisdiction)
        pel = colorFactor(palette = "viridis",domain = unique2$price.c)  
        pel2 = colorFactor(palette = "plasma",domain = facilities$jurisdiction)
        unique2 = unique2 %>% sample_n(size = input$sampleSize,replace = T)
        facilities = facilities %>% sample_n(size = input$sampleSize,replace = T)
             
            leaflet(data = unique2) %>% addTiles() %>%
                setView(mean(unique2$lon),mean(unique2$lat), zoom = 12) %>%
                addCircleMarkers(group = "Airbnb",lng = unique2$lon, lat = unique2$lat, label = unique2$price,radius =input$Pointsize,color=pel(unique2$price.c), stroke = FALSE, fillOpacity = 0.8)%>%
                addCircleMarkers(data = facilities,group = "Facility",lng = facilities$longitude, lat = facilities$latitude, label = facilities$jurisdiction,radius =input$Pointsize, stroke = FALSE, fillOpacity = 0.8)%>%
                addScaleBar(position = "bottomleft") %>%
                addLayersControl(
                    overlayGroups = c("Airbnb"),
                    options = layersControlOptions(collapsed = T)
                )
    
            
    })
}
# Run the application 
shinyApp(ui = ui, server = server)
