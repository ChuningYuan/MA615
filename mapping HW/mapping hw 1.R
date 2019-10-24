library(shiny)
library(ggmap)
library(maptools)
library(maps)
library(mapproj)
mapWorld <- map_data("world")

ui <- fluidPage(
  selectInput("projection", "Projection",choices = c("mercator","cylindrical",
                                                     "sinusoidal","gnomonic")),
  mainPanel(plotOutput("plot"),width = 800, height = 800)
)

server <- function(input, output, session){
  output$plot<-renderPlot({
    ggplot(mapWorld, aes(x=long, y=lat, group=group))+
      geom_polygon(fill="white", color="black") +
      coord_map(input$projection,xlim=c(-180,180), ylim=c(-60, 90))})
}
shinyApp(ui, server)

