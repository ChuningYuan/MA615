data = read.csv("data.csv")
data %<>% arrange(year)
library(magrittr)

n = 2008

sub_data = data %>% filter(year == n) %>% dplyr::select(country,GDP)
# first step
sub_data %<>% arrange(GDP)

# second step
f = 0.75
yuzhi = sum(sub_data$GDP)*f
sub_data["sum"] = cumsum(sub_data$GDP)
new1 = sub_data[which(sub_data$sum < yuzhi),]
new2 = sub_data[which(sub_data$sum > yuzhi),]
new2$country = as.character(new2$country)
new = rbind(new2,c("Other",sum(new1$GDP),sum(new1$GDP)))
new = new[,c("country","GDP")]

f = function(x,fa){
  n = x
  
  sub_data = data %>% filter(year == n) %>% dplyr::select(country,GDP)
  # first step
  sub_data %<>% arrange(GDP)
  
  # second step
  f = fa
  yuzhi = sum(sub_data$GDP)*f
  sub_data["sum"] = cumsum(sub_data$GDP)
  new1 = sub_data[which(sub_data$sum < yuzhi),]
  new2 = sub_data[which(sub_data$sum > yuzhi),]
  new2$country = as.character(new2$country)
  new = rbind(new2,c("Other",sum(new1$GDP),sum(new1$GDP)))
  new = new[,c("country","GDP")]
  #ggplot(new) + geom_bar(aes(x = "",y = GDP,fill = country),stat = "identity",position = "stack")+
    #coord_polar("y") + theme(axis.text = element_blank())+ theme(axis.ticks = element_blank())
  new$GDP = as.numeric(new$GDP)
  new["Proportion"] = paste(round(new$GDP / sum(new$GDP)*100,2),"%")
  return(new)
  }

ye = 2008
ggplot(f(ye)) + geom_bar(aes(x = "",y = GDP,fill = country),stat = "identity",position = "stack")+
  coord_polar("y") + theme(axis.text = element_blank())+ theme(axis.ticks = element_blank()) 

######################################################################
library(shiny)


ui <- fluidPage(
  
  # Application title
  titlePanel("GDP Proportion"),
   
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("year", "Select year:", sort(unique(data$year))),
      selectInput("fa","Select a Number", seq(0,1,0.05))
      # varSelectInput("V1", "Select a Variable", da[2:215]),
      # varSelectInput("V2", "Select a Variable", da[2:215])
    ),
    
    mainPanel(
      tableOutput("table"),
      plotOutput("Plot")
      #verbatimTextOutput("info")
    )
  )
)


server <- function(input, output, session) {
  
  output$Plot = renderPlot({
    fa = as.numeric(input$fa)
    dai = f(input$year,fa)
    ggplot(dai) + geom_bar(aes(x = "",y = GDP,fill = country),stat = "identity",position = "stack")+
      coord_polar("y") + theme(axis.text = element_blank())+ theme(axis.ticks = element_blank())
  })
  # 
  output$table = renderTable({
    fa = as.numeric(input$fa)
    dai = f(input$year,fa)
    dai
  })
  
  
}

shinyApp(ui = ui, server = server)


