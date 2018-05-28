## SEATTLE PD DATA REPORT ##

## Wasif Siddique
## Jeremy Chang
## Rey Matsunaga
## Israel Sixto-Sanchez

library(dplyr)
library(ggplot2)
library(tidyverse)
library(shiny)
library(httr)
library(jsonlite)
library(leaflet)
library(leaflet.extras)

#####################
######## API ########
#####################


data <- read.csv("./data/Seattle_police_data_2017.csv",na.strings = "NA",
                   stringsAsFactors = FALSE, fill = TRUE, header = TRUE)
offense_type <- unique(data$Summarized.Offense.Description)
month <- c("January", "February", "March", "April", "May", "June", "July",
           "August","September", "October", "November", "December")


## we're only doing records for 2017

 
ui <- fluidPage(
  titlePanel("Seattle PD data report 2017"),
  
  tabsetPanel(
    tabPanel("HeatMap", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
        # State Widget
          selectInput("crime_type", "Offense Type", choices = offense_type, selected = ""),
          selectInput("months", "Month", choices = month, selected = "")
      ),
        mainPanel(
          leafletOutput("heatmap"),
          p()
        )
      )
    ),
    tabPanel("Jeremy", fluid = TRUE,
      sidebarLayout(   # layout the page in two columns
        sidebarPanel(  # specify content for the "sidebar" column
          p("sidebar panel content goes here")
        ),
        mainPanel(     # specify content for the "main" column
          p("main panel content goes here")
        )
      )
    ),
    tabPanel("Rey" , fluid = TRUE,
      sidebarLayout(   # layout the page in two columns
        sidebarPanel(  # specify content for the "sidebar" column
          p("sidebar panel content goes here")
        ),
        mainPanel(     # specify content for the "main" column
          p("main panel content goes here")
        )
      )
    ),
    tabPanel("Israel", fluid = TRUE,
      sidebarLayout(   # layout the page in two columns
        sidebarPanel(  # specify content for the "sidebar" column
          p("sidebar panel content goes here")
        ),
        mainPanel(     # specify content for the "main" column
          p("main panel content goes here")
        )
      )
    )
  )
)

server <- function(input,output) {
  
  select_month <- reactive({
    text <- input$months[1]
    if (text == "January" ){
      return(1)
    } else if (text == "February"){
      return(2)
    } else if (text == "March") {
      return(3)
    } else if (text == "April") {
      return(4)
    } else if (text == "May") {
      return(5)
    } else if (text == "June") {
      return(6)
    } else if (text == "July") {
      return(7)
    } else if (text == "August") {
      return(8)
    } else if (text == "September") {
      return(9)
    } else if (text == "October") {
      return(10)
    } else if (text == "November") {
      return(11)
    } else if (text == "December") {
      return(12)
    }
  })
  
  # reactive variable for shared data
  filtered_table_heatmap <- reactive({
    month <- select_month()
    crime_data <- filter(data,Summarized.Offense.Description == 
                           input$crime_type[1]) %>% 
                  filter(Month == month)
    
    result <- select(crime_data,Date.Reported,Longitude,Latitude,Location,
                     Month,Year)
    return(result)
  })
  
  output$heatmap <- renderLeaflet({
    data_plot <- filtered_table_heatmap()
    lat <- as.vector(select(data_plot,Latitude))
    long <- as.vector(select(data_plot,Longitude))
    
    map <- leaflet(data_plot) %>%
      addTiles() %>%  # Add default OpenStreetMap map tiles
      #addProviderTiles(providers$Stamen.TonerLite,
       #                options = providerTileOptions(noWrap = TRUE)
      #) %>% 
      #addWebGLHeatmap(lng=~long, lat=~lat,size=1000)
      addCircles(lng=~Longitude, lat=~Latitude)
    return(map)
  })
  
  #####################
  #### SECTION ONE ####
  #####################
  #Wasif: map visualization for a heat map, categorized by type of crime. Analysis: talk about the heat map
  
  
  #####################
  #### SECTION TWO ####
  #####################
  
  
  #######################
  #### SECTION THREE ####
  #######################
  
  
  
  ######################
  #### SECTION FOUR ####
  ######################
  
}
  
# Create a new `shinyApp()` using the above ui and server
shinyApp(ui = ui, server = server)





