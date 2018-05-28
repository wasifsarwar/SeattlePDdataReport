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

#####################
######## API ########
#####################


data <- read.csv("./data/Seattle_police_data_2017.csv",na.strings = "NA",
                   stringsAsFactors = FALSE, fill = TRUE, header = TRUE)
offense_type <- unique(data$Offense.Type)
#colnames(offense_type) <- 

## we're only doing records for 2017

 
ui <- fluidPage(
  titlePanel("Seattle PD data report 2017"),
  
  tabsetPanel(
    tabPanel("HeatMap", fluid = TRUE,
      sidebarLayout(
        sidebarPanel(
        # State Widget
          selectInput("crime_type", "Offense Type", choices = offense_type, selected = "")
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
  
  
  # reactive variable for shared data
  filtered_table_heatmap <- reactive({
    crime_data <- data %>%
      filter(Offense.type == input$crime_type[1]) 
    
    result <- select(crime_data,Date.Reported,Longitude,Latitude,Location,
                     Month,Year)
    return(result)
  })
  
  output$heatmap <- renderLeaflet({
    data_plot <- filtered_table() 
    
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
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





