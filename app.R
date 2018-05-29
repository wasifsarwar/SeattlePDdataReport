## SEATTLE PD DATA REPORT ##

## Wasif Siddique
## Jeremy Chang
## Rey Matsunaga
## Israel Sixto-Sanchez

library(dplyr)
library(ggplot2)
library(tidyverse)
library(shiny)
library(leaflet)
library(leaflet.extras)
library(shinythemes)

data <- read.csv("./data/Seattle_police_data_2017.csv",na.strings = "NA",
                 stringsAsFactors = FALSE, fill = TRUE, header = TRUE)
offense_type <- tolower(unique(data$Summarized.Offense.Description))
month <- c("January", "February", "March", "April", "May", "June", "July",
           "August","September", "October", "November", "December")


## we're only doing records for 2017


ui <- fluidPage(
  #shinythemes::themeSelector(),
  theme = shinythemes::shinytheme("spacelab"),
  
  tags$head(tags$style(HTML("
                            #maptitle {
                            text-align: center;
                            }
                            div.box-header {
                            text-align: center;
                            }
                            "))),
  
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
                 textOutput("maptitle"),
                 br(),
                 leafletOutput("heatmap"),
                 br()
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
    tabPanel("Crime Frequency" , fluid = TRUE,
             sidebarLayout(   # layout the page in two columns
               sidebarPanel(  # specify content for the "sidebar" column
                 selectInput("crime_freq_type", "Offense Type", choices = offense_type, selected = "")
               ),
               mainPanel(
                 br(),
                 plotOutput("freq_plot"),
                 br()
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
                           toupper(input$crime_type[1])) %>% 
      filter(Month == month)
    
    result <- select(crime_data,Date.Reported,Longitude,Latitude,Location,
                     Month,Year)
    return(result)
  })
  
  output$heatmap <- renderLeaflet({
    data_plot <- filtered_table_heatmap()
    
    map <- leaflet(data_plot) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addHeatmap(lng= ~Longitude, lat= ~Latitude,
                 blur = 18, max = 0.5 , radius = 15)
    return(map)
  })
  
  output$maptitle <- renderText({
    text <- HTML(paste0("Heat map for ", tolower(input$crime_type[1]), " in the month of ",
                        input$months[1]))
    return(text)
    
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
  
  # GEt data
  filtered_table_freq_plot <- reactive({
    data <- filter(data,Summarized.Offense.Description == 
                           toupper(input$crime_freq_type))
    
    result <- select(data, Summarized.Offense.Description, Month)
    
    return(result)
  })
  
  
  # Render frequncy Plot over time for specified crime
  
  output$freq_plot <- renderPlot({
    result <- filtered_table_freq_plot()
    
    x <- ggplot(data = result) +
      geom_bar(mapping = aes(x = Month), stat = "count", width = .5) + # no y mapping needed!
      labs(
        title=paste0(input$crime_freq_type, " in 2017"),
        x = "Month",
        y = paste0("Count for ", input$crime_freq_type),
        legend = ""
      ) + 
      guides(fill = guide_legend(title=""))
    return(x)
  })
  
  
  
  
  
  
  
}

# Create a new `shinyApp()` using the above ui and server
shinyApp(ui = ui, server = server)
