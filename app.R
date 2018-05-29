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
library(pracma)
library(compare)

data <- read.csv("./data/Seattle_police_data_2017.csv",na.strings = "NA",
                 stringsAsFactors = FALSE, fill = TRUE, header = TRUE)
offense_type <- tolower(unique(data$Summarized.Offense.Description))
month <- c("January", "February", "March", "April", "May", "June", "July",
           "August","September", "October", "November", "December")

districts <- unique(data$District.Sector)


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
                 br(),
                 textOutput("map_analysis")
               )
             )
    ),
    tabPanel("Top 5 Crimes", fluid = TRUE,
      sidebarLayout(   # layout the page in two columns
        sidebarPanel(  # specify content for the "sidebar" column
          # District Selector
          selectInput("user_month", "months", choices = month, selected = "")
        ),
        mainPanel(     # specify content for the "main" column
          textOutput("Bar Graph"),
          br(),
          plotOutput("bargraph")
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
  
  output$map_analysis <- renderText({
    data_text <- filtered_table_heatmap()
    count <- NROW(data_text$Location)
    text <- HTML(
      paste0(
        "The heatmap here represents the areas in Seattle where the crime type '", input$crime_type[1], "' is the most common in the 
        month of ", input$months[1], ". The areas on the map that is more saturated is where '", input$crime_type[1], "' is more frequent.
        There was ", count, " reports of '", input$crime_type[1], "' in ", input$months[1], "."
      )
    )
  })
  
  
  #####################
  #### SECTION TWO ####
  #####################
  #Jeremy: create a bar graph that shows the top 5 most frequent crimes every month in 2017.
  # Reactive Data Table for the top 5 crimes
  filtered_table_bargraph <- reactive({
    # Select desired columns
    data <- select(data, Summarized.Offense.Description, Month)
    month <- crime_month()
    district_data <- filter(data, Month == month) %>% 
      group_by(Summarized.Offense.Description) %>% 
      summarize(
        n = n()
      ) %>% 
      arrange(-n)
    top_5 <- district_data[1:5, ]
    
    return(top_5)
  })
  
  # Create a bar graph with the top 5 crimes on the x-axis and the frequency on the y-axis
  output$bargraph <- renderPlot({
    data_bar_graph <- filtered_table_bargraph()
    
    bar <- ggplot(data = data_bar_graph,
                  mapping = aes(x = Summarized.Offense.Description, y = n)) +
      geom_bar(stat = "identity") +
      ggtitle("Top 5 Crimes in the Selected Month in 2017") +
      xlab("Crime Type") +
      ylab("Incidents")
      theme_bw()
    return(bar)
  })

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
  
  crime_month <- reactive({
    text <- input$user_month[1]
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
  
  
  
  
  
}

# Create a new `shinyApp()` using the above ui and server
shinyApp(ui = ui, server = server)
