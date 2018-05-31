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

date_col <- substr(data$Date.Reported, 1, 10)
data[ ,"Recorded_Date"] <- date_col

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
                            #bar_title {
                            text-align: center;
                            }
                            "))),
  
  titlePanel(
    "Seattle crime data report 2017"
    ),
  
  tabsetPanel(
    tabPanel("Heat Map for Crimes in Seattle", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 # State Widget
                 selectInput("crime_type", "Offense Type", choices = offense_type, selected = ""),
                 selectInput("months", "Month", choices = month, selected = "")
               ),
               mainPanel(
                 br(),
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
          selectInput("user_month", "Month", choices = month, selected = "")
        ),
        mainPanel(     # specify content for the "main" column
          br(),
          textOutput("bar_title"),
          br(),
          plotOutput("bargraph"),
          br(),
          textOutput("bar_analysis"),
          br(),
          textOutput("bar_trend")
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
    tabPanel("Resolved Crime Rates", fluid = TRUE,
             sidebarLayout(   # layout the page in two columns
               sidebarPanel(  # specify content for the "sidebar" column
                 selectInput("crime_in", "Offense Type", choices = offense_type, selected = ""),
                 selectInput("district_in", "District", choices = districts, selected = ""),
                 helpText("heyooooooo")
               ),
               mainPanel(     # specify content for the "main" column
                 br(),
                 p("main panel content goes here"),
                 br(),
                 plotOutput("density_plot"),
                 br(),
                 textOutput("density_analysis")
               )
             )
    )
  )
)

server <- function(input,output) {
  
  # converts month name to indexes 
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
                     Month,Year,Occurred.Date.Range.End)
    return(result)
  })
  
  # outputs a heatmap for the selected crime
  output$heatmap <- renderLeaflet({
    data_plot <- filtered_table_heatmap()
    
    map <- leaflet(data_plot) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addHeatmap(lng= ~Longitude, lat= ~Latitude,
                 blur = 18, max = 0.5 , radius = 15)
    
    return(map)
  })
  
  # outputs a title for the map
  output$maptitle <- renderText({
    text <- HTML(paste0("Heat map for ", tolower(input$crime_type[1]), " in the month of ",
                        input$months[1]))
    return(text)
    
  })
  
  # outputs an analysis for the heatmap visualiztion
  output$map_analysis <- renderText({
    data_text <- filtered_table_heatmap()
    unsolved <- data_text %>% 
                filter(Occurred.Date.Range.End == "") 
    unsolved <- NROW(unsolved$Location)    
    count <- NROW(data_text$Location)
    solved <- count - unsolved
    summary <-
      paste0(
        "The heatmap here represents the areas in Seattle where the crime type '", input$crime_type[1], "' is the most common in the 
        month of ", input$months[1], ". The areas on the map that is more saturated is where '", input$crime_type[1], "' is more frequent.")
    report <- 
      if (count > 0){
      paste0(" There were ", count, " reports of '", input$crime_type[1], "' in ", input$months[1], ", of which ", solved, " reports have been
        resolved by the Police, and for the rest of ", unsolved, " reports, the investigation is still ongoing.")
      } else if( count == 0) {
        paste0(" There was no report of ' ", input$crime_type[1], "' in ", input$months[1], ".")
      }
    text <- HTML(paste0(summary,report))
    return(text)
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
                  mapping = aes(x = Summarized.Offense.Description, y = n,
                                fill = Summarized.Offense.Description)) +
      geom_bar(stat = "identity") +
      ggtitle("") +
      xlab("Crime Type") +
      ylab("Number of Incidents") +
      guides(fill = guide_legend(title = "Crime Type")) +
      theme_bw()
    return(bar)
  })
  
  output$bar_title <- renderText({
    text <- HTML(paste0("Top 5 Crimes in the month of ", input$user_month[1] ," in 2017"))
    return(text)
  })
  
  output$bar_analysis <- renderText({
    data_text <- filtered_table_bargraph()
    crime_type <- data_text[, 1]
    crime_freq <- data_text[, 2]
    text <- HTML(paste0("The top 5 crimes that occured in ", input$user_month[1], " 2017 were ", 
                        tolower(crime_type[1,]), " at ", crime_freq[1,], " occurances, ",
                        tolower(crime_type[2,]), " at ", crime_freq[2,], " occurances, ",
                        tolower(crime_type[3,]), " at ", crime_freq[3,], " occurances, ",
                        tolower(crime_type[4,]), " at ", crime_freq[4,], " occurances, ",
                        tolower(crime_type[5,]), " at ", crime_freq[5,], " occurances. "
                        ))
    return(text)
  })
  
  output$bar_trend <- renderText({
    text <- HTML("Throughout the year, burglary and car prowl are two of 
                        the most common types of crimes. This is most likely due to the fact that, during
    the winter months, with shorter daytimes there is more darkness through out the 24-hr period to conceal 
    the actions of the criminals. 
    And during the summer months, there are less people supervising their property, which is likely
    because they on vacation.")
    return(text)
  })
  
  #######################
  #### SECTION THREE ####
  #######################
  
  #filtered data
  resolved_df <- reactive({
    filt_df <- data %>% 
      filter(District.Sector == (input$district_in)) %>% 
      filter(Summarized.Offense.Description == toupper(input$crime_in)) %>% 
      select(Summarized.Offense.Description, Month,
             Occurred.Date.Range.End, District.Sector) %>% 
      mutate(Resolved = (Occurred.Date.Range.End != ""))
    
    return(filt_df)
  })
  
  #resolved count
  rc_count <- reactive({
    reactive_data <- resolved_df()
    
    resolved_crimes <- NROW(reactive_data %>% 
                              filter(Resolved == TRUE))
    return(resolved_crimes)
  })
  
  #unresolved count
  urc_count <- reactive({
    reactive_data <- resolved_df()
    
    unresolved_crimes <- NROW(reactive_data %>% 
                                filter(Resolved == FALSE))
    return(unresolved_crimes)
  })
  
  #density plot
  output$density_plot <- renderPlot({
    reactive_data <- resolved_df()
    reactive_res <- rc_count()
    reactive_unres <- urc_count()
    
    g <- ggplot(reactive_data, aes(Month)) + 
      geom_density(aes(fill=factor(Resolved)), alpha=0.7) + 
      labs(title="Resolved Crime Rate Density by Crime & District",
           subtitle=paste0("Percentage of ", input$crime_in, " Crimes located in ", ######
                           input$district_in, " District"), ######
           caption="Source: suck my ass", ######
           x = "Month",
           y="Density (Crime Rate %)",
           fill="Case Status") +
      scale_x_continuous(breaks = c(seq(1:12)),
                         label = c("JAN", "FEB", "MAR",
                                   "APR", "MAY", "JUNE",
                                   "JULY", "AUG", "SEPT",
                                   "OCT", "NOV", "DEC")) +
      scale_y_continuous(labels = scales::percent) +
      scale_fill_manual(name = "Case Status",
                        labels = list(paste("Unresolved:", reactive_res),
                                      paste("Resolved:", reactive_unres)),
                        values = c("#f6cac9", "#91a7d0"), #pantones: rosequartz + serenity
                        guide = guide_legend(reverse=TRUE))
    return(g)
  })
  
  #plot analysis
  output$density_analysis <- renderText({
    text <- HTML("Add the cool data analysis here.")
    return(text)
  })
  
  ######################
  #### SECTION FOUR ####
  ######################
  # Get Month
  select_month_freq <- reactive({
    text <- input$month_freq
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
  
  
  # GEt data
  filtered_table_freq_plot <- reactive({
    data <- filter(data, Summarized.Offense.Description == toupper(input$crime_freq_type))
    
    result <- select(data, Summarized.Offense.Description, Occurred.Date.or.Date.Range.Start, Month, Year)
    result$Occurred.Date <- str_split(result$Occurred.Date.or.Date.Range.Start, " ")[[1]][1] # Add date column
    return(result)
  })
  
  
  # Render frequncy Plot over time for specified crime
  
  output$freq_plot <- renderPlot({
    result <- filtered_table_freq_plot() # Get Data
    x <- ggplot(data = result) +
      geom_tile(mapping = aes(Month, Occurred.Date, fill = nrow(result)), width = .5) +
      facet_grid(Year ~ Month) +
      scale_fill_gradient(low = "red", high = "green") + 
      labs(x = "Date of Occurrence",
           y = "",
           title = paste0("Time-Series Calendar Heatmap For ", input$crime_freq_type), 
           fill="Close")
    
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
