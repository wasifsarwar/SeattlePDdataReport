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

districts <- c("U-District", "West Seattle", "Greater Ballard", "North Seattle")


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
                            #frequency_title {
                              text-align : center;
                            }
                            #density_title {
                              text-align: center;
                            }
                            h1 {
                              font-size : 40px;
                              font-weight: 500;
                              line-height: 1.1;
                            }
                            h2 {
                              font-size: 30px;
                              font-weight: 400;
                            }
                            h3 {
                              font-size: 25px;
                              font-weight: 300;
                            }
                            h4{
                              font-size: 18px;
                              font-weight: 200;
                            }
                            p {
                              font-weight: 100;
                              font-size: 15px;
                            }
                            "))),
  
  h1("Seattle Crime Data Report of 2017"),
  
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
                 selectInput("crime_freq_type", "Offense Type", choices = offense_type, selected = ""),
                 selectInput("month_freq", "Month", choices = month, selected = ""),
                 helpText("Note: certain offense and month combinations contain empty data,
                          and will thus display empty visualizations. This plot is intended to
                          visualize how frequent crimes in Seattle are for a selected month to
                          answer the questions concerning what time of month are particularly trending 
                          for crime.")
               ),
               mainPanel(
                 br(),
                 textOutput("frequency_title"),
                 br(),
                 plotOutput("freq_plot"),
                 br(),
                 textOutput("crime_frequency")
               )
             )
    ),
    tabPanel("Resolved Crime Rates", fluid = TRUE,
             sidebarLayout(   # layout the page in two columns
               sidebarPanel(  # specify content for the "sidebar" column
                 selectInput("crime_in", "Offense Type", choices = offense_type, selected = ""),
                 selectInput("district_in", "District", choices = districts, selected = ""),
                 helpText("Note: certain offense and district combinations contain empty data,
                          and will thus display empty visualizations. This plot is intended to
                          visualize common crimes in densely populated areas of Seattle to
                          answer the questions concerning what time of year are most cases
                          resolved in, relative to the crime and district selected.")
               ),
               mainPanel(     # specify content for the "main" column
                 br(),
                 textOutput("density_title"),
                 br(),
                 plotOutput("density_plot"),
                 br(),
                 textOutput("density_analysis")
               )
             )
    ),
    tabPanel("Documentation", fluid = TRUE,
             h2("Project Documentation"),
             h4("Created By: Wasif Siddique, Jeremy Chang, Rey Matsunaga, Israel-SixtoSanchez"),
             br(),
             h3("Project Description"),
             p("The data set that we worked with can be found here:",a(href = "https://data.seattle.gov/Public-Safety/Seattle-Police-Department-Police-Report-Incident/7ais-f98f", "Seattle PD data")),
             p("This dataset is based on information from the Seattle City Gov website that records incidents based on initial police reports taken by officers when responding to incidents around the city.
               We've filtered out the data for 2017 records only."),
             p("This data report gives us an insider about how frequently crime is reported over different regions in Seattle. It also presents 
               visualizations regarding how frequent a certain crime is reported everyday for a specific month, and what the statistics are for the reported crimes to be resolved
               by the police."),
             br(),
             h3("Technical Description"),
             p("To create the visualizations we used", a(href = "https://shiny.rstudio.com/", "shiny"), ", ", 
               a(href = "https://rstudio.github.io/leaflet/", "leaflet for R"), ",  and " , a(href = "https://ggplot2.tidyverse.org/", "ggplot"), ". 
               For the in depth data manipulation we used", a(href = "https://cran.r-project.org/web/packages/dplyr/vignettes/dplyr.html","dplyr"), "."),
             p("The interactive widgets on the side allows the user to select crime types, specify months and location. Therefore the resulting plots and analysis are dynamic,
               and will represent the data analysis corresponding to whatever crime type, month or location the user chooses to explore.")
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
  
  #####################
  ### DOCUMENTATION ###
  #####################
  
  output$documentation <- 
  
  
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
    district <- select_district()
    filt_df <- data %>% 
      filter(District.Sector == district) %>% 
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
      labs(title="",
           subtitle=paste0("Percentage of ", input$crime_in, " Crimes reported in ",
                           input$district_in),
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
    reactive_res <- rc_count()
    reactive_unres <- urc_count()
    
    total <- reactive_unres + reactive_res
    
    para <- paste0("This interactive visualization provides information on the 
                       density of resolved/unresolved crimes in Seattle. It allows 
                       the user to compare different districts and crime types to 
                       determine what month of the year crimes are likely to beresolved. 
                       We can see that throughout the year, there are two peaks consistently 
                       occur from Jan-June, and July-Dec. This particular visualization looks at ",
                       input$crime_in, " crimes in the ", input$district_in, " district. An important factor
                       to note, is that of ", input$crime_in, " crimes in the ", input$district_in, " district, only about ",
                       round(((reactive_unres/total) * 100), 2), "% have been resolved, while ", 
                       round(((reactive_res/total) * 100), 2), "% have remained unresolved in a total of ", total, " crimes."
                       )
    
    text <- HTML(para)
    
    return(text)
  })
  
  output$density_title <- renderText({
    text <- HTML("Resolved Crime Rate Density by Crime & District")
    return(text)
  })
  
  select_district <- reactive({
    text <- input$district_in[1]
    if (text == "U-District") {
      return("U")
    } else if (text == "West Seattle") {
      return("W")
    } else if (text == "Greater Ballard") {
      return("J")
    } else if (text == "North Seattle") {
      return("N")
    }
  })
  
  ######################
  #### SECTION FOUR ####
  ######################
  # Get Month
  select_month_freq <- reactive({
    text <- input$month_freq[1]
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
    data <- filter(data, Summarized.Offense.Description == toupper(input$crime_freq_type) & Month == select_month_freq() & Year == 2017)  
    
    result <- select(data, Summarized.Offense.Description,Date.Reported, Month, Year)
    return(result)
  })
  
  
  # Render frequncy Plot over time for specified crime
  
  output$freq_plot <- renderPlot({
    library(RColorBrewer)
    result <- filtered_table_freq_plot() # Get Data
    
    result$Date <- as.character(as.Date(as.character(as.POSIXct(result$Date.Reported, 
                                                   format = "%m/%d/%Y %H:%M:%S %p"))))
    
    # Plot
    x <- ggplot(result) +
      geom_bar(mapping = aes(x = Date), width = 0.5) + 
      theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
      scale_color_brewer(palette = "Set3") +
      labs(title= "",
           x = "Month",
           y="Frequency of Crime")
    return(x)
  })
  
  output$crime_frequency <- renderText({
    result <- filtered_table_freq_plot()
    result$Date <- as.character(as.Date(as.character(as.POSIXct(result$Date.Reported, 
                                                                format = "%m/%d/%Y %H:%M:%S %p"))))
    
    ###################
    ####### REY #######
    ###################
    
    #FIND THE highest number of crimes happened for that month and on which day of the month did it happen.
   # use the max function, Make a count for each date of how many times it has been repeated
    
    result_grouped <- group_by(result, Date) %>% 
      summarize(
        n = n()
      ) %>% 
      arrange(-n)
    
    highest_date <- result_grouped[1, 1]
    highest <- result_grouped[1, 2]
    text <- HTML(paste0("This frequency plot shows a visualization of how frequent ",input$crime_freq_type ," was
      for the month of ",input$month_freq[1],".  The highest record of ", input$crime_freq_type, " that happened in " , 
                        input$month_freq[1], "is ", highest, "  which occurred on ", highest_date, "."))
    return(text)
  })
  
  output$frequency_title <- renderText({
    text <- HTML(paste0("Bar Graph for ", input$crime_freq_type, " over each day in ",input$month_freq[1],"."))
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
