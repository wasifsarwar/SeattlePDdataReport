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

#####################
######## API ########
#####################


data <- read.csv("./data/Seattle_police_data_2017.csv",na.strings = "NA",
                   stringsAsFactors = FALSE,fill = TRUE, header = TRUE)

## we're only doing records for 2017

 
ui <- fluidPage(
  titlePanel("Eviction Data Report in the United States"),
  sidebarLayout(
    sidebarPanel(
      # State Widget
      selectInput("State", "Select State", choices = state_data$name, selected = ""),
      # Year Slider
      sliderInput('year_choice', label = "Choose Year", min = year_range[1], 
                  max = year_range[2], value = 2000,step = 1)
    ),
    mainPanel(
      tabsetPanel( 
        tabPanel("Table", br(), strong(textOutput("table_info")), br(), tableOutput("table")),
        tabPanel("Plot", br(), strong(textOutput("plot_info")), br(), plotOutput("plot"
                                                                                 , click = "plot_click"), em(textOutput("click_info"))) 
      )
    )
  )
)
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
