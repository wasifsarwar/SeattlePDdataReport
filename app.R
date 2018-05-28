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


data <- read.csv("./data/Seattle_Police_Department_Police_Report_Incident.csv",na.strings = "NA",
                   stringsAsFactors = FALSE,fill = TRUE, header = TRUE)

## we're only doing records for 2017

data <- data %>% 
        filter(Year == 2017) %>% 
        select(Offense.Type,Summarized.Offense.Description,Date.Reported,
               Occurred.Date.or.Date.Range.Start,Occurred.Date.Range.End,
               Hundred.Block.Location,District.Sector,Longitude,Latitude,
               Location,Month,Year)

 

#####################
#### SECTION ONE ####
#####################



#####################
#### SECTION TWO ####
#####################



#######################
#### SECTION THREE ####
#######################



######################
#### SECTION FOUR ####
######################
