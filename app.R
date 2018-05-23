## SEATTLE PD DATA REPORT ##

## Wasif Siddique
## Jeremy Chang
## Rey Matsunaga
## Israel Sixto-Sanchez

<<<<<<< HEAD
library("dplyr")
library("ggplot2")
library("tidyverse")
library("shiny")
library("httr")
library("jsonlite")

source("api_token.R")
#####################
######## API ########
#####################
resource <- paste0("https://data.seattle.gov/resource/y7pv-r3kh.json")
response <- GET(resource, add_headers(token))
fromJSON <- fromJSON(content(response, "text"))
data <- flatten(data.frame(fromJSON))
  
View(data)

data_columns <- c("Bill ID", "Name", "Legislator", "Active", "URL")
colnames(short_df) <- data_columns
short_df
=======
library(httr)
library(jsonlite)

#####################
######## API ########
#####################

resource <- paste0("https://data.seattle.gov/resource/y7pv-r3kh.json")
response <- GET(resource, add_headers("X-API-Key" = token))
fromJSON <- fromJSON(content(response, "text"))
data <- flatten(data.frame(fromJSON$results$bills))

>>>>>>> wasif

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
