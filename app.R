## SEATTLE PD DATA REPORT ##

## Wasif Siddique
## Jeremy Chang
## Rey Matsunaga
## Israel Sixto-Sanchez

#####################
######## API ########
#####################
request_topic <- function(topic) {
  resource <- paste0("https://data.seattle.gov/resource/y7pv-r3kh.json")
  response <- GET(paste0(base_uri, bill_resource), add_headers("X-API-Key" = api_key))
  fromJSON <- fromJSON(content(bill_response, "text"))
  data <- flatten(data.frame(bill_fromJSON$results$bills))
  
  bill_data <- bill_data[1:10, ]
  legislator <- paste(bill_data$sponsor_name, bill_data$sponsor_party, bill_data$sponsor_state, sep = ", ")
  short_df <- data.frame(bill_data$bill_id, bill_data$short_title, 
                         legislator, bill_data$active, 
                         bill_data$govtrack_url)
  data_columns <- c("Bill ID", "Name", "Legislator", "Active", "URL")
  colnames(short_df) <- data_columns
  short_df
}

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
