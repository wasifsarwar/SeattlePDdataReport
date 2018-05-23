## SEATTLE PD DATA REPORT ##

## Wasif Siddique
## Jeremy Chang
## Rey Matsunaga
## Israel Sixto-Sanchez

library(httr)
library(jsonlite)

#####################
######## API ########
#####################

resource <- paste0("https://data.seattle.gov/resource/y7pv-r3kh.json")
response <- GET(resource, add_headers("X-API-Key" = token))
fromJSON <- fromJSON(content(response, "text"))
data <- flatten(data.frame(fromJSON$results$bills))


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
