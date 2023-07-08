#A4
#Author: Owen Treleaven
#-------------------------------
getwd()
setwd("~/Google Drive/UoT - MBiotech/DHT_R_2023/Assignments/A4_MARTIANS")
library(tidyverse)
ufo_data <- read.delim("ufo_subset.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
print(head(ufo_data))
class(ufo_data)#ensuring data is loaded as a dataframe
#' Data Dictionary
#' datetime: Contains date and time of sighting
#' city: City in which UFO was sighted
#' state: State code in which UFO was sighted
#' country: Country code of sighting
#' shape: Shape of the UFO
#' duration seconds: Duration of the sighting in seconds
#' duration hours min: Duration of the sighting in hours and min
#' comments: Sighting description
#' date _osted: Posted date of the sighting
#' latitude: Latitude coordinate of the sighting
#' longitude: Longitude coordinate of the sighting

column_names <- colnames(ufo_data) #trimming potential spaces in column names
clean_col_names <- trimws(column_names, which = "both")

# Replacing old column names with the cleaned ones
colnames(ufo_data) <- clean_col_names
#Finding and replacing missing shape data
missing_shape <- filter(ufo_data, shape == "")
ufo_data <- mutate(ufo_data, shape = replace(shape, shape == "", "unknown"))
#Finding those city values that contain country information and incorporating the country into the country column
ufo_data <- mutate(ufo_data, 
                   country = ifelse(country == "", 
                                    str_extract(city, "\\(.*\\)"), 
                                    country))
ufo_data$country <- str_replace_all(ufo_data$country, "[()]", "")
missing_country <- filter(ufo_data, country == "")
ufo_data$country <- trimws(ufo_data$country)
ufo_data <- filter(ufo_data, country != "")
# Converting datetime and date_posted to appropriate formats






 

