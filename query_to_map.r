
if(!require(revgeo)){
  install.packages("revgeo")
  library("revgeo")
}
if(!require(RCurl)){
  install.packages("RCurl")
  library("RCurl")
}

getfromosm <- function (longitude, latitude)
{
  url <- paste0("http://photon.komoot.de/reverse?lon=", 
                longitude, "&lat=", latitude)
  # get infomation from osm
  data <- tryCatch(getURLAsynchronous(url), error = function(e) "Error")
  returned_data <- tryCatch(fromJSON(data), error = function(e) "There was an issue retrieving an address from Photon.  Please check that your coordinates are correct and try again.")
  
  housenumber <- tryCatch(returned_data$features[[1]]$properties$housenumber, 
                          error = function(e) "House Number Not Found")
  street <- tryCatch(returned_data$features[[1]]$properties$street, 
                     error = function(e) "Street Not Found")
  city <- tryCatch(returned_data$features[[1]]$properties$city, 
                   error = function(e) "City Not Found")
  zip <- tryCatch(returned_data$features[[1]]$properties$postcode, 
                  error = function(e) "Postcode Not Found")
  state <- tryCatch(returned_data$features[[1]]$properties$state, 
                    error = function(e) "State Not Found")
  country <- tryCatch(returned_data$features[[1]]$properties$country, 
                      error = function(e) "Country Not Found")
  type_road <- tryCatch(returned_data$features[[1]]$properties$osm_key, 
                        error = function(e) "type_road Not Found")
  
  if (is.null(housenumber)) {
    housenumber <- "House Number Not Found"
  }
  if (is.null(street)) {
    street <- "Street Not Found"
  }
  if (is.null(city)) {
    city <- "City Not Found"
  }
  if (is.null(zip)) {
    zip <- "Postcode Not Found"
  }
  if (is.null(state)) {
    state <- "State Not Found"
  }
  if (is.null(country)) {
    country <- "Country Not Found"
  }
  if (is.null(type_road)) {
    type_road <- "type_road Not Found"
  }
  
  geocode_data[["housenumber"]] <- c(geocode_data[["housenumber"]], 
                                     housenumber)
  geocode_data[["street"]] <- c(geocode_data[["street"]], 
                                street)
  geocode_data[["city"]] <- c(geocode_data[["city"]], 
                              city)
  geocode_data[["state"]] <- c(geocode_data[["state"]], 
                               state)
  geocode_data[["zip"]] <- c(geocode_data[["zip"]], 
                             zip)
  geocode_data[["country"]] <- c(geocode_data[["country"]], 
                                 country)
  geocode_data[["type_road"]] <- c(geocode_data[["type_road"]], 
                                 country)
  
  geocode_frame <- rbind(geocode_frame, as.data.frame(geocode_data))
  
}


perc_csv <- "../progetto_data_tech_&_machine_learning_dataset/dataset_with_add_features.csv"

dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

state <- c()
country <- c()
city <- c()
type_road <- c()

for(i_row in 1:nrow(dati))
{
  # compose url for query osm
  info <- getfromosm(longitude = dati$Longitude[i_row], latitude = dati$Latitude[i_row])
  
  state <- c(state, info$state)
  country <- c(country, info$country)
  city <- c(city, info$city)
  type_road <- c(type_road, info$type_road)
  
  #revgeo(longitude = dati$Longitude[i_row], latitude = dati$Latitude[i_row], output='frame')
}

dati$state <- state
dati$country <- country
dati$city <- city
dati$type_road <- type_road

write.csv(dati,file="dataset_with_osm.csv" ,row.names=FALSE) 


