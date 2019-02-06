
if(!require(revgeo)){
  install.packages("revgeo")
  library("revgeo")
}

perc_csv <- "../progetto_data_tech_&_machine_learning_dataset/dataset_with_add_features.csv"

file.remove("dataset_with_osm.csv")

dati$state <- ""
dati$country <- ""
dati$city <- ""

state <- dati$state
country <- dati$country
city <- dati$city

for(i_row in 1:nrow(dati))
{
  info = evgeo(longitude = dati$Longitude[i_row], latitude = dati$Latitude[i_row], output='frame')
  
  state[i_row] <- as.character(info$state)
  country[i_row] <- as.character(info$country)
  city[i_row] <- as.character(info$city)

}

dati$state <- state
dati$country <- country
dati$city <- city

write.csv(dati,file="dataset_with_osm_city.csv" ,row.names=FALSE) 
