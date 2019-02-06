
if(!require(revgeo)){
  install.packages("revgeo")
  library("revgeo")
}

perc_csv <- "../progetto_data_tech_&_machine_learning_dataset/dataset_with_add_features.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

file.remove("dataset_with_osm.csv")

info <- revgeo(longitude = dati$Longitude, latitude = dati$Latitude, output='frame')

dati$state <- as.character(info$state)
dati$country <- as.character(info$country)
dati$city <- as.character(info$city)

write.csv(dati,file="dataset_with_osm_city.csv" ,row.names=FALSE) 
