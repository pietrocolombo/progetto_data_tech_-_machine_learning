
deg2rad <- function(deg) {(deg * pi) / (180)}

if(!require(geosphere)){
  install.packages("geosphere")
}
library("geosphere")

perc_csv <- "../progetto_data_tech_&_machine_learning_dataset/dataset_completo.csv"

file.remove("dataset_with_add_features.csv")

dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

# cancelliamo righe le quali hanno latitudine >90 0 <-90 
dati <- dati[dati$Latitude < 90,]
dati <- dati[dati$Latitude > -90,]

dati <- dati[dati$Longitude < 180,]
dati <- dati[dati$Longitude > -180,]



cond <- c(FALSE, (dati$Id_user[-nrow(dati)] == dati$Id_user[-1]) & (dati$Id_perc[-nrow(dati)] == dati$Id_perc[-1]) & (dati$Label[-nrow(dati)] == dati$Label[-1]))
dati$distance <- 0
dati$vel <- 0
dati$delta_time <- 0
dati$angle <- 0

distance <- dati$distance
vel <- dati$vel
delta_time <- dati$delta_time
angle <- dati$angle

for(i_row in 1:nrow(dati))
{
  if(cond[i_row])
  {
    if(i_row %% 10000 == 0)
    {
      print(i_row)
    }
    longitudinePrec <- dati$Longitude[i_row-1]
    latitudinePrec <- dati$Latitude[i_row-1]
    longitudine <- dati$Longitude[i_row]
    latitudine <- dati$Latitude[i_row]
    
    distance[i_row] <- distGeo(c(longitudinePrec, latitudinePrec), c(longitudine, latitudine))
    delta_time[i_row] <- as.numeric(difftime(dati$Date_Time[i_row], dati$Date_Time[i_row-1], units = "secs"))
    vel[i_row] <- distance[i_row]/delta_time[i_row]
    bearing <- atan2(sin(deg2rad(longitudine) - deg2rad(longitudinePrec)) * cos(deg2rad(latitudine)),
                     cos(deg2rad(latitudinePrec)) * sin(deg2rad(latitudine)) - sin(deg2rad(latitudinePrec))
                     * cos(deg2rad(latitudine))
                     * cos(deg2rad(longitudine) - deg2rad(longitudinePrec)))
    bearing = bearing + 2.0 * pi
    while(bearing > 2.0 * pi)
    {
      bearing = bearing - 2.0 * pi
    }
    angle[i_row] <- bearing
  }
}

dati$distance <- distance
dati$vel <- vel
dati$delta_time <- delta_time
dati$angle <- angle

write.csv(dati,file="dataset_with_add_features.csv" ,row.names=FALSE) 

