
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

id_user_current <- dati$Id_user[1]
id_perc_current <- dati$Id_perc[1]
label_current <- dati$Label[1]
first_time <- TRUE

dati$distance <- 0
dati$vel <- 0
dati$delta_time <- 0
dati$angle <- 0

for(i_row in 1:nrow(dati))
{
  if(i_row %% 10000 == 0)
  {
    print(i_row)
  }
  if(id_user_current == dati$Id_user[i_row] && id_perc_current == dati$Id_perc[i_row] && label_current == dati$Label[i_row])
  {
    if(first_time)
    {
      first_time <- FALSE
      dati$distance[i_row] <- 0
      dati$vel[i_row] <- 0
      dati$delta_time[i_row] <- 0
      dati$angle[i_row] <- 0
    }
    else
    {
      # ottendo la distanza in metri
      dati$distance[i_row] <- distGeo(c(dati$Longitude[i_row-1], dati$Latitude[i_row-1]), c(dati$Longitude[i_row], dati$Latitude[i_row]))
      # calcolo differenza di tempo
      dati$delta_time[i_row] <- as.numeric(difftime(dati$Date_Time[i_row], dati$Date_Time[i_row-1], units = "secs"))
      # calcolo la velocità in m/s
      dati$vel[i_row] =  dati$distance[i_row]/dati$delta_time[i_row]

      # calcolo l'angolo tra il punto precedente e il corrente per capire come mi sto movendo
      bearing <- atan2(sin(deg2rad(dati$Longitude[i_row]) - deg2rad(dati$Longitude[i_row-1])) * cos(deg2rad(dati$Longitude[i_row])),
                       cos(deg2rad(dati$Longitude[i_row-1])) * sin(deg2rad(dati$Longitude[i_row])) - sin(deg2rad(dati$Longitude[i_row-1]))
                       * cos(deg2rad(dati$Longitude[i_row]))
                       * cos(deg2rad(dati$Longitude[i_row]) - deg2rad(dati$Longitude[i_row-1])))
      bearing = bearing + 2.0 * pi
      while(bearing > 2.0 * pi)
      {
        bearing = bearing - 2.0 * pi
      }
      dati$angle[i_row] <- bearing
    }
  }
  else
  {
    id_user_current <- dati$Id_user[i_row]
    id_perc_current <- dati$Id_perc[i_row]
    label_current <- dati$Label[i_row]
    
    dati$distance[i_row] <- 0
    dati$vel[i_row] <- 0
    dati$delta_time[i_row] <- 0
    dati$angle[i_row] <- 0
  }
}

write.csv(dati,file="dataset_with_add_features.csv" ,row.names=FALSE) 

