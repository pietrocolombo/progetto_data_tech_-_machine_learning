
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

distance <- c()
vel <- c()
delta_time <- c()
angle <- c()

for(i_row in 1:nrow(dati))
{
  if(i_row %% 1000 == 0)
  {
    print(i_row)
  }
  if(id_user_current == dati$Id_user[i_row] && id_perc_current == dati$Id_perc[i_row] && label_current == dati$Label[i_row])
  {
    if(first_time)
    {
      first_time <- FALSE
      distance <- c(distance, 0)
      vel <- c(vel, 0)
      delta_time <- c(delta_time, 0)
      angle <- c(angle, 0)
    }
    else
    {
      # ottendo la distanza in metri
      distance <- c(distance, distGeo(c(dati$Longitude[i_row-1], dati$Latitude[i_row-1]), c(dati$Longitude[i_row], dati$Latitude[i_row])))
      # calcolo differenza di tempo
      delta <- as.numeric(difftime(dati$Date_Time[i_row], dati$Date_Time[i_row-1], units = "secs"))
      # calcolo la velocità in m/s
      vel <- c(vel, distance[i_row]/delta)
      delta_time <- c(delta_time, delta)
      
      # calcolo l'angolo tra il punto precedente e il corrente per capire come mi sto movendo
      bearing <- atan2(sin(dati$Longitude[i_row] - dati$Longitude[i_row-1]) * cos(dati$Longitude[i_row]),
                       cos(dati$Longitude[i_row-1]) * sin(dati$Longitude[i_row]) - sin(dati$Longitude[i_row-1]) * cos(dati$Longitude[i_row])
                       *cos(dati$Longitude[i_row] - dati$Longitude[i_row-1]))
      bearing = bearing + 2.0 * pi
      while(bearing > 2.0 * pi)
      {
        bearing = bearing - 2.0 * pi
      }
      angle <- c(angle, bearing)
    }
  }
  else
  {
    id_user_current <- dati$Id_user[i_row]
    id_perc_current <- dati$Id_perc[i_row]
    label_current <- dati$Label[i_row]
    
    distance <- c(distance, 0)
    vel <- c(vel, 0)
    delta_time <- c(delta_time, 0)
    angle <- c(angle, 0)
  }
}

dati$distance <- distance
dati$vel <- vel
dati$delta_time <- delta_time
dati$angle <- angle

write.csv(dati,file="dataset_with_add_features.csv", append=TRUE,sep=",",row.names=FALSE) 

