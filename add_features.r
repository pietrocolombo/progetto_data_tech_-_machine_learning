
if(!require(geosphere)){
  install.packages("geosphere")
}
library("geosphere")

perc_csv <- "../progetto_data_tech_&_machine_learning_dataset/dataset_completo_senza_cancellazione_label_row.csv"

dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

id_user_current <- dati$Id_user[1]
id_perc_current <- dati$Id_perc[1]
label_current <- dati$Label[1]
first_time <- TRUE

distance <- c()
vel <- c()
angle <- c()

for(i_row in 1:nrow(dati))
{
  if(id_user_current == dati$Id_user[i_row] && id_perc_current == dati$Id_perc[i_row] && label_current == dati$Label[i_row])
  {
    if(first_time)
    {
      first_time <- FALSE
      distance <- c(distance, 0)
      vel <- c(vel, 0)
      angle <- c(angle, 0)
    }
    else
    {
      # ottendo la distanza in metri
      distance <- c(distance, distGeo(c(dati$Longitude[i_row-1], dati$Latitude[i_row-1]), c(dati$Longitude[i_row], dati$Latitude[i_row])))
      # calcolo la velocità in m/s
      vel <- c(vel, distance[i_row]/as.numeric(difftime(dati$Date_Time[i_row], dati$Date_Time[i_row-1], units = "secs")))
      # calcolo l'angolo tra il punto precedente e il corrente per capire come mi sto movendo
      # angle <- c(angle, )
    }
  }
  else
  {
    id_user_current <- dati$Id_user[i_row]
    id_perc_current <- dati$Id_perc[i_row]
    label_current <- dati$Label[i_row]
    
    fist_time <- TRUE
    i_row <- i_row -1
  }
}

dati$distance <- distance
dati$vel <- vel