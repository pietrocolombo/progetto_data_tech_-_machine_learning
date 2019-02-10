
# aggiungiamo informazioni al nostro dataset

# funzione per trasforomare gli angoli deg in radianti
deg2rad <- function(deg) {(deg * pi) / (180)}

if(!require(geosphere)){
  install.packages("geosphere")
  library("geosphere")
}

# percorso del file che ha generato lo script creazione_dataset.r
perc_csv <- "dataset_completo.csv"

# rimuoviamo il file che genera questo script
file.remove("dataset_with_add_features.csv")

dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

# cancelliamo righe le quali hanno latitudine >90 0 <-90
# poichè non hanno senso essendo fuori range
dati <- dati[dati$Latitude < 90,]
dati <- dati[dati$Latitude > -90,]

# cancelliamo righe le quali hanno latitudine >180 0 <-180
# poichè non hanno senso essendo fuori range
dati <- dati[dati$Longitude < 180,]
dati <- dati[dati$Longitude > -180,]

# creiamo un array di true o false poichè R è più veloce rispetto a valutare le condizioni
# confrontiamo id_user e id_perc e label se tutti sono uguali alla riga precedente è true
# è false quando sono diversi cioè indica il cambio di percorso
cond <- c(FALSE, (dati$Id_user[-nrow(dati)] == dati$Id_user[-1]) & (dati$Id_perc[-nrow(dati)] == dati$Id_perc[-1]) & (dati$Label[-nrow(dati)] == dati$Label[-1]))

# copio le colonne in array di appoggio per ottimizazione del tempo di esecuzione
longitudine <- dati$Longitude
latitudine <- dati$Latitude
date_time <- dati$Date_Time

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
  if(i_row %% 10000 == 0)
  {
    # print per debug
    print(i_row)
  }
  if(cond[i_row])
  {
    # se la riga precedente e la corrente ha id_user e id_perc e label uguali
    
    # calcoliamo la distanza in metri tra il punto precedente e il punto attuale
    distance[i_row] <- distGeo(c(longitudine[i_row-1], latitudine[i_row-1]), c(longitudine[i_row], latitudine[i_row]))
    # calcoliamo in secondi il tempo tra il punto precedente e il successivo
    delta_time[i_row] <- as.numeric(difftime(date_time[i_row], date_time[i_row-1], units = "secs"))
    # se il delta tempo o il delta distanza sono a 0 metto la velocità a 0
    if(distance[i_row] == 0 | delta_time[i_row] == 0)
    {
      vel[i_row] = 0
    }
    else
    {
      vel[i_row] <- distance[i_row]/delta_time[i_row]
    }
    # calcolo l'angolo tra il nord e due cordinate
    bearing <- atan2(sin(deg2rad(longitudine[i_row]) - deg2rad(longitudine[i_row-1])) * cos(deg2rad(latitudine[i_row])),
                     cos(deg2rad(latitudine[i_row-1])) * sin(deg2rad(latitudine[i_row])) - sin(deg2rad(latitudine[i_row-1]))
                     * cos(deg2rad(latitudine[i_row]))
                     * cos(deg2rad(longitudine[i_row]) - deg2rad(longitudine[i_row-1])))
    bearing = bearing + 2.0 * pi
    while(bearing > 2.0 * pi)
    {
      bearing = bearing - 2.0 * pi
    }
    angle[i_row] <- bearing
  }
}

# inserisco nel data frame i nuovi valori calcolati 
dati$distance <- distance
dati$vel <- vel
dati$delta_time <- delta_time
dati$angle <- angle

# salvo il nuovo csv
write.csv(dati,file="dataset_with_add_features.csv" ,row.names=FALSE) 

