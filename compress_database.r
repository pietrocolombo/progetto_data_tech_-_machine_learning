
# dalle n righe che mi identificano un percorso
# ottengo una riga per percorso

# carico il csv che lo script add_features.r mi ha generato
perc_csv <- "dataset_with_add_features.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

# rimuoviamo il file che genera questo script
file.remove("dataset_with_osm_city.csv")

# soglie per calcolo features
delta_angolo <- 0.33 # angolo per il change rate in radianti
vel_tr <- 0.89 # velocità in m/s  
vr_soglia <- 0.26 # percentuale di cambio velocità

# TRUE se la riga corrente e la precedente hanno stessa label, plt e utente
cond <- c(TRUE, (dati$Id_user[-nrow(dati)] == dati$Id_user[-1]) & (dati$Id_perc[-nrow(dati)] == dati$Id_perc[-1]) & (dati$Label[-nrow(dati)] == dati$Label[-1]))

# calcolo la dimensione della nuova tabella
dim_array <- table(cond)["FALSE"]

# inizializzazione condizioni per velocizzare l'esecuzione
cond_delta_angolo <- c(FALSE, abs(dati$angle[-nrow(dati)] - dati$angle[-1]) > delta_angolo)
cond_vel <- dati$vel < vel_tr
cond_vel_0 <- dati$vel > 0
cond_alt_777 <- dati$Altitude == -777
cond_vr <- c(FALSE, (abs(dati$vel[-1] - dati$vel[-nrow(dati)]) / dati$vel[-1]) > vr_soglia)

# inizializzazione variabili temporanee per ogni traiettoria
distanceTotal_i <- 0
vel_max_i <- 0
timeTotal_i <- 0
n_hcr <- 0
n_sr <- 0
n_vr <- 0
vr <- 0
altitudeSum <- 0
# valore di deault inizialedi 
altitude_max <- -776
n_777 <- 0
n_points <- 0

# inizializzazione vettori delle features
distanceTotal <- vector(mode="double", length=dim_array)
time_total <- vector(mode="double", length=dim_array)
vel_max <- vector(mode="double", length=dim_array)
vel_avg <- vector(mode="double", length=dim_array)

# inizializzazione vettore delle label 
label <- vector(mode="character", length=dim_array)

# inizializzazione vettore ID_perc
Id_perc <- vector(mode="character", length=dim_array)

# inizializzazione vettore ID_user
Id_user <- vector(mode="character", length=dim_array)

# inizializzazione vettore Time_start che rappresenta il time stamp di inizio del percorso
Time_start <- vector(mode="character", length=dim_array)
Time_start[1] <- as.character(dati$Date_Time[1])

# inizializzazione vettore Time_end che rappresenta il time stamp di fine del percorso
Time_end <- vector(mode="character", length=dim_array)

# inizializzazione vettore altitudine media del percorso
altitudeAvg <- vector(mode="double", length=dim_array)

# inizializzazione vettore del'altitudine massima raggiunta durante il percorso
altitudeMax <- vector(mode="double", length=dim_array)

# inizializzazione vettore latitudine del punto di partenza
latitudeStart <- vector(mode="double", length=dim_array)
latitudeStart[1] <- dati$Latitude[1]

# inizializzazione vettore latitudine del punto di arrivo
latitudeEnd <- vector(mode="double", length=dim_array)

# inizializzazione vettore longitudine del punto di partenza
longitudeStart <- vector(mode="double", length=dim_array)
longitudeStart[1] <- dati$Longitude[1]

# inizializzazione vettore longitudine del punto di arrivo
longitudeEnd <- vector(mode="double", length=dim_array)

# Heading change rate
hcr <- vector(mode="double", length=dim_array)

# stop rate
sr <- vector(mode="double", length=dim_array)

# velocity change rate
vcr <- vector(mode="double", length=dim_array)

# numero punti con altezza a -777 di un percorso 
n777 <- vector(mode="double", length=dim_array)

# n punti di un percorso
npoints <- vector(mode="double", length=dim_array)

i <- 1
for(i_row in 2:nrow(dati))
{
  if(i_row %% 10000 == 0)
  {
    print(i_row)
  }
  if(cond[i_row])
  {
    # se sto scorrento lo stesso percorso
    # aggiorno la distanza totale sommando le distanze parziali
    distanceTotal_i <- distanceTotal_i + dati$distance[i_row]
    timeTotal_i <- timeTotal_i + dati$delta_time[i_row]
    if(dati$vel[i_row] > vel_max_i){
      # tengo traccia della velocità massima del percorso
      vel_max_i <- dati$vel[i_row]
    }
    if(cond_delta_angolo[i_row])
    {
      # tengo conto del numero di volte che cambia direzione
      n_hcr <- n_hcr + 1
    }
    if(cond_vel[i_row])
    {
      # si aggiorna il contatore contenente il numero di punti del percorso la cui velocità è inferiore della soglia
      n_sr <- n_sr + 1
    }
    if(cond_vel_0[i_row])
    {
      #vr <- abs(dati$vel[i_row - 1] - dati$vel[i_row]) / dati$vel[i_row]
      if(cond_vr[i_row])
      {
        n_vr <- n_vr + 1
      }
    }
    if(cond_alt_777[i_row])
    {
      # si aggiorna il contatore contenente il numero di punti del percorso la cui altitudine non è conosciuta
      n_777 <- n_777 + 1
    }
    else
    {
      #calcolo parametri dell'altitudine
      altitudeSum <- altitudeSum + dati$Altitude[i_row]
      if(altitude_max < dati$Altitude[i_row])
      {
        altitude_max <- dati$Altitude[i_row]
      }
    }
    n_points <- n_points + 1
  }
  else
  {
    # scrittura dei parametri finali di un percorso nei rispettivi vettori
    distanceTotal[i] <- distanceTotal_i
    time_total[i] <- timeTotal_i
    vel_max[i] <- vel_max_i
    vel_avg[i] <- distanceTotal_i/timeTotal_i
    hcr[i] <- n_hcr/distanceTotal_i
    sr[i] <- n_sr/distanceTotal_i
    vcr[i] <- n_vr/distanceTotal_i
    npoints[i] <- n_points
    n777[i] <- n_777
    altitudeAvg[i] <- altitudeSum/(n_points-n_777)
    altitudeMax[i] <- altitude_max
    latitudeEnd[i] <- dati$Latitude[i_row-1]
    longitudeEnd[i] <- dati$Longitude[i_row-1]
    label[i] <- as.character(dati$Label[i_row-1])
    Id_perc[i] <- as.character(dati$Id_perc[i_row-1])
    Id_user[i] <- as.character(dati$Id_user[i_row-1])
    Time_end[i] <- as.character(dati$Date_Time[i_row-1])
    
    # reset parametri
    distanceTotal_i <- 0
    vel_max_i <- 0
    timeTotal_i <- 0
    n_hcr <- 0
    n_sr <- 0
    n_vr <- 0
    # vr <- 0
    n_777 <- 0
    n_points <- 0
    altitudeSum <- 0
    altitude_max <- -776
    
    # inizializzazione parametri iniziali per il nuovo percorso
    longitudeStart[i+1] <- dati$Longitude[i_row]
    latitudeStart[i+1] <- dati$Latitude[i_row]
    Time_start[i+1] <- as.character(dati$Date_Time[i_row])
    
    # aggiornamento contatore
    i <- i+1
  }
}
# scrittura parametri per l'ultimo percorso del file all'interno dei rispettivi vettori
distanceTotal[i] <- distanceTotal_i
time_total[i] <- timeTotal_i
vel_max[i] <- vel_max_i
vel_avg[i] <- distanceTotal_i/timeTotal_i
hcr[i] <- n_hcr/distanceTotal_i
sr[i] <- n_sr/distanceTotal_i
vcr[i] <- n_vr/distanceTotal_i
npoints[i] <- n_points
n777[i] <- n_777
altitudeAvg[i] <- altitudeSum/(n_points-n_777)
altitudeMax[i] <- altitude_max
latitudeEnd[i] <- dati$Latitude[i_row]
longitudeEnd[i] <- dati$Longitude[i_row]
label[i] <- as.character(dati$Label[i_row])
Id_perc[i] <- as.character(dati$Id_perc[i_row])
Id_user[i] <- as.character(dati$Id_user[i_row])
Time_end[i] <- as.character(dati$Date_Time[i_row])

# creazione del frame contenente tutti i valori
dati_fin <- data.frame(
  Id_user = Id_user,
  Id_perc = Id_perc,
  label = label,
  longitudeStart = longitudeStart,
  latitudeStart = latitudeStart,
  latitudeEnd = latitudeEnd,
  longitudeEnd = longitudeEnd,
  TimeStart = Time_start,
  TimeEnd = Time_end,
  distanceTotal = distanceTotal,
  time_total = time_total,
  n777 = n777,
  npoints = npoints,
  vel_avg = vel_avg,
  vel_max = vel_max,
  altitudeAvg = altitudeAvg * 0.3048,
  altitudeMax = altitudeMax * 0.3048,
  vcr = vcr,
  sr = sr,
  hcr = hcr
  )

#info <- revgeo(longitude = dati$Longitude, latitude = dati$Latitude, output='hash')
#dati$state <- as.character(info$state)
#dati$country <- as.character(info$country)
#dati$city <- as.character(info$city)

# scrittura dati all'interno del file csv
write.csv(dati_fin,file="dataset_compresso.csv" ,row.names=FALSE) 
