
if(!require(revgeo)){
  install.packages("revgeo")
  library("revgeo")
}

perc_csv <- "../progetto_data_tech_&_machine_learning_dataset/dataset_with_add_features.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

file.remove("dataset_with_osm_city.csv")

# TRUE se la riga corrente e la precedente hanno stessa label, plt e utente
cond <- c(TRUE, (dati$Id_user[-nrow(dati)] == dati$Id_user[-1]) & (dati$Id_perc[-nrow(dati)] == dati$Id_perc[-1]) & (dati$Label[-nrow(dati)] == dati$Label[-1]))

cond_delta_angolo <- abs(dati$angle[-nrow(dati)] - dati$angle[-1]) > delta_angolo

cond_vel <- dati$vel < vel_tr

cond_vel_0 <- dati$vel > 0

cond_alt_777 <- dati$Altitude == -777

cond_vr <- abs(dati$vel[- 1] - dati$vel[-nrow(dati)]) / dati$vel[-nrow(dati)] > vr_soglia



# soglie per calcolo features
delta_angolo <- 0.8
vel_tr <- 0.3
vr_soglia <- 0.2

# inizializzazione variabili temporanee per ogni traiettoria
distanceTotal_i <- 0
vel_max_i <- 0
timeTotal_i <- 0
n_hcr <- 0
n_sr <- 0
n_vr <- 0
vr <- 0
altitudeSum <- 0
n_777 <- 0
n_points <- 0

# inizializzazione vettori delle features
distanceTotal <- c()
time_total <- c()
vel_max <- c()
# inizializzazione vettore delle label 
label <- c()
# inizializzazione vettore altitudine media del percorso
altitudeMean <- c()
# inizializzazione vettore latitudine del punto di partenza
latitudeStart <- c(dati$Latitude[1])
# inizializzazione vettore latitudine del punto di arrivo
latitudeEnd <- c()
# inizializzazione vettore longitudine del punto di partenza
longitudeStart <- c(dati$Longitude[1])
# inizializzazione vettore longitudine del punto di arrivo
longitudeEnd <- c()
# Heading change rate
hcr <- c()
# stop rate
sr <- c()
# velocity change rate
vcr <- c()
# numero punti con altezza a -777 di un percorso 
n777 <- c()
# n punti di un percorso
npoints <- c()

for(i_row in 2:nrow(dati))
{
  if(i_row %% 10000 == 0)
  {
    print(i_row)
  }
  if(cond[i_row])
  {
    distanceTotal_i <- distanceTotal_i + dati$distance[i_row]
    timeTotal_i <- timeTotal_i + dati$delta_time[i_row]
    if(dati$vel[i_row] > vel_max_i){
      vel_max_i <- dati$vel[i_row]
    }
    if(cond_delta_angolo[i_row])
    {
      n_hcr <- n_hcr + 1
    }
    if(cond_vel[i_row])
    {
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
      n_777 <- n_777 + 1
    }
    else
    {
      altitudeSum <- altitudeSum + dati$Altitude
    }
    n_points <- n_points + 1
  }
  else
  {
    distanceTotal <- c(distanceTotal, distanceTotal_i)
    time_total <- c(time_total, timeTotal_i)
    vel_max <- c(vel_max, vel_max_i)
    hcr <- c(hcr, (n_hcr/distanceTotal))
    sr <- c(sr, (n_sr/distanceTotal))
    vcr <- c(vcr, n_vr/distanceTotal)
    npoints <- c(npoints, n_points)
    n777 <- c(n777, n_777)
    altitudeMean <- c(altitudeMean, altitudeSum/(n_points-n_777))
    longitudeStart <- c(longitudeStart, dati$Longitude[i_row])
    latitudeStart <- c(latitudeStart, dati$Latitude[i_row])
    
    latitudeEnd <- c(latitudeEnd, dati$Latitude[i_row-1])
    longitudeEnd <- c(longitudeEnd, dati$Longitude[i_row-1])
    
    label <- dati$Label[i_row-1]
    
    distanceTotal_i <- 0
    vel_max_i <- 0
    timeTotal_i <- 0
    n_hcr <- 0
    n_sr <- 0
    n_vr <- 0
    vr <- 0
    n_777 <- 0
    n_points <- 0
    altitudeSum <- 0
    
  }
  
}
dati_fin <- data.frame(
  longitudeStart = longitudeStart,
  latitudeStart = latitudeStart,
  latitudeEnd = latitudeEnd,
  longitudeEnd = longitudeEnd,
  altitudeMean = altitudeMean,
  label = label,
  n777 = n777,
  npoints = npoints,
  vcr = vcr,
  sr = sr,
  hcr = hcr,
  vel_max = vel_max,
  time_total = time_total,
  distanceTotal = distanceTotal)

#info <- revgeo(longitude = dati$Longitude, latitude = dati$Latitude, output='hash')

#dati$state <- as.character(info$state)
#dati$country <- as.character(info$country)
#dati$city <- as.character(info$city)

write.csv(dati_fin,file="dataset_compresso.csv" ,row.names=FALSE) 
