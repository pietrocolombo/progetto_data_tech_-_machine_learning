
#if(!require(revgeo)){
#  install.packages("revgeo")
#  library("revgeo")
#}

perc_csv <- "../progetto_data_tech_&_machine_learning_dataset/dataset_with_add_features.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

file.remove("dataset_with_osm_city.csv")

# soglie per calcolo features
delta_angolo <- 0.8
vel_tr <- 0.3
vr_soglia <- 0.2

# TRUE se la riga corrente e la precedente hanno stessa label, plt e utente
cond <- c(TRUE, (dati$Id_user[-nrow(dati)] == dati$Id_user[-1]) & (dati$Id_perc[-nrow(dati)] == dati$Id_perc[-1]) & (dati$Label[-nrow(dati)] == dati$Label[-1]))

dim_array <- table(cond)["FALSE"]

cond_delta_angolo <- abs(dati$angle[-nrow(dati)] - dati$angle[-1]) > delta_angolo

cond_vel <- dati$vel < vel_tr

cond_vel_0 <- dati$vel > 0

cond_alt_777 <- dati$Altitude == -777

cond_vr <- (abs(dati$vel[-1] - dati$vel[-nrow(dati)]) / dati$vel[-nrow(dati)]) > vr_soglia

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
distanceTotal <- vector(mode="double", length=dim_array)
time_total <- vector(mode="double", length=dim_array)
vel_max <- vector(mode="double", length=dim_array)
# inizializzazione vettore delle label 
label <- vector(mode="character", length=dim_array)
# inizializzazione vettore altitudine media del percorso
altitudeMean <- vector(mode="double", length=dim_array)
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

i <- 2

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
      altitudeSum <- altitudeSum + dati$Altitude[i_row]
    }
    n_points <- n_points + 1
  }
  else
  {
    distanceTotal[i] <- distanceTotal_i
    time_total[i] <- timeTotal_i
    vel_max[i] <- vel_max_i
    hcr[i] <- n_hcr/distanceTotal
    sr[i] <- n_sr/distanceTotal
    vcr[i] <- n_vr/distanceTotal
    npoints[i] <- n_points
    n777[i] <- n_777
    altitudeMean[i] <- altitudeSum/(n_points-n_777)
    longitudeStart[i] <- dati$Longitude[i_row]
    latitudeStart[i] <- dati$Latitude[i_row]
    
    latitudeEnd[i-1] <- dati$Latitude[i_row-1]
    longitudeEnd[i-1] <- dati$Longitude[i_row-1]
    
    label[i] <- dati$Label[i_row-1]
    
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
    
    i <- i+1
  }
  
}

latitudeEnd[i] <- dati$Latitude[nrow(dati)]
longitudeEnd[i] <- dati$Longitude[nrow(dati)]

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
