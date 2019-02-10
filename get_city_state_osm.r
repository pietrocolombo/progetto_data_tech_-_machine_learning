
# query a open street map o bing per ottenere le informazioni dello stato, city, regione 
# sul punto di partenza e di arrivo di ogni percorso

if(!require(revgeo)){
  install.packages("revgeo")
  library("revgeo")
}

# leggo il csv generato dallo script compress_database.r
perc_csv <- "dataset_compresso.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

perc_csv_info <- "info_city.csv"

if(!file.exists(perc_csv_info))
{
  not_use_BING = FALSE
  
  # istanzio i nuovi vettori per le nuove informazioni pre allocando la memoria
  # per ottimizare le performance
  stateStart <- vector(mode="character", length=nrow(dati))
  countryStart <- vector(mode="character", length=nrow(dati))
  cityStart <- vector(mode="character", length=nrow(dati))
  
  stateEnd <- vector(mode="character", length=nrow(dati))
  countryEnd <- vector(mode="character", length=nrow(dati))
  cityEnd <- vector(mode="character", length=nrow(dati))
  
  
  for(i_row in 1:nrow(dati))
  {
    if(i_row %% 100 == 0)
    {
      print(i_row)
    }
    if(not_use_BING)
    {
      # query a open street map per ottenere le informazioni dello stato, city, regione del punto di partenza
      info_S <- revgeo(longitude = dati$longitudeStart[i_row], latitude = dati$latitudeStart[i_row], output='hash')
      info_E <- revgeo(longitude = dati$longitudeEnd[i_row], latitude = dati$latitudeEnd[i_row], output='hash')
    }
    else
    {
      # query a bing per ottenere le informazioni dello stato, city, regione del punto di partenza
      info_S <- revgeo(longitude = dati$longitudeStart[i_row], latitude = dati$latitudeStart[i_row], provider = "bing", API = "AigJWwO_gPozxV3zLU9eG5oGKcH5VEEoIb5fp_58AT2QUWj6pNRvg5fitEz_mXT_", output='hash')
      info_E <- revgeo(longitude = dati$longitudeEnd[i_row], latitude = dati$latitudeEnd[i_row], provider = "bing", API = "AigJWwO_gPozxV3zLU9eG5oGKcH5VEEoIb5fp_58AT2QUWj6pNRvg5fitEz_mXT_", output='hash')
    }
    
    # assegno i valori che mi arrivano da open street map
    stateStart[i_row] <- as.character(info_S$state)
    countryStart[i_row] <- as.character(info_S$country)
    cityStart[i_row] <- as.character(info_S$city)
    
    stateEnd[i_row] <- as.character(info_E$state)
    countryEnd[i_row] <- as.character(info_E$country)
    cityEnd[i_row] <- as.character(info_E$city)
  }
  
  # aggiungo i nuovi vettori al dataframe iniziale
  dati$stateStart <- stateStart
  dati$countryStart <- countryStart
  dati$cityStart <- cityStart
  
  dati$stateEnd <- stateEnd
  dati$countryEnd <- countryEnd
  dati$cityEnd <- cityEnd
  
  dati_fin <- data.frame(
    stateStart = dati$stateStart,
    countryStart = dati$countryStart,
    cityStart = dati$cityStart,
    stateEnd = dati$stateEnd,
    countryEnd = dati$countryEnd,
    cityEnd = dati$cityEnd)
  write.csv(dati_fin,file="info_city.csv" ,row.names=FALSE)
}
else
{
  info_city <- read.csv(perc_csv_info, header = TRUE, sep =",", quote = "\"", dec = ".")
  
  dati$stateStart <- info_city$stateStart
  dati$countryStart <- info_city$countryStart
  dati$cityStart <- info_city$cityStart
  
  dati$stateEnd <- info_city$stateEnd
  dati$countryEnd <- info_city$countryEnd
  dati$cityEnd <- info_city$cityEnd
}

# salvo le informazioni
write.csv(dati,file="dataset_compresso_info_city.csv" ,row.names=FALSE)

