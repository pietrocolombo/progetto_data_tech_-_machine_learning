
if(!require(revgeo)){
  install.packages("revgeo")
  library("revgeo")
}

perc_csv <- "dataset_compresso.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

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
  info_S <- revgeo(longitude = dati$longitudeStart[i_row], latitude = dati$latitudeStart[i_row], output='hash')
  
  info_E <- revgeo(longitude = dati$longitudeEnd[i_row], latitude = dati$latitudeEnd[i_row], output='hash')
  
  stateStart[i_row] <- as.character(info_S$state)
  countryStart[i_row] <- as.character(info_S$country)
  cityStart[i_row] <- as.character(info_S$city)
  
  stateEnd[i_row] <- as.character(info_E$state)
  countryEnd[i_row] <- as.character(info_E$country)
  cityEnd[i_row] <- as.character(info_E$city)
}
dati$stateStart <- stateStart
dati$countryStart <- countryStart
dati$cityStart <- cityStart

dati$stateEnd <- stateEnd
dati$countryEnd <- countryEnd
dati$cityEnd <- cityEnd

write.csv(dati,file="dataset_compresso_info_city_v2.csv" ,row.names=FALSE) 

  

