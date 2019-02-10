#script for download osm map
if(!require(RCurl)){
  install.packages("RCurl")
  library("RCurl")
}

perc_csv <- "dataset_with_add_features.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

perc <- "osm_central/"

cond <- c(TRUE, (dati$Id_user[-nrow(dati)] == dati$Id_user[-1]) & (dati$Id_perc[-nrow(dati)] == dati$Id_perc[-1]) & (dati$Label[-nrow(dati)] == dati$Label[-1]))

index <- 0

latitude <- c()
longitude <- c()

# scarico le mappe nel punto centrale di ogni ogni percorso
for(i_row in 1:nrow(dati))
{
  if(i_row %% 10000 == 0)
  {
    print(i_row)
  }
  if(cond[i_row])
  {
   index <- index + 1 
  }
  else
  {
    #url <- "http://lz4.overpass-api.de/api/interpreter?data=(" # sopra le 100 query il server ci bloccava
    url <- "https://overpass.kumi.systems/api/interpreter?data=("
    area <- paste0("(around:150,",dati$Latitude[i_row - (index%/%2)],",",dati$Longitude[i_row - (index%/%2)], ")")
    
    highwaysQuery <- paste0("node[highway]", area, ";",
                            "way[highway]", area, ";",
                            "relation[highway]", area, ";")
    
    aerowayQuery <- paste0("node[aeroway]", area, ";",
                           "way[aeroway]", area, ";",
                           "relation[aeroway]", area, ";")
    
    railwayQuery <- paste0("node[railway]", area, ";",
                           "way[railway]", area, ";",
                           "relation[railway]", area, ";")
    
    # url_old <- paste0("http://overpass-api.de/api/interpreter?data=(way[","highway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");way[","aeroway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");way[","railway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");>;);out%20meta;")
    
    url <- paste0(url, highwaysQuery, aerowayQuery, railwayQuery, ");(._;>;);out%20meta;")
    osm <- tryCatch(getURLAsynchronous(url), error = function(e) "Error")
    # osm <-getURL(url)
    
    fileConn<-file(paste0(perc, (i_row - (index%/%2)), ".osm"))
    writeLines(osm, fileConn)
    close(fileConn)
    
    latitude <- c(latitude, dati$Latitude[i_row - (index%/%2)])
    longitude <- c(longitude, dati$Longitude[i_row - (index%/%2)])
    
    index <- 0
  }
}

dati_fin <- data.frame(
  latitude = latitude,
  longitude = longitude
)

write.csv(dati_fin,file="lat_lon_central.csv" ,row.names=FALSE)

# prendo le mappe per ogni punto iniziale di ogni percorso

perc <- "osm_start/"


index <- 0

latitude <- c()
longitude <- c()

for(i_row in 1:nrow(dati))
{
  if(i_row %% 10000 == 0)
  {
    print(i_row)
  }
  if(cond[i_row])
  {
    index <- index + 1 
  }
  else
  {
    #url <- "http://lz4.overpass-api.de/api/interpreter?data=(" # sopra le 100 query il server ci bloccava
    url <- "https://overpass.kumi.systems/api/interpreter?data=("
    area <- paste0("(around:150,",dati$Latitude[i_row],",",dati$Longitude[i_row], ")")
    
    highwaysQuery <- paste0("node[highway]", area, ";",
                            "way[highway]", area, ";",
                            "relation[highway]", area, ";")
    
    aerowayQuery <- paste0("node[aeroway]", area, ";",
                           "way[aeroway]", area, ";",
                           "relation[aeroway]", area, ";")
    
    railwayQuery <- paste0("node[railway]", area, ";",
                           "way[railway]", area, ";",
                           "relation[railway]", area, ";")
    
    # url_old <- paste0("http://overpass-api.de/api/interpreter?data=(way[","highway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");way[","aeroway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");way[","railway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");>;);out%20meta;")
    
    url <- paste0(url, highwaysQuery, aerowayQuery, railwayQuery, ");(._;>;);out%20meta;")
    osm <- tryCatch(getURLAsynchronous(url), error = function(e) "Error")
    # osm <-getURL(url)
    
    fileConn<-file(paste0(perc, i_row, ".osm"))
    writeLines(osm, fileConn)
    close(fileConn)
    
    latitude <- c(latitude, dati$Latitude[i_row])
    longitude <- c(longitude, dati$Longitude[i_row])
    
    index <- 0
  }
}

dati_fin <- data.frame(
  latitude = latitude,
  longitude = longitude
)

write.csv(dati_fin,file="lat_lon_start.csv" ,row.names=FALSE)

