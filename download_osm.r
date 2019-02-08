#script for download osm map
if(!require(RCurl)){
  install.packages("RCurl")
  library("RCurl")
}

perc_csv <- "dataset_with_add_features.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

perc <- "osm/"

for(i_row in 2486:nrow(dati))
{
  if(i_row %% 10000 == 0)
  {
    print(i_row)
  }
  url <- "http://overpass-api.de/api/interpreter?data=("
  
  area <- paste0("(around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row], ")")
  
  highwaysQuery <- paste0("node[highway]", area, ";",
                          "way[highway]", area, ";",
                          "relation[highway]", area, ";")
  
  aerowayQuery <- paste0("node[aeroway]", area, ";",
                          "way[aeroway]", area, ";",
                          "relation[aeroway]", area, ";")
  
  railwayQuery <- paste0("node[railway]", area, ";",
                         "way[railway]", area, ";",
                         "relation[railway]", area, ";")
  
   url <- paste0("http://overpass-api.de/api/interpreter?data=(way[","highway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");way[","aeroway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");way[","railway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");>;);out%20meta;")
  
  url <- paste0(url, highwaysQuery, aerowayQuery, railwayQuery, ";(._;>;);out%20meta;")
  data <- tryCatch(getURLAsynchronous(url), error = function(e) "Error")
  osm <-getURL(url)
  
  fileConn<-file(paste0(perc,i_row,".osm"))
  writeLines(osm, fileConn)
  close(fileConn)
}

