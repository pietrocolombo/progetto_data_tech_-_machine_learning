#script for download osm map


perc_csv <- "../progetto_data_tech_&_machine_learning_dataset/cordinate.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

perc <- "./progetto_data_tech_&_machine_learning_dataset/osm/"

for(i_row in 1:nrow(dati))
{
  if(i_row %% 10000 == 0)
  {
    print(i_row)
  }
  url <- paste0("http://overpass-api.de/api/interpreter?data=(way[","highway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");way[","aeroway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");way[","railway","](around:100,",dati$Latitude[i_row],",",dati$Longitude[i_row],");way(r);>;);out%20meta;")
  osm <-getURL(url)
  
  fileConn<-file(paste0(perc,i_row,".osm"))
  writeLines(osm, fileConn)
  close(fileConn)
}

