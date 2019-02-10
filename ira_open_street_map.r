# abbiamo opportunamente modificato ira_open_street_map per leggere oltre alle strade
# anche i tag per la ferrovia e gli aereoporti

# grazie a ira_open_street_map otteniamo le informazione della way più vicina con i suoi relativi tag

# per far funzionare questo script bisogna avere ROS (noi abbiamo usato la versione ROS_KINETIC)
# far partire il nodo con il comando:
# rosrun ira_open_street_map osm_query_node nome_mappa.osm

# aprire rstudio da terminale e lanciare questo script

# query per il punto iniziale
point_start <- TRUE

# carichiamo il dataset
perc_csv <- "dataset_with_add_features.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

cond <- c(TRUE, (dati$Id_user[-nrow(dati)] == dati$Id_user[-1]) & (dati$Id_perc[-nrow(dati)] == dati$Id_perc[-1]) & (dati$Label[-nrow(dati)] == dati$Label[-1]))

dim_array <- table(cond)["FALSE"]

Latitude <- vector(mode="double", length=dim_array)
Longitude <- vector(mode="double", length=dim_array)

n_row<- vector(mode="double", length=dim_array)

# creiamo i vettori per le informazioni che otteniamo
highway <- vector(mode="character", length=dim_array)
aeroway <- vector(mode="character", length=dim_array)
railway <- vector(mode="character", length=dim_array)
sidewalk <- vector(mode="character", length=dim_array)

i <- 1
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
    callroslatlon <- "rosservice call /ira_open_street_map/latlon_2_xy \"latitude: "
    
    if(point_start)
    {
      callroslatlon <- paste0(callroslatlon, dati$Latitude[i_row])
      callroslatlon <- paste0(callroslatlon, "\nlongitude: ", dati$Longitude[i_row],"\"")
    }
    else
    {
      callroslatlon <- paste0(callroslatlon, dati$Latitude[i_row - (index%/%2)])
      callroslatlon <- paste0(callroslatlon, "\nlongitude: ", dati$Longitude[i_row - (index%/%2)],"\"")
    }
    
    cordinate <- system(callroslatlon, intern = TRUE)
    cordinate_split <- strsplit(cordinate, " ")
    
    x <- cordinate_split[[1]][2]
    y <- cordinate_split[[2]][2]
    # callrosservice <- "rosservice call /ira_open_street_map/latlon_2_xy \""
    
    callrosSnap <- "rosservice call /ira_open_street_map/snap_particle_xy_2 \"x: "
    callrosSnap <- paste0(callrosSnap, x, "\n", "y: ", y, "\n", "max_distance_radius: 50.0\"")
    
    info_way <- system(callrosSnap, intern = TRUE)
    
    highway[i] <- info_way[14]
    aeroway[i] <- info_way[15]
    railway[i] <- info_way[16]
    sidewalk[i] <- info_way[17]
    
    if(point_start)
    {
      Latitude[i] <- dati$Latitude[i_row]
      Longitude[i] <- dati$Longitude[i_row]
    }
    else
    {
      Latitude[i] <- dati$Latitude[i_row - (index%/%2)]
      Longitude[i] <- dati$Longitude[i_row - (index%/%2)]
    }
    n_row[i] <- i_row
    
    i <- i + 1
    index <- 0
  }
}

dati_fin <- data.frame(
  n_row = n_row,
  Latitude = Latitude,
  Longitude = Longitude,
  highway = highway,
  aeroway = aeroway,
  railway = railway,
  sidewalk = sidewalk
)

write.csv(dati_fin,file="type_road.csv" ,row.names=FALSE)