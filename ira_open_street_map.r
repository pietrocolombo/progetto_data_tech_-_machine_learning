
perc_csv <- "dataset_with_add_features.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

cond <- c(TRUE, (dati$Id_user[-nrow(dati)] == dati$Id_user[-1]) & (dati$Id_perc[-nrow(dati)] == dati$Id_perc[-1]) & (dati$Label[-nrow(dati)] == dati$Label[-1]))

dim_array <- table(cond)["FALSE"]

Latitude <- vector(mode="double", length=dim_array)
Longitude <- vector(mode="double", length=dim_array)

n_row<- vector(mode="double", length=dim_array)


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
    
    callroslatlon <- paste0(callroslatlon, dati$Latitude[i_row - (index%/%2)])
    callroslatlon <- paste0(callroslatlon, "\nlongitude: ", dati$Longitude[i_row - (index%/%2)],"\"")
    
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
    
    Latitude[i] <- dati$Latitude[i_row - (index%/%2)]
    Longitude[i] <- dati$Longitude[i_row - (index%/%2)]
    
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