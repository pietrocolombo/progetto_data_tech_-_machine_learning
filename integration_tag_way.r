


# aggiungiamo le informazioni della strada
perc_middle <- "type_road_middle.csv"
dati_middle <- read.csv(perc_middle, header = TRUE, sep =",", quote = "\"", dec = ".")

dati_middle$highway <- as.character(dati_middle$highway)
dati_middle$aeroway <- as.character(dati_middle$aeroway)
dati_middle$railway <- as.character(dati_middle$railway)
dati_middle$sidewalk <- as.character(dati_middle$sidewalk)

perc_start <- "type_road_start.csv"
dati_start <- read.csv(perc_start, header = TRUE, sep =",", quote = "\"", dec = ".")

dati_start$highway <- as.character(dati_start$highway)
dati_start$aeroway <- as.character(dati_start$aeroway)
dati_start$railway <- as.character(dati_start$railway)
dati_start$sidewalk <- as.character(dati_start$sidewalk)

perc_csv <- "dataset_compresso_info_city.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

tag_way <- vector(mode="character", length=nrow(dati))
val_tag_way <- vector(mode="character", length=nrow(dati))
sidewalk <- vector(mode="character", length=nrow(dati))

for(i_row in 1:nrow(dati))
{
  
  highway_start_split <- strsplit(dati_start$highway[i_row], " ")
  aeroway_start_split <- strsplit(dati_start$aeroway[i_row], " ")
  railway_start_split <- strsplit(dati_start$railway[i_row], " ")
  sidewalk_start_split <- strsplit(dati_start$sidewalk[i_row], " ")
  
  highway_middle_split <- strsplit(dati_middle$highway[i_row], " ")
  aeroway_middle_split <- strsplit(dati_middle$aeroway[i_row], " ")
  railway_middle_split <- strsplit(dati_middle$railway[i_row], " ")
  sidewalk_middle_split <- strsplit(dati_middle$sidewalk[i_row], " ")
  
  
  
  if(highway_start_split[[1]][2] == "\'\'" | is.na(dati_start$highway[i_row]))
  {
    if(aeroway_start_split[[1]][2] == "\'\'" | is.na(dati_start$aeroway[i_row]))
    {
      if(railway_start_split[[1]][2] == "\'\'" | is.na(dati_start$railway[i_row]))
      {
        if(sidewalk_start_split[[1]][2] == "\'\'" | is.na(dati_start$sidewalk[i_row]))
        {
          if(highway_middle_split[[1]][2] == "\'\'" | is.na(dati_middle$highway[i_row]))
          {
            if(aeroway_middle_split[[1]][2] == "\'\'" | is.na(dati_middle$aeroway[i_row]))
            {
              if(railway_middle_split[[1]][2] == "\'\'" |  is.na(dati_middle$railway[i_row]))
              {
                if(sidewalk_middle_split[[1]][2] == "\'\'" | is.na(dati_middle$sidewalk[i_row]))
                {
                  
                }
                else
                {
                  if(!is.na(dati_middle$sidewalk[i_row]))
                  {
                    sidewalk[i_row] <- sidewalk_middle_split[[1]][2]
                  }
                }
              }
              else
              {
                if(!is.na(dati_middle$railway[i_row]))
                {
                  val_tag_way[i_row] <- railway_middle_split[[1]][2]
                  tag_way[i_row] <- railway_middle_split[[1]][1]
                }
              }
            }
            else
            {
              if(!is.na(dati_middle$aeroway[i_row]))
              {
                val_tag_way[i_row] <- aeroway_middle_split[[1]][2]
                tag_way[i_row] <- aeroway_middle_split[[1]][1]
              }
            }
          }
          else
          {
            if(!is.na(dati_middle$highway[i_row]))
            {
              val_tag_way[i_row] <- highway_middle_split[[1]][2]
              tag_way[i_row] <- highway_middle_split[[1]][1]
            }
          }
        }
        else
        {
          if(!is.na(dati_start$sidewalk[i_row]))
          {
            sidewalk[i_row] <- sidewalk_start_split[[1]][2]
          }
        }
      }
      else
      {
        if(!is.na(dati_start$railway[i_row]))
        {
          val_tag_way[i_row] <- railway_start_split[[1]][2]
          tag_way[i_row] <- railway_start_split[[1]][1]
        }
      }
    }
    else
    {
      if(!is.na(dati_start$aeroway[i_row]))
      {
        val_tag_way[i_row] <- aeroway_start_split[[1]][2]
        tag_way[i_row] <- aeroway_start_split[[1]][1]
      }
    }
  }
  else
  {
    if(!is.na(dati_start$highway[i_row]))
    {
      val_tag_way[i_row] <- highway_start_split[[1]][2]
      tag_way[i_row] <- highway_start_split[[1]][1]
    }
  }
}

dati$val_tag_way <- val_tag_way
dati$tag_way <- tag_way
dati$sidewalk <- sidewalk

write.csv(dati,file="dataset_compresso_info_city_with_tag.csv" ,row.names=FALSE)


# uniformiamo i tag

tag <- vector(mode="character", length=nrow(dati))
for(i_row in 1:nrow(dati))
{
  if(tag_way[i_row] == "aeroway:")
  {
    tag[i_row] <- "aeroway"
  }
  else
  {
    if(tag_way[i_row] == "railway:")
    {
      tag[i_row] <- "railway"
    }
    else
    {
      if(val_tag_way[i_row] == "\"footway\"" || val_tag_way[i_row] == "\"pedestrian\"" || val_tag_way[i_row] == "\"runway\"" || val_tag_way[i_row] == "\"steps\"")
      {
        tag[i_row] <- "pedestrian"
      }
      else
      {
        if(val_tag_way[i_row] == "\"cycleway\"")
        {
          tag[i_row] <- "cycleway"
        }
        else
        {
          tag[i_row] <- "road"
        }
      }
    }
  }
}

perc_csv <- "dataset_compresso_info_city.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

dati$tag <- tag

write.csv(dati,file="dataset_compresso_info_city_simple_tag.csv" ,row.names=FALSE)

