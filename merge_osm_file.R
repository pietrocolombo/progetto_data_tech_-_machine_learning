
# merge osm for easy query with ira_open_street_map

if(!require(readtext)){
  install.packages("readtext")
  library("readtext")
}

dirs_perc = list.dirs("../progetto_data_tech_&_machine_learning_dataset/osm_n")

file = list.files(dirs_perc)
file <- file[file!="Icon\r"]

for (i_dirs in 1:length(file))
{
  DATA_DIR <- system.file(paste0(dirs_perc, "/", file[1]), package = "readtext")
}  
  