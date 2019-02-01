
# acquisizione dati dai file plt per non perdere cifre decimali nei dati acquisisco i dati come carattere

# prendo il percorso di ogni cartella
dirs_perc = list.dirs("../progetto_data_tech_&_machine_learning_dataset/Geolife Trajectories 1.3/Data", recursive = FALSE)
# prendo il nome di ogni cartella la quale mi identifica l'utente
dirs_name = list.files("../progetto_data_tech_&_machine_learning_dataset/Geolife Trajectories 1.3/Data")

first_time = TRUE
file.remove("dataset.csv")

for (i_dirs in 1:length(dirs_perc)){
  print(i_dirs)
  # guardo il numedo di file nella cartella se è 3 ho le label
  file = list.files(dirs_perc[i_dirs])
  if (length(file) == 3) {
    # leggo il file delle label
    label <- read.table(paste(dirs_perc[i_dirs],"/labels.txt", sep = ""), quote = "\"", sep = "\t", header = TRUE )
    # ottendo il percorso delle traiettorie
    trajectory_perc <- paste(dirs_perc[i_dirs],"/Trajectory", sep = "")
    file_trajectory <- list.files(trajectory_perc)
    index_file <- grep(".plt",file_trajectory)
    
    # uniformo le date per avere poterle confrontare
    label$Start.Time.Posix <- as.POSIXct(label$Start.Time, format="%Y/%m/%d %H:%M:%OS")
    label$End.Time.Posix <- as.POSIXct(label$End.Time, format="%Y/%m/%d %H:%M:%OS")
    for (index in index_file){
      dati <- read.table(paste(trajectory_perc, "/", file_trajectory[index], sep = ''), header = FALSE, quote = "\"", skip = 6, sep = ",", colClasses = c("character", "character", "character", "character", "character", "character", "character") , numerals = "no.loss")
      # uniformo le date per avere poterle confrontare
      dati$Time <- as.POSIXct(paste(dati$V6, dati$V7, sep = " "),format="%Y-%m-%d %H:%M:%OS")
      
      # verifico che abbiamo le label riferenti al file delle traiettorie
      # quindi guardo nella tabella delle label se ho una traiettoria con la label che parte con lo stesso timestamps
      find_label <- grep(dati$Time[1], label$Start.Time.Posix)
      if (length(find_label) != 0){
        if(length(find_label) > 1){
          print(paste("ho trovato più label per uno stesso percorso è strano", length(find_label), " nome del file ", file_trajectory[index], " directory name ", dirs_name[i_dirs], " indice for ", i_dirs))
        }

        # se ho trovato una label che corrisponde al percorso
        
        # elimino le colonne che non mi servono
        # che sono la collonna 3 e 5
        # poichè v3 è sempre a 0 e V5 rapresenta i giorni da una certa data ma abbiamo la data direttamente.
        dati$V3 <- NULL
        dati$V5 <- NULL
        # cambio i nomi alle colonne con nomi più significativi
        colnames(dati) <- c("Latitude", "Longitude", "Altitude", "Date", "Time")
        options(digits=10)
        for(i in 1:3) {
          dati[,i] <- as.double(dati[,i])
        }
        # aggiungo nel data frame dati la colonna della label
        dati$Label <- label$Transportation.Mode[find_label[1]]
        # aggiungo id dell'utente
        dati$Id_user <- dirs_name[i_dirs]
        if(first_time){
          #data_Trajectorys <- dati
          write.table(dati,file="dataset.csv", append=TRUE,sep=",",row.names=FALSE) 
          first_time = FALSE
        }else{
          #data_Trajectorys <- rbind(data_Trajectorys, dati)
          write.table(dati,file="dataset.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
        }
      }
    }
  }
}
