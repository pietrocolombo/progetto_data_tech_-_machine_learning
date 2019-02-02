
# acquisizione dati dai file plt per non perdere cifre decimali nei dati acquisisco i dati come carattere

# prendo il percorso di ogni cartella
dirs_perc = list.dirs("../progetto_data_tech_&_machine_learning_dataset/Geolife Trajectories 1.3/Data", recursive = FALSE)
# prendo il nome di ogni cartella la quale mi identifica l'utente
dirs_name = list.files("../progetto_data_tech_&_machine_learning_dataset/Geolife Trajectories 1.3/Data")

first_time = TRUE
file.remove("dataset.csv")

for (i_dirs in 1:length(dirs_perc)){
  print(i_dirs)
  # guardo il numedo di file nella cartella se è 3 ho le label poichè il mac vede un file Icon\r
  file = list.files(dirs_perc[i_dirs])
  file <- file[file!="Icon\r"]
  # se ho due file vuol dire che ho anche le label
  if (length(file) == 2) {
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
      dati$Date_Time <- as.POSIXct(paste(dati$V6, dati$V7, sep = " "),format="%Y-%m-%d %H:%M:%OS")
      
      # elimino le colonne che non mi servono
      # che sono la collonna 3 e 5
      # poichè v3 è sempre a 0 e V5 rapresenta i giorni da una certa data ma abbiamo la data direttamente.
      dati$V3 <- NULL
      dati$V5 <- NULL
      # cambio i nomi alle colonne con nomi più significativi
      colnames(dati) <- c("Latitude", "Longitude", "Altitude", "Date", "Time", "Date_Time")
      # elimino Date e Time avengo già Date_Time
      dati$Date <- NULL
      dati$Time <- NULL
      
      options(digits=10)
      for(i in 1:3) {
        dati[,i] <- as.double(dati[,i])
      }
      
      # aggiungo id dell'utente
      dati$Id_user <- dirs_name[i_dirs]
      # aggiungo i id che identifica il percorso
      dati$Id_perc <- file_trajectory[index]
      
      # aggiungo la colonna per la label
      dati$Label <- NA
      
      # verifico che abbiamo le label riferenti al file delle traiettorie
      # quindi guardo nella tabella delle label se ho una traiettoria con la label che parte con lo stesso timestamps
      
      # siccome per ogni file plt ci possono essere più label
      # ciclo per ogni file su tutti gli elementi delle label
      for(i_row_label in 1:nrow(label))
      {
        # print(paste(i_row_label, " numero righe label ", nrow(label)))
        find_label <- grep(label$Start.Time.Posix[i_row_label], dati$Date_Time)
        if(length(find_label) != 0)
        {
          find_label_end <- grep(label$End.Time.Posix[i_row_label], dati$Date_Time)
          if(length(find_label_end) != 0)
          {
            print("Matching")
            if(length(find_label) > 1){
              print(paste("ho trovato più label per uno stesso percorso è strano", length(find_label), " nome del file ", file_trajectory[index], " directory name ", dirs_name[i_dirs], " indice for ", i_dirs))
            }
            # se ho trovato una label che corrisponde al percorso
            # metto la label solo per il tratto di percorso della label corrispondente
            dati$Label[find_label[1] : find_label_end[1]] <- label$Transportation.Mode[i_row_label]
            
            # salvo in un nuovo dataframe il tratto del percorso corrisponedente
            dati2 <- dati[find_label[1] : find_label_end[1],]
            
            # salvo i dati in un file csv
            if(first_time){
              #data_Trajectorys <- dati
              write.table(dati2,file="dataset.csv", append=TRUE,sep=",",row.names=FALSE) 
              first_time = FALSE
            }else{
              # rbind ci da problemi in velocità per la gestione della memoria che deve trovare uno spazio continuo
              #data_Trajectorys <- rbind(data_Trajectorys, dati)
              write.table(dati2,file="dataset.csv", append=TRUE,sep=",",col.names=FALSE,row.names=FALSE)
            }
          }
        }
      }
    }
  }
}
