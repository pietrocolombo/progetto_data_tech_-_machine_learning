# merge osm for easy query with ira_open_street_map

# percorso della cartella
dirs_perc = list.dirs("../osm_s")
# lista contenente i nomi dei file
file = list.files(dirs_perc)
file <- file[file!="Icon\r"]
# percorso dei file
path <- paste(dirs_perc[1], file[1], sep = "/")

# vettore utilizzato per contenere le righe di un singolo file
lines <- c()
# vettore utilizzato per contenere le righe di tutti i file
final_list <- c()

# connessione al file
conn <- file(path, open="r")
lines <- readLines(conn)
close(conn)
# rimozione righe vuote
lines = lines[! lines %in% ""]

lines = lines[1 : (length(lines)-1)]
# unisco tutte le righe in un solo elemento
l <- paste(lines, collapse = '')
# salvo l'elemento nel vettore finale
final_list[1] <- l 

for (i in 2:length(file))
{
  if(i %% 100 == 0)
  {
    print(i)
  }
  
  path <- paste(dirs_perc, file[i], sep = "/")
  # connessione al file  
  conn <- file(path, open="r")
  lines <- readLines(conn)
  close(conn)
  # rimozione righe vuote  
  lines = lines[! lines %in% ""]
  # non considero l'header del file xml 
  l = lines[4 : (length(lines)-1)]
  # unisco tutte le righe in un solo elemento
  l <- paste(l, collapse = '')
  # salvo l'elemento nel vettore finale
  final_list[i] <- l 
}  

final_list[i+1] <- lines[length(lines)]
# scrivo nel file mappa.osm l'unione di tutti i file
lapply(final_list, write, "mappa_S.osm", append=TRUE, ncolumns=1000)
