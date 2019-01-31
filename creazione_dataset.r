
# acquisizione dati dai file plt per non perdere cifre decimali nei dati acquisisco i dati come carattere

dati <- read.table("Geolife Trajectories 1.3/Data/000/Trajectory/20081023025304.plt", header = FALSE, quote = "\"", skip = 6, sep = ",", colClasses = c("character", "character", "character", "character", "character", "character", "character") , numerals = "no.loss")

# converto le colonne numeriche in double
options(digits=10)
for(i in c(1,3:ncol(dati))) {
  dati[,i] <- as.double(dati[,i])
  