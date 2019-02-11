# DATA PREPARATION FOR CLASSIFICATION PHASE


source("http://www.sthda.com/upload/rquery_cormat.r")
if(!require(caret)){
  install.packages("caret")
  library("caret")
}
if(!require(corrplot)){
  install.packages("corrplot")
  library("corrplot")
}
if(!require(colorspace)){
  install.packages("colorspace")
  library("colorspace")
}
if(!require(polycor)){
  install.packages("polycor")
  library("polycor")
}
if(!require(DataExplorer)){
  install.packages("DataExplorer")
  library("DataExplorer")
}
if(!require(Hmisc)){
  install.packages("Hmisc")
  library("Hmisc")
}
if(!require(RGraphics)){
  install.packages("RGraphics")
  library("RGraphics")
}
if(!require(grid)){
  install.packages("grid")
  library("grid")
}
if(!require(gridExtra)){
  install.packages("gridExtra")
  library("gridExtra")
}



perc_csv <- "dataset_compresso_info_city_simple_tag.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

# delete of all the journey with altitude equals to nan (percentage of value -777 within it > threshold)
dati = dati[!(is.na(dati$altitudeAvg)), ]

## PREPROCESSING DATA 

# conversion from factor to character of label attribute
dati$label <- as.character(dati$label)
# anytime there is a "taxi" label replace it with "car"
dati$label[dati$label == "taxi"] <- "car"
#rename the rows with label "run" with "walk"
dati$label[dati$label == "run"] <- "walk"

dati$stateStart <- as.character(dati$stateStart)
dati$stateEnd <- as.character(dati$stateEnd)
# Creation of a new boolean attribute that we'll use to train classifier
dati$state_changed <- dati$stateStart != dati$stateEnd

dati$label[dati$label == "taxi"] <- "car"

# reconvert from character to factor
dati$label <- as.factor(dati$label)
dati$stateStart <- as.factor(dati$stateStart)
dati$stateEnd <- as.factor(dati$stateEnd)

## data analisys

# introduce(dati)
# plot_intro(dati)
# plot_bar(dati)

#Creation of the table used for classification phase

data_classification <- data.frame(
  vcr = dati$vcr,
  sr = dati$sr,
  hcr = dati$hcr,
  vel_max = dati$vel_max,
  vel_avg = dati$vel_avg,
  altitude_max = dati$altitudeMax,
  altitude_avg = dati$altitudeAvg,
  tot_duration = dati$time_total,
  tot_distance = dati$distanceTotal,
  state_changed = dati$state_changed,
  target = dati$label
)
# 
# features_analysis <- data.frame(
#   vcr = dati$vcr,
#   sr = dati$sr,
#   hcr = dati$hcr,
#   vel_max = dati$vel_max,
#   vel_avg = dati$vel_avg,
#   altitude_max = dati$altitudeMax,
#   altitude_avg = dati$altitudeAvg,
#   tot_duration = dati$time_total,
#   tot_distance = dati$distanceTotal,
#   state_changed = dati$state_changed
# )



# scrittura dati all'interno del file csv
write.csv(data_classification,file="dataset_final.csv" ,row.names=FALSE) 

