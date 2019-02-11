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


dati$cityStart <- as.character(dati$cityStart)
dati$cityEnd <- as.character(dati$cityEnd)
dati$stateStart <- as.character(dati$stateStart)
dati$stateEnd <- as.character(dati$stateEnd)
dati$countryEnd <- as.character(dati$countryEnd)
dati[1844, "cityEnd"] <- "Sanya"
dati[1845,"cityStart"] <- "Sanya"
dati[1845,"cityEnd"] <- "Sanya"
dati[3617,"stateStart"] <- "China"
dati[3617,"cityStart"] <- "Aoluo Qixia Sandao"
dati <- dati[-c(3726),] #Unfeasible position



## PREPROCESSING DATA FOR CLASSIFICATION

# delete of all the journey with altitude equals to nan (percentage of value -777 within it > threshold)
dati = dati[!(is.na(dati$altitudeAvg)), ]
# delete of all the journey labelled as BOAT and AIRPLANE classes because we do not have so much istances within the dataset
dati = dati[(dati$label != "airplane"), ]
dati = dati[(dati$label != "boat"), ]
# conversion from factor to character of label attribute
dati$label <- as.character(dati$label)
# anytime there is a "taxi" label replace it with "car"
dati$label[dati$label == "taxi"] <- "car"
#rename the rows with label "run" with "walk"
dati$label[dati$label == "run"] <- "walk"
#rename subway to train (explanation in the documentation)
dati$label[dati$label == "subway"] <- "train"
dati$stateStart <- as.character(dati$stateStart)
dati$stateEnd <- as.character(dati$stateEnd)
# Creation of a new boolean attribute that we'll use to train classifier
dati$state_changed <- dati$stateStart != dati$stateEnd
dati$city_changed <- dati$cityStart != dati$cityEnd
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
  city_changed = dati$city_changed,
  type = dati$tag,
  target = dati$label
)




##CORRELATION MATRIX

#c <- cor(data_correlation)
# corrplot(c, type = "upper",order = "hclust", tl.col = "black", tl.srt = 45)
corr <- hetcor(as.data.frame(data_classification))
ggcorrplot(corr$correlations, outline.col = "white", insig = "pch")
theme(axis.text.x = element_text(size=10, angle=90, vjust=0.5), axis.text.y = element_text(size=10, vjust=0.5))
labs(title = paste("Correlation Matrix"))

#corr <- correlate(data_correlation, test=TRUE, corr.method="pearson", p.adjust.method="holm")
# corrplot(corr,type = "upper",order = "hclust", tl.col = "black", tl.srt = 45)
# cor2(data_correlation)


## SCATTER PLOTS

bus_car <- data_classification[data_classification$target == "bus" | data_classification$target == "car", ]
g1<-ggplot(bus_car,aes(x=vcr,y=sr, shape=target, color=target))+
geom_point(size=3)
grid.arrange(g1,nrow=1,ncol=1,  top = "Scatter plots")




# scrittura dati all'interno del file csv
write.csv(data_classification,file="dataset_final.csv" ,row.names=FALSE) 

