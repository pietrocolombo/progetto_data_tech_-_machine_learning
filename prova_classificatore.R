
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

features_analysis <- data.frame(
  vcr = dati$vcr,
  sr = dati$sr,
  hcr = dati$hcr,
  vel_max = dati$vel_max,
  vel_avg = dati$vel_avg,
  altitude_max = dati$altitudeMax,
  altitude_avg = dati$altitudeAvg,
  tot_duration = dati$time_total,
  tot_distance = dati$distanceTotal,
  state_changed = dati$state_changed
)




# bus_car <- data_classification[data_classification$target == "airplane" | data_classification$target == "bike", ]
# g1<-ggplot(bus_car,aes(x=vcr,y=sr, shape=target, color=target))+
# geom_point(size=3)
# grid.arrange(g1,nrow=1,ncol=1,  top = "Scatter plots")
# 


# par(mar=c(1,1,1,1))
# pairs(plane_car[,1:5],
#       lower.panel=NULL)
# pairs(plane_car[,5:10],
#       lower.panel=NULL)



## CORRELATION MATRIX EVALUATION
# data_classification <- as.integer(data_classification)
# head(data_classification)
# correlation_matrix <- data_classification[,2:length(data_classification)]
# c <- cor(correlation_matrix)
# corrplot(c, type = "upper", 
#          order = "hclust", tl.col = "black", tl.srt = 45)

# In this section we make sure that the sample of the training set give us all the all 11 labels
repeat{
  # function useful to keep trace of the division (3033 is a random nummber, idk why)
  set.seed(3033)
  # variable to partition dataset
  # intrain <- createDataPartition(y = data_classification$target, p = 0.7, list = FALSE)
  # training_set <- data_classification[intrain, ]
  # label_training <- training_set$target
  # test_set <- data_classification[-intrain, ]
  
  # SPLIT DATASET
  p = 0.8
  sample = sample.int(n = nrow(data_classification), size = floor(p * nrow(data_classification)), replace = FALSE)
  training_set = data_classification[sample, ]
  label_training <- training_set$target
  test_set = data_classification[-sample, ]
  
  if(length(levels(label_training)) == 8)
    break
}


# view dimension of training and test set
dim(training_set)
dim(test_set)

# any null value in data_classification? if it's FALSE it's good ;)
anyNA(data_classification)



training_set[["target"]] = factor(training_set[["target"]])

# Training phase

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(3233)

svm_Linear <- train(target ~., data = training_set, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)



# Test phase
test_pred <- predict(svm_Linear, newdata = test_set)
test_pred
cm <- confusionMatrix(test_pred,test_set$target)
#cm_grid <-confusionMatrix(test_pred_grid,test_set$target)
cm
