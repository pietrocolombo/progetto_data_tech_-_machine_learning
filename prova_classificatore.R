
# decomment the line below if this is the first time you are running this script
# the caret package it's useful to implement Support Vector Machine classifier


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



perc_csv <- "dataset_compresso_info_city_11_47.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

# delete of all the journey with altitude equals to nan (percentage of value -777 within it > threshold)
dati_prova = dati[(!dati$altitudeAvg) != "NA", ]

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



# reconvert from character to factor
dati$label <- as.factor(dati$label)
dati$stateStart <- as.factor(dati$stateStart)
dati$stateEnd <- as.factor(dati$stateEnd)



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



# In this section we make sure that the sample of the training set give us all the all 8 labels
repeat{
# function useful to keep trace of the division (3033 is a random nummber, idk why)
set.seed(3033)
# variable to partition dataset
intrain <- createDataPartition(y = data_classification$target, p = 0.7, list = FALSE)
training_set <- data_classification[intrain, ]
label_training <- training_set$target
test_set <- data_classification[-intrain, ]

if(length(levels(label_training)) == 8)
  break
}


dati$label[dati$label == "taxi"] <- "car"
training_wo_string <- training_set 

corr <- hetcor(training_set)
# training_wo_string$target <- as.character(training_wo_string$target)
# training_wo_string$target[training_wo_string$target == "walk"] <- "1"
# training_wo_string$target[training_wo_string$target == "bus"] <- "2"
# training_wo_string$target[training_wo_string$target == "car"] <- "3"
# training_wo_string$target[training_wo_string$target == "bike"] <- "4"
# training_wo_string$target[training_wo_string$target == "subway"] <- "5"
# training_wo_string$target[training_wo_string$target == "train"] <- "6"
# training_wo_string$target[training_wo_string$target == "boat"] <- "7"
# training_wo_string$target[training_wo_string$target == "airplane"] <- "8"
# training_wo_string$target <- as.integer(training_wo_string$target)



# view dimension of training and test set
dim(training_wo_string)
dim(test_set)

# any null value in data_classification? if it's FALSE it's good ;)
anyNA(data_classification)
#plot(cor(training_wo_string))
correlation <- rquery.cormat(training_wo_string)

M <-cor(training_wo_string)
corrplot(M, type="upper", order="hclust",
         col=brewer.pal(n=8, name="RdYlBu"))



training_wo_string[["target"]] = factor(training_wo_string[["target"]])

# Training phase

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
set.seed(3233)

svm_Linear <- train(target ~., data = training_wo_string, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

# Now I try to tune C parameter to obtain a better accuracy
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

svm_Linear_Grid <- train(target ~., data = training_wo_string, method = "svmLinear",
                         trControl=trctrl,
                         preProcess = c("center", "scale"),
                         tuneGrid = grid,
                         tuneLength = 10)
plot(svm_Linear_Grid)

# Test phase

test_pred <- predict(svm_Linear, newdata = test_set)
test_pred

test_pred_grid <- predict(svm_Linear_Grid, newdata = test_set)
test_pred_grid

cm <- confusionMatrix(test_pred,test_set$target)
cm_grid <-confusionMatrix(test_pred_grid,test_set$target)








