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
if(!require(e1071)){
  install.packages("e1071")
  library("e1071")
}






perc_csv <- "dataset_compresso_info_city_V3.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

# delete of all the journey with altitude equals to nan (percentage of value -777 within it > threshold)
dati <- dati[!(is.na(dati$altitudeAvg)), ]

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

normalize(dati, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
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


## CORRELATION MATRIX EVALUATION
data_correlation <- data.frame(
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


c <- cor(data_correlation)
corrplot(c, type = "upper",order = "hclust", tl.col = "black", tl.srt = 45)

n <- nrow(dati)
ntrain <- round(n*0.8)

repeat{
p <- 0.7
sample <- sample.int(n = nrow(data_classification), size = floor(p * nrow(data_classification)), replace = FALSE)
training_set <- data_classification[sample, ]
label_training <- training_set$target
test_set <- data_classification[-sample, ]
label_test <- test_set$target
if(length(levels(label_training)) == 8  &&  length(levels(label_test)) == 8 )
  break
}

# view dimension of training and test set
dim(training_set)
dim(test_set)

# any null value in data_classification? if it's FALSE it's good ;)
anyNA(data_classification)


svm1 <- svm(target~., data = training_set,
            method = "C-classification",
            kernel = "radial",
            scale = c("center","scale"),
            gamma = 1,
            cost = 10)

predict <- predict(svm1, test_set)
cm <- confusionMatrix(predict,test_set$target)
cm


