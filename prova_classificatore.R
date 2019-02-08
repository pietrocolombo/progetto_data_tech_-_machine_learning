
# decomment the line below if this is the first time you are running this script
# the caret package it's useful to implement Support Vector Machine classifier


if(!require(caret)){
  install.packages("caret")
  library("caret")
}


perc_csv <- "dataset_compresso_info_city.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

## PREPROCESSING DATA 

# conversion from factor to character
dati$label <- as.character(dati$label)
# anytime there is a "taxi" label replace it with "car"
dati$label[dati$label == "taxi"] <- "car"

#delete the rows with label "run" (there are only three rows with that label, not significantly importants for our model)
dati <- dati[dati$label != "run", ]
#delete the rows with label "boat" for the same reasons
dati <- dati[dati$label !="boat", ]
dati$stateStart <- as.character(dati$stateStart)
dati$stateEnd <- as.character(dati$stateEnd)

# Creation of a new boolean attribute that we'll use to train classifier
dati$state_changed <- dati$stateStart != dati$stateEnd

# reconvert from character to factor
dati$label <- as.factor(dati$label)

#Classification table

data_classification <- data.frame(
  vcr = dati$vcr,
  sr = dati$sr,
  hcr = dati$hcr,
  vel_max = dati$vel_max,
  tot_duration = dati$time_total,
  to_distance = dati$distanceTotal,
  state_changed = dati$state_changed,
  target = dati$label
)


# function useful to keep trace of the division (3033 is a random nummber, idk why)
set.seed(3033)
# variable to partition dataset
intrain <- createDataPartition(y = data_classification$target, p = 0.7, list = FALSE)
training_set <- data_classification[intrain, ]
test_set <- data_classification[-intrain, ]

# view dimension of training and test set
dim(training_set)
dim(test_set)

# any null value in data_classification? if it's FALSE it's good ;)
anyNA(data_classification)

training_set[["target"]] = factor(training_set[["target"]])

# Training phase

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
set.seed(3233)

svm_Linear <- train(target ~., data = training_set, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

# Now I try to tune C parameter to obtain a better accuracy
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))

svm_Linear_Grid <- train(target ~., data = training_set, method = "svmLinear",
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








