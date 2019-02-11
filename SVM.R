if(!require(corrplot)){
  install.packages("corrplot")
  library("corrplot")
}
if(!require(ggcorrplot)){
  install.packages("ggcorrplot")
  library("ggcorrplot")
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
if(!require(caret)){
  install.packages("caret")
  library("caret")
}



perc_csv <- "dataset_final.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

introduce(dati)
plot_intro(dati)
plot_bar(dati)




training_set[["target"]] = factor(training_set[["target"]])
# view dimension of training and test set
dim(training_set)
dim(test_set)
# any null value in data_classification? if it's FALSE it's good ;)
anyNA(data_classification)


# repeatedcv performs a balanced folds creation
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10) #10 fold cross validation 
svm_Radial <- svm(target~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + city_changed + type , 
              data = training_set,
              kernel = "radial",
              method = "C-classification",
              preProcess = c("center","scale"), #scaling values for svm
              trControl=trctrl,
              cost = 10)


# predict on test set with svm, radial kernel
predict_radial <- predict(svm_Radial, test_set)
cm_radial <- confusionMatrix(predict_radial,test_set$target)
# view statistic on test set
cm_radial

conf <- table(predict_radial, test_set$target)
accuracy <- sum(diag(conf)) / sum(conf)
# precision is defined as the fraction of correct predictions for a certain class
precision <- diag(conf) / rowSums(conf)
# recall is the fraction of instances of a class that were correctly predicted
recall <- (diag(conf) / colSums(conf))
# F-1 score is defined as the harmonic mean (or a weighted average) of precision and recall
f1 = 2 * precision * recall / (precision + recall)

accuracy #is the medium accuracy from 10 fold validation, repeated ten times, 
precision
recall
f1


# svm_Linear <- train(target ~., data = data_classification, 
#               method = "svmLinear",
#               trControl=trctrl,
#               preProcess = c("center", "scale")) #scaling values for svm



training_set[["target"]] = factor(training_set[["target"]])

# Training phase

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10, savePredictions = TRUE)
set.seed(3233)
svm_Linear <- train(target ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration +
                    tot_distance + state_changed + city_changed + type, 
                    data = training_set, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10,
                    metric = "Accuracy")
# predict on test set with svm, linear
predict_linear <- predict(svm_Linear, test_set)
cm_linear <- confusionMatrix(predict_linear, test_set$target)
cm_linear


conf_lin <- table(predict_linear, test_set$target)
accuracy_lin <- sum(diag(conf_lin)) / sum(conf_lin)
# precision is defined as the fraction of correct predictions for a certain class
precision_lin <- diag(conf_lin) / rowSums(conf_lin)
# recall is the fraction of instances of a class that were correctly predicted
recall_lin <- (diag(conf_lin) / colSums(conf_lin))
# F-1 score is defined as the harmonic mean (or a weighted average) of precision and recall
f1_lin = 2 * precision_lin * recall_lin / (precision_lin + recall_lin)

accuracy_lin #is the medium accuracy from 10 fold validation, repeated ten times, 
precision_lin
recall_lin
f1_lin

