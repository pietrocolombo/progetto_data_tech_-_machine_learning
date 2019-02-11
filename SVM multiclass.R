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

#c <- cor(data_correlation)
# corrplot(c, type = "upper",order = "hclust", tl.col = "black", tl.srt = 45)
corr <- hetcor(as.data.frame(data_classification))
ggcorrplot(corr$correlations, outline.col = "white", insig = "pch")
theme(axis.text.x = element_text(size=10, angle=90, vjust=0.5), axis.text.y = element_text(size=10, vjust=0.5))
labs(title = paste("Correlation Matrix"))

#corr <- correlate(data_correlation, test=TRUE, corr.method="pearson", p.adjust.method="holm")
# corrplot(corr,type = "upper",order = "hclust", tl.col = "black", tl.srt = 45)
# cor2(data_correlation)

# library(plyr)
# 
# 
# folds <- split(dati, cut(sample(1:nrow(dati)),10))
# accuracy <- rep(NA,length(folds))
# 
# for(i in 1:length(folds))
# {
#   repeat{
#   test <- ldply(folds[i],data.frame) # convert to data frame
#   train <- ldply(folds[-i],data.frame) # convert to data frame
#   test <- test[,-1]
#   train <- train[,-1]
#   label_training <- train$target
#   label_test <- test$target
#   }
#   test[["target"]] = factor(test[["target"]])
#   train[["target"]] = factor(train[["target"]])
#   svm_model <- svm(target~., data = train,
#               method = "C-classification",
#               kernel = "radial",
#               preProcess = c("center","scale"), #scaling values for svm
#               gamma = 0.5,
#               cost = 10)
#   pred <- predict(svm_model, test )
#   conf_mat <- confusionMatrix(pred,test$target)
#   accuracy[i] <- conf_mat$
# 
# 
# 
# }



# SPLIT DATASET INTO TEST (30%) AND TRAINING (70%)

repeat{
p <- 0.7
sample <- sample.int(n = nrow(dati), size = floor(p * nrow(dati)), replace = FALSE)
training_set <- dati[sample, ]
label_training <- training_set$target
test_set <- dati[-sample, ]
label_test <- test_set$target
if(length(levels(label_training)) == 5  &  length(levels(label_test)) == 5 )
  break
}

training_set[["target"]] = factor(training_set[["target"]])
# view dimension of training and test set
dim(training_set)
dim(test_set)

# any null value in data_classification? if it's FALSE it's good ;)
anyNA(data_classification)


# repeatedcv performs a balanced folds creation
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10) #10 fold cross validation 


svm_Radial <- svm(target~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type , 
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



accuracy
precision
recall
f1


# svm_Linear <- train(target ~., data = data_classification, 
#               method = "svmLinear",
#               trControl=trctrl,
#               preProcess = c("center", "scale")) #scaling values for svm

# predict on test set with svm, linear
# predict_linear <- predict(svm_Linear, test_set)
# cm_linear <- confusionMatrix(predict_linear, test_set$target)
# cm_linear



