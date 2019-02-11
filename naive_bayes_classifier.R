# Naive Bayes
if(!require(e1071)){
  install.packages("e1071")
  library("e1071")
}

if(!require(caret)){
  install.packages('caret', dependencies = TRUE)
  library("caret")
}





#training_set[["target"]] = factor(training_set[["target"]])

# Training phase

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# x = training_set[, 1:11]
# y = training_set[, 12]

naive_bayes_model=train(target ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type,
                        data = training_set,
                        preProcess= c("center","scale"),
                        method = 'nb',
                        trControl=trctrl)

test_pred <- predict(naive_bayes_model, newdata = test_set)
cm <- confusionMatrix(test_pred, test_set$target)
conf <- table(test_pred, test_set$target)

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
