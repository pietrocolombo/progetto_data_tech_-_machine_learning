# ROC EVALUATION
if(!require(pROC)){
  install.packages("pROC")
  library("pROC")
}

#Binarization of the classes

training_set$is_walk <- as.numeric(training_set$target == 'walk')
test_set$is_walk <- as.numeric(test_set$target == 'walk')

training_set$is_car <- as.numeric(training_set$target == 'car')
test_set$is_car <- as.numeric(test_set$target == 'car')

training_set$is_bus <- as.numeric(training_set$target == 'bus')
test_set$is_bus <- as.numeric(test_set$target == 'bus')

training_set$is_airplane <- as.numeric(training_set$target == 'airplane')
test_set$is_airplane <- as.numeric(test_set$target == 'airplane')

training_set$is_bike <- as.numeric(training_set$target == 'bike')
test_set$is_bike <- as.numeric(test_set$target == 'bike')

training_set$is_boat<- as.numeric(training_set$target == 'boat')
test_set$is_boat <- as.numeric(test_set$target == 'boat')

training_set$is_subway <- as.numeric(training_set$target == 'subway')
test_set$is_subway <- as.numeric(test_set$target == 'subway')

training_set$is_train <- as.numeric(training_set$target == 'train')
test_set$is_train <- as.numeric(test_set$target == 'train')


library(e1071)


# Evaluating models for each class
mod_walk = svm(is_walk ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type , data=training_set,
               method = "C-classification",
               kernal = "radial",
               preProcess = c("center","scale"), #scaling values for svm
               trControl=trctrl,
               gamma = 0.5,
               cost = 5)

mod_car = svm(is_car ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type , data=training_set, cost = 5)

mod_bus = svm(is_bus ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type , data=training_set, cost = 5)

mod_airplane = svm(is_airplane ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type , data=training_set, cost = 5)

mod_bike = svm(is_bike ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type , data=training_set, cost = 5)

mod_subway = svm(is_subway ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type , data=training_set, cost = 5)

mod_train = svm(is_train ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type , data=training_set, cost = 5)


library(ROCR)
pred_walk <- prediction(predict(mod_walk, test_set), test_set$is_walk)
pred_car <- prediction(predict(mod_car, test_set), test_set$is_car)
pred_bus <- prediction(predict(mod_bus, test_set), test_set$is_bus)
pred_airplane <- prediction(predict(mod_airplane, test_set), test_set$is_airplane)
pred_bike <- prediction(predict(mod_bike, test_set), test_set$is_bike)
pred_subway <- prediction(predict(mod_subway, test_set), test_set$is_subway)
pred_train <- prediction(predict(mod_train, test_set), test_set$is_train)


perf_walk <- performance(pred_walk,"tpr","fpr")
perf_car <- performance(pred_car,"tpr","fpr")
perf_bus <- performance(pred_bus,"tpr","fpr")
perf_airplane <- performance(pred_airplane,"tpr","fpr")
perf_bike <- performance(pred_bike,"tpr","fpr")
perf_subway <- performance(pred_subway,"tpr","fpr")
perf_train <- performance(pred_train,"tpr","fpr")



plot(perf_walk)
abline(0, 1, col= "black")
par(new=TRUE)
plot(perf_car, col = "red")
par(new=TRUE)
plot(perf_bus, col = "yellow")
par(new=TRUE)
plot(perf_airplane, col = "green")
par(new=TRUE)
plot(perf_bike, col = "blue")
par(new=TRUE)
plot(perf_subway, col = "orange")
par(new=TRUE)
plot(perf_train, col = "violet")
par(new=TRUE)


legend(1,95,legend = c("walk", "car","bus","airplane","bike","subway","train"),
       col=c("black","red","yellow","green","blue","orange","violet"),
       lty = 1:8, cex = 0.8, box.lty = 0)


#calcolo di AUC 
# pred1 <- predict(mod1, testset)
# roc_obj <- roc(testset$isv1, pred1)
# au1 = auc(roc_obj)
# print(au1)