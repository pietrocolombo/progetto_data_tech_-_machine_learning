# ROC EVALUATION
if(!require(pROC)){
  install.packages("pROC")
  library("pROC")
}
if(!require(ROCR)){
  install.packages("ROCR")
  library("ROCR")
}
if(!require(e1071)){
  install.packages("e1071")
  library("e1071")
}


#Binarization of the classes

training_set$is_walk <- as.numeric(training_set$target == 'walk')
test_set$is_walk <- as.numeric(test_set$target == 'walk')

training_set$is_car <- as.numeric(training_set$target == 'car')
test_set$is_car <- as.numeric(test_set$target == 'car')

training_set$is_bus <- as.numeric(training_set$target == 'bus')
test_set$is_bus <- as.numeric(test_set$target == 'bus')

training_set$is_bike <- as.numeric(training_set$target == 'bike')
test_set$is_bike <- as.numeric(test_set$target == 'bike')

training_set$is_train <- as.numeric(training_set$target == 'train')
test_set$is_train <- as.numeric(test_set$target == 'train')

# training_set$is_boat<- as.numeric(training_set$target == 'boat')
# test_set$is_boat <- as.numeric(test_set$target == 'boat')

# training_set$is_subway <- as.numeric(training_set$target == 'subway')
# test_set$is_subway <- as.numeric(test_set$target == 'subway')

# training_set$is_airplane <- as.numeric(training_set$target == 'airplane')
# test_set$is_airplane <- as.numeric(test_set$target == 'airplane')




################### SVM RADIAL KERNEL ROC EVALUATION FOR EACH CLASS#############################
trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

# Evaluating models for each class
mod_walk = svm(is_walk ~  vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + city_changed + type , data=training_set,
               method = "C-classification",
               kernal = "radial",
               preProcess = c("center","scale"), #scaling values for svm
               trControl=trctrl,
               gamma = 0.5,
               cost = 5)

mod_car = svm(is_car ~  vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + city_changed + type, data=training_set,
              method = "C-classification",
              kernal = "radial",
              preProcess = c("center","scale"), #scaling values for svm
              trControl=trctrl,
              gamma = 0.5,
              cost = 5)

mod_bus = svm(is_bus ~  vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type, data=training_set,
              method = "C-classification",
              kernal = "radial",
              preProcess = c("center","scale"), #scaling values for svm
              trControl=trctrl,
              gamma = 0.5,
              cost = 5)
# 
# mod_airplane = svm(is_airplane ~  vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type,data=training_set, 
#                    method = "C-classification",
#                    kernal = "radial",
#                    preProcess = c("center","scale"), #scaling values for svm
#                    trControl=trctrl,
#                    gamma = 0.5,
#                    cost = 5)

mod_bike = svm(is_bike ~  vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + city_changed + type, data=training_set,
               method = "C-classification",
               kernal = "radial",
               preProcess = c("center","scale"), #scaling values for svm
               trControl=trctrl,
               gamma = 0.5,
               cost = 5)

# mod_boat = svm(is_boat ~  vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + type, data=training_set, 
#                  method = "C-classification",
#                  kernal = "radial",
#                  preProcess = c("center","scale"), #scaling values for svm
#                  trControl=trctrl,
#                  gamma = 0.5,
#                  cost = 5)



mod_train = svm(is_train ~  vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + city_changed + type, data=training_set, 
                method = "C-classification",
                kernal = "radial",
                preProcess = c("center","scale"), #scaling values for svm
                trControl=trctrl,
                gamma = 0.5,
                cost = 5)


pred_walk <- prediction(predict(mod_walk, test_set), test_set$is_walk)
pred_car <- prediction(predict(mod_car, test_set), test_set$is_car)
pred_bus <- prediction(predict(mod_bus, test_set), test_set$is_bus)
# pred_airplane <- prediction(predict(mod_airplane, test_set), test_set$is_airplane)
pred_bike <- prediction(predict(mod_bike, test_set), test_set$is_bike)
# pred_boat <- prediction(predict(mod_boat, test_set), test_set$is_boat)
# pred_subway <- prediction(predict(mod_subway, test_set), test_set$is_subway)
pred_train <- prediction(predict(mod_train, test_set), test_set$is_train)


perf_walk <- performance(pred_walk,"tpr","fpr")
perf_car <- performance(pred_car,"tpr","fpr")
perf_bus <- performance(pred_bus,"tpr","fpr")
# perf_airplane <- performance(pred_airplane,"tpr","fpr")
perf_bike <- performance(pred_bike,"tpr","fpr")
# perf_boat <- performance(pred_boat,"tpr","fpr")
# perf_subway <- performance(pred_subway,"tpr","fpr")
perf_train <- performance(pred_train,"tpr","fpr")



plot(perf_walk, col= "black")
par(new=TRUE)
plot(perf_car, col = "red")
par(new=TRUE)
plot(perf_bus, col = "yellow")
par(new=TRUE)
# plot(perf_airplane, col = "green")
# par(new=TRUE)
plot(perf_bike, col = "blue")
par(new=TRUE)
# plot(perf_boat, col = "light blue")
# par(new=TRUE)
# plot(perf_subway, col = "orange")
# par(new=TRUE)
plot(perf_train, col = "violet")
par(new=TRUE)
abline(0,1, col ="black")


legend(0.55, 0.5, c("walk", "car","bus","bike","train"), 
      lty=1, col=c("black","red","yellow","blue","violet"), bty='n', cex=1.0)
title(main = "ROC Curve SVM")

#AUC evaluation for each class 
pred_walk <- predict(mod_walk, test_set)
roc_obj <- roc(test_set$is_walk, pred_walk)
au_walk = auc(roc_obj)
print(au_walk)
x_mean <- c(au_walk)

plot(perf_walk, col= "black")
par(new=TRUE)
abline(0,1, col ="black")
legend(0.55, 0.5, c("walk"), 
       lty=1, col=c("black"), bty='n', cex=1.0)
title(main = "ROC Curve SVM", sub = paste0("Area under the curve: ", au_walk ))

pred_car <- predict(mod_car, test_set)
roc_obj <- roc(test_set$is_car, pred_car)
au_car = auc(roc_obj)
print(au_car)
x_mean <- c(au_car, x_mean)

plot(perf_car, col = "red")
par(new=TRUE)
abline(0,1, col ="black")
legend(0.55, 0.5, c("car"), 
       lty=1, col=c("red"), bty='n', cex=1.0)
title(main = "ROC Curve SVM", sub = paste0("Area under the curve: ", au_car ))

pred_bus <- predict(mod_bus, test_set)
roc_obj <- roc(test_set$is_bus, pred_bus)
au_bus = auc(roc_obj)
print(au_bus)
x_mean <- c(au_bus, x_mean)

plot(perf_bus, col = "yellow")
par(new=TRUE)
abline(0,1, col ="black")
legend(0.55, 0.5, c("bus"), 
       lty=1, col=c("yellow"), bty='n', cex=1.0)
title(main = "ROC Curve SVM", sub = paste0("Area under the curve: ", au_bus ))

# pred_airplane <- predict(mod_airplane, test_set)
# roc_obj <- roc(test_set$is_airplane, pred_airplane)
# au_airplane = auc(roc_obj)
# print(au_airplane)
# 
# plot(perf_airplane, col = "green")
# par(new=TRUE)
# abline(0,1, col ="black")
# legend(0.55, 0.5, c("airplane"), 
#        lty=1, col=c("green"), bty='n', cex=1.0)
# title(main = "ROC Curve SVM", sub = paste0("Area under the curve: ", au_airplane ))

pred_bike <- predict(mod_bike, test_set)
roc_obj <- roc(test_set$is_bike, pred_bike)
au_bike = auc(roc_obj)
print(au_bike)
x_mean <- c(au_bike, x_mean)

plot(perf_bike, col = "blue")
par(new=TRUE)
abline(0,1, col ="black")
legend(0.55, 0.5, c("bike"), 
       lty=1, col=c("blue"), bty='n', cex=1.0)
title(main = "ROC Curve SVM", sub = paste0("Area under the curve: ", au_bike ))

# pred_subway <- predict(mod_subway, test_set)
# roc_obj <- roc(test_set$is_subway, pred_subway)
# au_subway = auc(roc_obj)
# print(au_subway)
# 
# plot(perf_subway, col = "orange")
# par(new=TRUE)
# abline(0,1, col ="black")
# legend(0.55, 0.5, c("subway"), 
#        lty=1, col=c("orange"), bty='n', cex=1.0)
# title(main = "ROC Curve SVM", sub = paste0("Area under the curve: ", au_subway ))

pred_train <- predict(mod_train, test_set)
roc_obj <- roc(test_set$is_train, pred_train)
au_train = auc(roc_obj)
print(au_train)
x_mean <- c(au_train, x_mean)

plot(perf_train, col = "violet")
par(new=TRUE)
abline(0,1, col ="black")
legend(0.55, 0.5, c("train"), 
       lty=1, col=c("violet"), bty='n', cex=1.0)
title(main = "ROC Curve SVM", sub = paste0("Area under the curve: ", au_train ))

plot(perf_walk, col= "black")
par(new=TRUE)
plot(perf_car, col = "red")
par(new=TRUE)
plot(perf_bus, col = "yellow")
par(new=TRUE)
# plot(perf_airplane, col = "green")
# par(new=TRUE)
plot(perf_bike, col = "blue")
par(new=TRUE)
# plot(perf_boat, col = "light blue")
# par(new=TRUE)
# plot(perf_subway, col = "orange")
# par(new=TRUE)
plot(perf_train, col = "violet")
par(new=TRUE)
abline(0,1, col ="black")


legend(0.55, 0.5, c("walk", "car","bus","bike","train"), 
       lty=1, col=c("black","red","yellow","blue","violet"), bty='n', cex=1.0)
title(main = "ROC Curve SVM", sub = paste0("mean of the area under the curve: ", mean(x_mean) ))

mean(x_mean)

