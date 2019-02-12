# ROC EVALUATION
if(!require(pROC)){
  install.packages("pROC")
  library("pROC")
}
if(!require(ROCR)){
  install.packages("ROCR")
  library("ROCR")
}
if(!require(caret)){
  install.packages("caret")
  library("caret")
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

training_set$is_walk <- as.factor(training_set$is_walk)
training_set$is_car <- as.factor(training_set$is_car)
training_set$is_bus <- as.factor(training_set$is_bus)
training_set$is_train <- as.factor(training_set$is_train)
training_set$is_bike <- as.factor(training_set$is_bike)

test_set$is_walk <- as.factor(test_set$is_walk)
test_set$is_car <- as.factor(test_set$is_car)
test_set$is_bus <- as.factor(test_set$is_bus)
test_set$is_train <- as.factor(test_set$is_train)
test_set$is_bike <- as.factor(test_set$is_bike)
################### NAIVE BAYES ROC EVALUATION FOR EACH CLASS #############################

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
# Evaluating ROC for each class
mod_walk = train(is_walk ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + city_changed + type,
                 data = training_set,
                 preProcess= c("center","scale"),
                 method = 'nb',
                 trControl=trctrl)

mod_car = train(is_car ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + city_changed + type,
                data = training_set,
                preProcess= c("center","scale"),
                method = 'nb',
                trControl=trctrl)

mod_bus = train(is_bus ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + city_changed + type,
                data = training_set,
                preProcess= c("center","scale"),
                method = 'nb',
                trControl=trctrl)

mod_bike = train(is_bike ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + city_changed + type,
                 data = training_set,
                 preProcess= c("center","scale"),
                 method = 'nb',
                 trControl=trctrl)

mod_train = train(is_train ~ vcr + sr + hcr + vel_max + vel_avg + altitude_max + altitude_avg + tot_duration + tot_distance + state_changed + city_changed + type,
                  data = training_set,
                  preProcess= c("center","scale"),
                  method = 'nb',
                  trControl=trctrl)



training_set$is_walk <- as.numeric(training_set$is_walk)
training_set$is_car <- as.numeric(training_set$is_car)
training_set$is_bus <- as.numeric(training_set$is_bus)
training_set$is_train <- as.numeric(training_set$is_train)
training_set$is_bike <- as.numeric(training_set$is_bike)

# ROC CURVEs

predict_walk <- predict(mod_walk, test_set)
roc_obj_walk <- roc(test_set$is_walk, as.numeric(predict_walk))
print(auc(roc_obj_walk))
rs <- roc_obj_walk[['rocs']]
plot(ggroc(roc_obj_walk, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw())

par(new=TRUE)
predict_car <- predict(mod_car, test_set)
roc_obj_car <- roc(test_set$is_car, as.numeric(predict_car))
print(auc(roc_obj_car))
rs <- roc_obj_car[['rocs']]
plot(ggroc(roc_obj_car, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw())
x_mean <- c(auc(roc_obj_car), x_mean)

par(new=TRUE)
predict_bus <- predict(mod_bus, test_set)
roc_obj_bus <- roc(test_set$is_bus, as.numeric(predict_bus))
print(auc(roc_obj_bus))
rs <- roc_obj_bus[['rocs']]
plot(ggroc(roc_obj_bus, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw())
x_mean <- c(auc(roc_obj_bus), x_mean)

par(new=TRUE)
predict_train <- predict(mod_train, test_set)
roc_obj_train <- roc(test_set$is_train, as.numeric(predict_train))
print(auc(roc_obj_train))
rs <- roc_obj_train[['rocs']]
plot(ggroc(roc_obj_train, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw())
x_mean <- c(auc(roc_obj_train), x_mean)


par(new=TRUE)
predict_bike <- predict(mod_bike, test_set)
roc_obj_bike <- roc(test_set$is_bike, as.numeric(predict_bike))
print(auc(roc_obj_bike))
rs <- roc_obj_bike[['rocs']]
plot(ggroc(roc_obj_bike, legacy.axes=TRUE) + geom_abline(slope=1, intercept=0, color="red", linetype=3) + theme_bw())
x_mean <- c(auc(roc_obj_bike), x_mean)


media <- mean(x_mean)
