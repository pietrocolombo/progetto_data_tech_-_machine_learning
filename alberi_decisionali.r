install.packages("devtools")
library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)

library(tree)
library(UsingR)
library(rattle)
library(ggplot2)
perc_csv <- "data_classification"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

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
  sample = sample.int(n = nrow(dati), size = floor(p * nrow(dati)), replace = FALSE)
  training_set = dati[sample, ]
  label_training <- training_set$target
  test_set = dati[-sample, ]
  
  if(length(levels(label_training)) == 8)
    break
}
training_set[,target] = lapply(training_set[,target], as.factor)
h.tree<-tree(target ~ vcr+sr+hcr+vel_max+vel_avg+altitude_max+altitude_avg+tot_duration+tot_distance+state_changed+type,training_set)
summary(h.tree)

plot(h.tree,lwd=3)
text(h.tree,pretty=0,cex=1.2,col="blue")

variables = c("walk", "bus", "car", "subway", "airplane", "boat", "bike", "train")
formula = reformulate(variables, response = 'target')


model_tree = rpart::rpart(target~vcr+sr+hcr+vel_max+vel_avg+altitude_max+altitude_avg+tot_duration+tot_distance+state_changed, data=training_set, method="class")
rattle::fancyRpartPlot(model_tree)
plot(model_tree)
text(model_tree)
prediction = predict(model_tree, test_set, type="class")

final<-data.frame(prediction,test_set$target)
count<-0
for (i in 1:1101){
  if (final$prediction[i]!=final$test_set.target[i])
    count<-count+1
}

plot_data<-training_set[training_set$target=="bus" | training_set$target=="car",]
plot_data<-data.frame(plot_data$vel_avg,plot_data$vcr,plot_data$target)

ggplot(plot_data, aes(x=plot_data.vcr, y=plot_data.vel_avg, color=plot_data.target, shape=plot_data.target))+
  geom_point(size=6, alpha=0.6)

plot_data2<-training_set[training_set$target=="airplane" | training_set$target=="car",]
plot_data2<-data.frame(plot_data2$vel_avg,plot_data2$vcr,plot_data2$target)

ggplot(plot_data2, aes(x=plot_data2.vcr, y=plot_data2.vel_avg, color=plot_data2.target, shape=plot_data2.target))+
  geom_point(size=6, alpha=0.6)