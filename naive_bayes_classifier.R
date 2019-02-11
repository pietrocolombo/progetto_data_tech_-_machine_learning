# Naive Bayes
if(!require(e1071)){
  install.packages("e1071")
  library("e1071")
}

if(!require(caret)){
  install.packages('caret', dependencies = TRUE)
  library("caret")
}

perc_csv <- "dataset_compresso_info_city_simple_tag.csv"
dati <- read.csv(perc_csv, header = TRUE, sep =",", quote = "\"", dec = ".")

# delete of all the journey with altitude equals to nan (percentage of value -777 within it > threshold)
dati = dati[!(is.na(dati$altitudeAvg)), ]

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
  tag = dati$tag,
  target = dati$label
)


## CORRELATION MATRIX EVALUATION
# data_classification <- as.integer(data_classification)
# head(data_classification)
# correlation_matrix <- data_classification[,2:length(data_classification)]
# c <- cor(correlation_matrix)
# corrplot(c, type = "upper", 
#          order = "hclust", tl.col = "black", tl.srt = 45)

# In this section we make sure that the sample of the training set give us all the all 11 labels
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
  sample = sample.int(n = nrow(data_classification), size = floor(p * nrow(data_classification)), replace = FALSE)
  training_set = data_classification[sample, ]
  label_training <- training_set$target
  test_set = data_classification[-sample, ]
  
  if(length(levels(label_training)) == 8)
    break
}

# view dimension of training and test set
dim(training_set)
dim(test_set)

# any null value in data_classification? if it's FALSE it's good ;)
anyNA(data_classification)

#training_set[["target"]] = factor(training_set[["target"]])

# Training phase

#trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 5)
trctrl <- trainControl(method = "cv", number = 10)

x = training_set[, 1:11]
y = training_set[, 12]

naive_bayes_model=train(x,
                        y,
                        #preProcess= c("center","scale"),
                        preProc = c("BoxCox", "center", "scale", "pca"),
                        method = 'nb',
                        trControl=trctrl)
                        #tuneLength = 10
                        #metric = "Kappa")
#cm_0 <- confusionMatrix(naive_bayes_model)
#naive_bayes_model = naiveBayes(training_set, training_set$target)
test_pred <- predict(naive_bayes_model, newdata = test_set)
cm <- confusionMatrix(test_pred, test_set$target)
conf <- table(test_pred, test_set$target)

n = sum(conf) # number of instances
nc = nrow(conf) # number of classes
diag = diag(conf) # number of correctly classified instances per class 
rowsums = apply(conf, 1, sum) # number of instances per class
colsums = apply(conf, 2, sum) # number of predictions per class
p = rowsums / n # distribution of instances over the actual classes
q = colsums / n # distribution of instances over the predicted classes

accuracy <- sum(diag(conf)) / sum(conf)
# precision is defined as the fraction of correct predictions for a certain class
precision <- diag(conf) / rowSums(conf)
# recall is the fraction of instances of a class that were correctly predicted
recall <- (diag(conf) / colSums(conf))
# F-1 score is defined as the harmonic mean (or a weighted average) of precision and recall
f1 = 2 * precision * recall / (precision + recall)





























# 
# spec_by_class = cm$byClass
# 
# aucs = c()
# plot(x=NA, y=NA, xlim=c(0,1), ylim=c(0,1),
#      ylab='True Positive Rate',
#      xlab='False Positive Rate',
#      bty='n')
# 
# 
# lines(roc.y ~ roc.x, col=type.id+1, lwd=2)
# library(ROCR)
# library(klaR)


# nbmodel = NaiveBayes(type ~ ., data=training_set[, 12])
# nbprediction=predict(nbmodel, newdata = test_set, type = "raw")
# score = nbprediction$posterior[, 'TRUE']
# lvls = levels(data_classification$target)
# for (target.id in 1:8) {
#   t =  as.factor(training_set$target == lvls[target.id])
#   actual.class = test_set$target == lvls[target.id]
#   pred = prediction(score, actual.class)
#   nbperf = performance(pred, "tpr", "fpr")
#   roc.x = unlist(nbperf@x.values)
#   roc.y = unlist(nbperf@y.values)
  
  
#   nbauc = performance(pred, "auc")
#   nbauc = unlist(slot(nbauc, "y.values"))
#   aucs[type.id] = nbauc
# }

lines(x=c(0,1), c(0,1))

mean(aucs)

## Da usare per ROC
# library("rpart")
# rp <- rpart(target ~ ., data = )
# library("ROCR")
# pred <- prediction(predict(rp, type = "prob")[, 2], data)
# 
# plot(performance(pred, "tpr", "fpr"))
# abline(0, 1, lty = 2)
















# library(pROC)
# rs <- roc.multi[['rocs']]
# plot.roc(rs[[1]])
# sapply(2:length(rs),function(i) lines.roc(rs[[i]],col=i))
# nbm = naiveBayes(target ~., training_set)
# tp <- predict(nbm, newdata = test_set)
# c <- confusionMatrix(tp, test_set$target)

#write.csv(data_classification, "data_classification.csv", row.names=FALSE)

# library(ggplot2)
# library(scales)
# 
# ggplotConfusionMatrix <- function(m){
#   mytitle <- paste("Accuracy", percent_format()(m$overall[1]),
#                    "Kappa", percent_format()(m$overall[2]))
#   p <-
#     ggplot(data = as.data.frame(m$table) ,
#            aes(x = Reference, y = Prediction)) +
#     geom_tile(aes(fill = log(Freq)), colour = "white") +
#     scale_fill_gradient(low = "white", high = "steelblue") +
#     geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
#     theme(legend.position = "none") +
#     ggtitle(mytitle)
#   return(p)
# }
# 
# ggplotConfusionMatrix(cm)
# 
# featurePlot(x, y, plot = "pairs")

