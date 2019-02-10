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
if(!require()){
  install.packages("e1071")
  library("e1071")
}





perc_csv <- "dataset_compresso_info_city_simple_tag.csv"
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

#normalize(dati, method = "standardize", range = c(0, 1), margin = 1L, on.constant = "quiet")
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
  type = dati$tag,
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
  type = dati$tag,
  state_changed = dati$state_changed,
  target = dati$label
)


#c <- cor(data_correlation)
# corrplot(c, type = "upper",order = "hclust", tl.col = "black", tl.srt = 45)
corr <- hetcor(as.data.frame(data_correlation))
ggcorrplot(corr$correlations, outline.col = "white", insig = "pch")
theme(axis.text.x = element_text(size=10, angle=90, vjust=0.5), axis.text.y = element_text(size=10, vjust=0.5))
labs(title = paste("Correlation Matrix"))

#corr <- correlate(data_correlation, test=TRUE, corr.method="pearson", p.adjust.method="holm")
# corrplot(corr,type = "upper",order = "hclust", tl.col = "black", tl.srt = 45)
# cor2(data_correlation)


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

training_set[["target"]] = factor(training_set[["target"]])
# view dimension of training and test set
dim(training_set)
dim(test_set)

# any null value in data_classification? if it's FALSE it's good ;)
anyNA(data_classification)

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)

svm1 <- svm(target~., data = training_set,
            method = "C-classification",
            kernal = "radial",
            preProcess = c("center","scale"),
            trControl=trctrl,
            gamma = 0.5,
            cost = 0.8)

predict <- predict(svm1, test_set)
cm <- confusionMatrix(predict,test_set$target)
cm






cor2 = function(df){
  
  stopifnot(inherits(df, "data.frame"))
  stopifnot(sapply(df, class) %in% c("integer"
                                     , "numeric"
                                     , "factor"
                                     , "character"))
  
  cor_fun <- function(pos_1, pos_2){
    
    # both are numeric
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("integer", "numeric")){
      r <- stats::cor(df[[pos_1]]
                      , df[[pos_2]]
                      , use = "pairwise.complete.obs"
      )
    }
    
    # one is numeric and other is a factor/character
    if(class(df[[pos_1]]) %in% c("integer", "numeric") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_1]] ~ as.factor(df[[pos_2]])))[["r.squared"]])
    }
    
    if(class(df[[pos_2]]) %in% c("integer", "numeric") &&
       class(df[[pos_1]]) %in% c("factor", "character")){
      r <- sqrt(
        summary(
          stats::lm(df[[pos_2]] ~ as.factor(df[[pos_1]])))[["r.squared"]])
    }
    
    # both are factor/character
    if(class(df[[pos_1]]) %in% c("factor", "character") &&
       class(df[[pos_2]]) %in% c("factor", "character")){
      r <- lsr::cramersV(df[[pos_1]], df[[pos_2]], simulate.p.value = TRUE)
    }
    
    return(r)
  } 
  
  cor_fun <- Vectorize(cor_fun)
  
  # now compute corr matrix
  corrmat <- outer(1:ncol(df)
                   , 1:ncol(df)
                   , function(x, y) cor_fun(x, y)
  )
  
  rownames(corrmat) <- colnames(df)
  colnames(corrmat) <- colnames(df)
  
  return(corrmat)
}
