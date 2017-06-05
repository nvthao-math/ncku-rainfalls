# load library
require(MASS)
library(jsonlite)
library(caret)
library(binaryLogic)
data.season.rain <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.rain.csv", header=TRUE)
data.season.dry <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.dry.csv", header=TRUE)
# extreme rain data 37.14
ind.extreme.rain <- which(data.season.rain$clazz == 2) 
ind.extreme.dry <- which(data.season.dry$clazz == 2)
rain.extreme <- rbind(data.season.rain[ind.extreme.rain, ], data.season.dry[ind.extreme.dry, ])
# normal rain data
ind.normal.rain <- which(data.season.rain$clazz == 1) 
ind.normal.dry <- which(data.season.dry$clazz == 1)
rain.normal <- rbind(data.season.rain[ind.normal.rain, ], data.season.dry[ind.normal.dry, ])
#
interval <- 130# 102
data <- rbind(rain.extreme, rain.normal)
data$clazz[which(data$rainfall > interval)] = 2
data$clazz[which(data$rainfall <= interval)] = 1
# data.tuning <- data.frame(data[, -c(1,2,3, (ncol(data)-1))])
# P5_z, P5th, P5zh, P8_f, P8_u, P8_v, P8_z, P8zh, P850, P_f, P_u, P_v, P_z, Rhum, Shum
data.tuning <- data.frame(data$P5_z, data$P5th, data$P5zh, data$P8_f, data$P8_u, data$P8_v, data$P8_z, data$P8zh, data$P850, data$P_f, data$P_u, data$P_v, data$P_z, data$Rhum, data$Shum, data$clazz)
colnames(data.tuning) <- c("P5_z", "P5th", "P5zh", "P8_f", "P8_u", "P8_v", "P8_z", "P8zh", "P850", "P_f", "P_u", "P_v", "P_z", "Rhum", "Shum", "clazz")
#
# feature selection
n.feature <- ncol(data.tuning) -1
x.bi <- as.binary((2^n.feature - 1), n=n.feature)
index.feature <- which(x.bi)
print(x.bi)
print(index.feature)
head(data.tuning[,index.feature])
#
# file to save
file <- "/home/bigdata/workspace/project/r/ncku-rainfalls/result/lda_interval_output_extreme_normal.txt"
#
threshold <- 0.6
for(i in c(1:1000)){
  feature_vec <- colnames(data.tuning)[index.feature]
  feature <- paste(feature_vec, collapse=", ")
  # raining data
  data.extreme <- data.tuning[which(data.tuning$clazz == 2, arr.ind = FALSE),]
  data.extreme.row <- nrow(data.extreme)
  data.extreme.ntrain <- round(data.extreme.row * 0.7)
  data.extreme.tindex <- sample(data.extreme.row, data.extreme.ntrain)
  data.extreme.xtrain <- as.matrix(data.extreme[data.extreme.tindex, index.feature])
  data.extreme.xtest <- as.matrix(data.extreme[-data.extreme.tindex, index.feature])
  data.extreme.ytrain <- matrix(data.extreme$clazz[data.extreme.tindex])
  data.extreme.ytest <- matrix(data.extreme$clazz[-data.extreme.tindex])
  # dry data
  data.normal <- data.tuning[which(data.tuning$clazz == 1, arr.ind = FALSE),]
  data.normal.row <- nrow(data.normal)
  data.normal.ntrain <- round(data.normal.row * 0.7)
  data.normal.tindex <- sample(data.normal.row, data.normal.ntrain)
  data.normal.xtrain <- as.matrix(data.normal[data.normal.tindex, index.feature])
  data.normal.xtest <- as.matrix(data.normal[-data.normal.tindex, index.feature])
  data.normal.ytrain <- matrix(data.normal$clazz[data.normal.tindex])
  data.normal.ytest <- matrix(data.normal$clazz[-data.normal.tindex])
  # merge data for train
  data.xtrain <- rbind(data.extreme.xtrain, data.normal.xtrain)
  data.ytrain <- rbind(data.extreme.ytrain, data.normal.ytrain)
  # merge data for test
  data.xtest <- rbind(data.extreme.xtest, data.normal.xtest)
  data.ytest <- rbind(data.extreme.ytest, data.normal.ytest)
  # # naive bayes model
  ytrain <- factor(data.ytrain, labels = c("yes", "no"))
  lda.model <- lda(data.xtrain, ytrain)
  ytest <- factor(data.ytest, labels = c("yes", "no"))
  # pred.result <- predict(naive.bayes.model$finalModel,data.xtest)
  pred.result.test <- predict(lda.model, data.xtest)
  data.ypred.test <- pred.result.test$class
  table(data.ypred.test, ytest)
  # accuracy on testing set
  extreme.acc.test <- sum(data.ypred.test[which(ytest=="no")] == "no")/(sum(ytest=="no"))
  normal.acc.test <- sum(data.ypred.test[which(ytest=="yes")]=="yes")/(sum(ytest=="yes"))
  acc.test <- sum(data.ypred.test==ytest)/length(ytest)
  # accuracy on training set
  pred.result.train <- predict(lda.model,data.xtrain)
  data.ypred.train <- pred.result.train$class
  extreme.acc.train <- sum(data.ypred.train[which(ytrain=="no")] == "no")/(sum(ytrain=="no"))
  normal.acc.train <- sum(data.ypred.train[which(ytrain=="yes")]=="yes")/(sum(ytrain=="yes"))
  acc.train <- sum(data.ypred.train==ytrain)/length(ytrain)
  #
  result.df <- data.frame(feature, extreme.acc.test, normal.acc.test, acc.test, extreme.acc.train, normal.acc.train, acc.train)
  line <- toJSON(result.df)
  print(line)
  write(line,file=file,append=TRUE)
  # save data train to build model
  if(acc.test > threshold){
    threshold <- acc.test
    train2BuildModel <- cbind(data.xtrain, data.ytrain)
    colnames(train2BuildModel) <- c(colnames(data.xtrain), "clazz")
    write.csv(train2BuildModel, file = "/home/bigdata/workspace/project/r/ncku-rainfalls/data/buildModel/extrem_vs_normal/lda_trainData.csv")
  }
}


