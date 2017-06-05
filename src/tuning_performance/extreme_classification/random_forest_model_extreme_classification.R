library(randomForest)
##
##
# load library
library(kernlab)
library(jsonlite)
library(caret)
require(gdata)
library(binaryLogic)
data.season.rain <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.rain.csv", header=TRUE)
data.season.dry <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.dry.csv", header=TRUE)
# extreme rain data
ind.extreme.rain <- which(data.season.rain$clazz == 2) 
ind.extreme.dry <- which(data.season.dry$clazz == 2)
rain.extreme <- rbind(data.season.rain[ind.extreme.rain, ], data.season.dry[ind.extreme.dry, ])
# normal rain data
ind.normal.rain <- which(data.season.rain$clazz == 1) 
ind.normal.dry <- which(data.season.dry$clazz == 1)
rain.normal <- rbind(data.season.rain[ind.normal.rain, ], data.season.dry[ind.normal.dry, ])
#
data <- rbind(rain.extreme, rain.normal)
# data.tuning <- data.frame(data[, -c(1,2,3, (ncol(data)-1))])
data.tuning <- data.frame(data$P5_z, data$P5th, data$P5zh, data$P8_f, data$P8_u, data$P8_v, data$P8_z, data$P8zh, data$P850, data$P_f, data$P_u, data$P_v, data$P_z, data$Rhum, data$Shum, data$P5_u, data$R850, data$R850, data$R850, data$P8th, data$Mslp, data$P500, data$P_zh, data$P_th, data$Temp, data$clazz)
colnames(data.tuning) <- c("P5_z", "P5th", "P5zh", "P8_f", "P8_u", "P8_v", "P8_z", "P8zh", "P850", "P_f", "P_u", "P_v", "P_z", "Rhum", "Shum", "P5_u", "R850", "R850", "R850", "P8th", "Mslp", "P500", "P_zh", "P_th", "Temp", "clazz")
# feature selection
n.feature <- ncol(data.tuning) -1
# # file to save
bayes.file <- "/home/bigdata/workspace/project/r/ncku-rainfalls/result/radom_forest_output_extreme_normal.txt"
#
for(i in c(33553408:(2^n.feature-1))){
  x.bi <- as.binary(i, n=n.feature)
  index.feature <- which(x.bi)
  print(x.bi)
  print(index.feature)
  head(data.tuning[,index.feature])
  #
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
  bayse.ytrain <- factor(data.ytrain, labels = c("yes", "no"))
  bayse.ytest <- factor(data.ytest, labels = c("yes", "no"))
  iris.rf <- randomForest(data.xtrain, bayse.ytrain)
  bayes.data.ypred <- predict(iris.rf, data.xtest)
  # accuracy
  extreme.acc <- c(sum(bayes.data.ypred[which(bayse.ytest=="no")] == "no")/(sum(bayse.ytest=="no")))
  normal.acc <- c(sum(bayes.data.ypred[which(bayse.ytest=="yes")]=="yes")/(sum(bayse.ytest=="yes")))
  bayes.acc <- c(sum(bayes.data.ypred==bayse.ytest)/length(bayse.ytest))
  bayes.df <- data.frame(feature, extreme.acc, normal.acc, bayes.acc)
  bayes.line <- toJSON(bayes.df)
  write(bayes.line,file=bayes.file,append=TRUE)
  print(bayes.line)
}


