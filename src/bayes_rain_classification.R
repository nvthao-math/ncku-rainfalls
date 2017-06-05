# load library
library(kernlab)
library(jsonlite)
library(caret)
require(gdata)
library(binaryLogic)
data.season.rain <- read.xls(("data/input/tunning_mean_rainfall_two_season.xlsx"), sheet = 1, header = TRUE)
data.season.dry <- read.xls(("data/input/tunning_mean_rainfall_two_season.xlsx"), sheet = 2, header = TRUE)
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
data.tuning <- data.frame(data$P8_z, data$R500, data$P500, data$P_z, data$P850, data$R850, data$Rhum, data$P5zh, data$P5_v, data$clazz)
colnames(data.tuning) <- c("P8_z", "R500", "P500", "P_z", "P850", "R850", "Rhum", "P5zh", "P5_v", "clazz")
# feature selection
n.feature <- ncol(data.tuning) -1
# # file to save
bayes.file <- "result/bayes_output_extreme_normal.txt"
#
for(i in c(1:(2^n.feature-1))){
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
  naive.bayes.model = train(data.xtrain,bayse.ytrain,'nb',trControl=trainControl(method='cv',number=10))
  bayse.ytest <- factor(data.ytest, labels = c("yes", "no"))
  pred.result <- predict(naive.bayes.model$finalModel,data.xtest)
  bayes.data.ypred <- pred.result$class
  table(pred.result$class,bayse.ytest)
  # accuracy
  extreme.acc <- c(sum(bayes.data.ypred[which(bayse.ytest=="no")] == "no")/(sum(bayse.ytest=="no")))
  normal.acc <- c(sum(bayes.data.ypred[which(bayse.ytest=="yes")]=="yes")/(sum(bayse.ytest=="yes")))
  bayes.acc <- c(sum(bayes.data.ypred==bayse.ytest)/length(bayse.ytest))
  bayes.df <- data.frame(feature, extreme.acc, normal.acc, bayes.acc)
  bayes.line <- toJSON(bayes.df)
  write(bayes.line,file=bayes.file,append=TRUE)
}
