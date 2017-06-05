# load library
library(jsonlite)
library(caret)
library(binaryLogic)
#
data.train.raw <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/extreme/extreme.data.train.csv", header=TRUE)
data.test.raw <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/extreme/extreme.data.test.csv", header=TRUE)
#
x.name <- c("P5_z", "P5th", "P5zh", "P8_f", "P8_u", "P8_v", "P8_z", "P8zh", "P850", "P_f", "P_u", "P_v", "P_z", "Rhum", "Shum", "P5_u", "R850", "R850", "R850", "P8th", "Mslp", "P500", "P_zh", "P_th", "Temp", "clazz")
data.train <- data.frame(data.train.raw$P5_z, data.train.raw$P5th, data.train.raw$P5zh, data.train.raw$P8_f, data.train.raw$P8_u, data.train.raw$P8_v, data.train.raw$P8_z, data.train.raw$P8zh, data.train.raw$P850, data.train.raw$P_f, data.train.raw$P_u, data.train.raw$P_v, data.train.raw$P_z, data.train.raw$Rhum, data.train.raw$Shum, data.train.raw$P5_u, data.train.raw$R850, data.train.raw$R850, data.train.raw$R850, data.train.raw$P8th, data.train.raw$Mslp, data.train.raw$P500, data.train.raw$P_zh, data.train.raw$P_th, data.train.raw$Temp, data.train.raw$clazz)
colnames(data.train) <- x.name
data.test <- data.frame(data.test.raw$P5_z, data.test.raw$P5th, data.test.raw$P5zh, data.test.raw$P8_f, data.test.raw$P8_u, data.test.raw$P8_v, data.test.raw$P8_z, data.test.raw$P8zh, data.test.raw$P850, data.test.raw$P_f, data.test.raw$P_u, data.test.raw$P_v, data.test.raw$P_z, data.test.raw$Rhum, data.test.raw$Shum, data.test.raw$P5_u, data.test.raw$R850, data.test.raw$R850, data.test.raw$R850, data.test.raw$P8th, data.test.raw$Mslp, data.test.raw$P500, data.test.raw$P_zh, data.test.raw$P_th, data.test.raw$Temp, data.test.raw$clazz)
colnames(data.test) <- x.name
# file to save
file_save <- "/home/bigdata/workspace/project/r/ncku-rainfalls/result/bayes_output_extreme_classifier.txt"
# feature selection
n.feature <- ncol(data.train) -1
for(i in c(5000000:10000000)){ # 33554431
  x.bi <- as.binary(i, n=n.feature)
  index.feature <- which(x.bi)
  print(index.feature)
  head(data.train[,index.feature])
  #
  feature_vec <- colnames(data.train)[index.feature]
  feature <- paste(feature_vec, collapse=", ")
  # merge data for train
  data.xtrain <- data.frame(data.train[,index.feature])
  data.ytrain <- data.train$clazz
  # merge data for test
  data.xtest <- data.frame(data.test[, index.feature])
  data.ytest <- data.test$clazz
  # naive bayes model
  ytrain <- factor(data.ytrain, labels = c("yes", "no"))
  naive.bayes.model = train(data.xtrain, ytrain,'nb',trControl=trainControl(method='cv',number=10))
  pred.result.test <- predict(naive.bayes.model$finalModel,data.xtest)
  data.ypred.test <- pred.result.test$class
  # lda.model <- lda(data.xtrain, ytrain)
  ytest <- factor(data.ytest, labels = c("yes", "no"))
  # pred.result.test <- predict(lda.model, data.xtest)
  data.ypred.test <- pred.result.test$class
  table(data.ypred.test, ytest)
  # accuracy on testing set
  extreme.acc.test <- sum(data.ypred.test[which(ytest=="no")] == "no")/(sum(ytest=="no"))
  normal.acc.test <- sum(data.ypred.test[which(ytest=="yes")]=="yes")/(sum(ytest=="yes"))
  acc.test <- sum(data.ypred.test==ytest)/length(ytest)
  # accuracy on training set
  pred.result.train <- predict(naive.bayes.model$finalModel,data.xtrain)
  data.ypred.train <- pred.result.train$class
  extreme.acc.train <- sum(data.ypred.train[which(ytrain=="no")] == "no")/(sum(ytrain=="no"))
  normal.acc.train <- sum(data.ypred.train[which(ytrain=="yes")]=="yes")/(sum(ytrain=="yes"))
  acc.train <- sum(data.ypred.train==ytrain)/length(ytrain)
  #
  result.df <- data.frame(i, feature, extreme.acc.test, normal.acc.test, acc.test, extreme.acc.train, normal.acc.train, acc.train)
  line <- toJSON(result.df)
  print(line)
  write(line,file=file_save,append=TRUE)
}

