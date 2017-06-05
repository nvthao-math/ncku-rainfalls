# Dry season (11-04)
# svm: "feature":"P8_z, P500, P_z, P850, R850, Rhum, P5zh, P5_v"
# parameter: "c":1,"sigma":0.1
# accuracy: "rain.acc":0.7945,"dry.acc":0.6743,"accuracy":0.7417
# load library
library(kernlab)
library(jsonlite)
library(caret)
require(gdata)
library(binaryLogic)
# data.season.dry <- read.xls(("data/input/tunning_mean_rainfall_two_season.xlsx"), sheet = 2, header = TRUE)
data.season.dry <- read.csv("data/input/twoseason.dry.csv", header=TRUE)
# data.season.rain <- read.xls(("data/input/tunning_mean_rainfall_two_season.xlsx"), sheet = 1, header = TRUE)
# data.season.rain <- read.csv("data/input/twoseason.rain.csv", header=TRUE)
data <- rbind(data.season.dry)
data.tuning <- data.frame(data$R500, data$P500, data$P850, data$R850, data$P5_v, data$clazz)
colnames(data.tuning) <- c("R500", "P500", "P850", "R850", "P5_v","clazz")
#
data.xtrain <- data.tuning[, -ncol(data.tuning)]
data.ytrain <- data.tuning$clazz
data.ytrain[which(data.ytrain == 2)] = 1
# # naive bayes model
bayse.ytrain <- factor(data.ytrain, labels = c("yes", "no"))
naive.bayes.model = train(data.xtrain,bayse.ytrain,'nb',trControl=trainControl(method='cv',number=10))
pred.result <- predict(naive.bayes.model$finalModel,data.xtrain)
posterior <- as.vector(pred.result$posterior[,1])
#
data <- data.frame(data, posterior)
##
data.tuning <- data.frame(data$P8_z, data$P500, data$P_z, data$P850, data$R850, data$Rhum, data$P5zh, data$P5_v, data$posterior, data$clazz)
colnames(data.tuning) <- c("P8_z", "P500", "P_z", "P850", "R850", "Rhum", "P5zh", "P5_v", "posterior", "clazz")
for(i in c(1:1000)) {
  # raining data
  data.rain <- data.tuning[which(data.tuning$clazz == 1 | data.tuning$clazz == 2, arr.ind = FALSE),]
  data.rain.row <- nrow(data.rain)
  data.rain.ntrain <- round(data.rain.row * 0.7)
  data.rain.tindex <- sample(data.rain.row, data.rain.ntrain)
  data.rain.xtrain <- as.matrix(data.rain[data.rain.tindex, ])
  data.rain.xtest <- as.matrix(data.rain[-data.rain.tindex, ])
  data.rain.ytrain <- matrix(data.rain$clazz[data.rain.tindex])
  data.rain.ytest <- matrix(data.rain$clazz[-data.rain.tindex])
  # dry data
  data.dry <- data.tuning[which(data.tuning$clazz == -1, arr.ind = FALSE),]
  data.dry.row <- nrow(data.dry)
  data.dry.ntrain <- round(data.dry.row * 0.7)
  data.dry.tindex <- sample(data.dry.row, data.dry.ntrain)
  data.dry.xtrain <- as.matrix(data.dry[data.dry.tindex, ])
  data.dry.xtest <- as.matrix(data.dry[-data.dry.tindex, ])
  data.dry.ytrain <- matrix(data.dry$clazz[data.dry.tindex])
  data.dry.ytest <- matrix(data.dry$clazz[-data.dry.tindex])
  # merge data for train
  data.xtrain <- rbind(data.rain.xtrain[, -(ncol(data.rain.xtrain))], data.dry.xtrain[, -ncol(data.dry.xtrain)])
  data.ytrain <- rbind(data.rain.ytrain, data.dry.ytrain)
  data.ytrain[which(data.ytrain == 2)] = 1
  # merge data for test
  data.xtest <- rbind(data.rain.xtest[, -ncol(data.rain.xtest)], data.dry.xtest[, -ncol(data.dry.xtest)])
  data.ytest <- rbind(data.rain.ytest, data.dry.ytest)
  data.ytest[which(data.ytest == 2), ] = 1
  # begin training model with tuning parameters for SVM
  c_value <-c(1)
  sig_value <- c(0.1)
  svp <- ksvm(data.xtrain, data.ytrain, type="C-svc", kernel='rbf', kpar=list(sigma=0.1), C=1)
  # Predict labels on test
  data.ypred.test = predict(svp,data.xtest)
  table(data.ytest,data.ypred.test)
  # Compute accuracy
  rain.acc.test <- c(sum(data.ypred.test[which(data.ytest==1)] == 1)/(sum(data.ytest==1)))
  dry.acc.test <- c(sum(data.ypred.test[which(data.ytest==-1)]==-1)/(sum(data.ytest==-1)))
  acc.test <- sum(data.ypred.test==data.ytest)/length(data.ytest)
  accuracy.test <- c(acc.test)
  df.test <- data.frame(c_value, sig_value, rain.acc.test, dry.acc.test, accuracy.test)
  line.test <- toJSON(df.test)
  print(line.test)
  # predict label on train
  data.train.ypred = predict(svp,data.xtrain)
  table(data.ytrain,data.train.ypred)
  # compute accuracy
  rain.acc.train <- c(sum(data.train.ypred[which(data.ytrain==1)] == 1)/(sum(data.ytrain==1)))
  dry.acc.train <- c(sum(data.train.ypred[which(data.ytrain==-1)]==-1)/(sum(data.ytrain==-1)))
  acc.train <- sum(data.train.ypred==data.ytrain)/length(data.ytrain)
  accuracy.train <- c(acc.train)
  df.train <- data.frame(c_value, sig_value, rain.acc.train, dry.acc.train, accuracy.train)
  line.train <- toJSON(df.train)
  print(line.train)
}






