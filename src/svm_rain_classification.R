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
svm.file <- "result/svm_output_extreme_normal.txt"
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
  # begin training model with tuning parameters for SVM
  c_value <-c(1,10,100,1000)
  sig_value <- c(0.001,0.01,0.1)
  for(i_c in c_value){
    for(i_sig in sig_value){
      svp <- ksvm(data.xtrain, data.ytrain, type="C-svc", kernel='rbf', kpar=list(sigma=i_sig), C=i_c)
      # Predict labels on test
      data.ypred = predict(svp,data.xtest)
      table(data.ytest,data.ypred)
      # Compute accuracy
      normal.acc <- c(sum(data.ypred[which(data.ytest==1)] == 1)/(sum(data.ytest==1)))
      extreme.acc <- c(sum(data.ypred[which(data.ytest==2)]==2)/(sum(data.ytest==2)))
      acc <- sum(data.ypred==data.ytest)/length(data.ytest)
      c <- c(i_c)
      sigma <- c(i_sig)
      accuracy <- c(acc)
      df <- data.frame(feature, c, sigma, normal.acc, extreme.acc, accuracy)
      line <- toJSON(df)
      write(line,file=svm.file,append=TRUE)
    }
  }
}
