# load library
library(kernlab)
library(jsonlite)
library(caret)
require(gdata)
library(binaryLogic)
for(i in c(2:2)){
  data <- read.xls(("data/input/tunning_mean_rainfall_two_season.xlsx"), sheet = i, header = TRUE)
  # file to save
  if(i ==1){
    svm.file <- "result/svm_output_season_5_10.txt"
  } else{
    svm.file <- "result/svm_output_season_11_4.txt"
  }
  # data <- read.table("data/rainfall_tuning/rainfall_tuning.dat", header=TRUE)
  # The top 5 variables (out of 26):
  #   R500, R850, P_z, P8_z, P500
  #   P8_z, R500, P500, P_z, P850, R850, Rhum, P5zh, P5_v || 9 features
  data.tuning <- data.frame(data$P8_z, data$R500, data$P500, data$P_z, data$P850, data$R850, data$Rhum, data$P5zh, data$P5_v, data$clazz)
  colnames(data.tuning) <- c("P8_z", "R500", "P500", "P_z", "P850", "R850", "Rhum", "P5zh", "P5_v", "clazz")
  # feature selection
  n.feature <- ncol(data.tuning) -1
  # # file to save
  # file <- "result/svm_output_season_5_10.txt"
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
    data.rain <- data.tuning[which(data.tuning$clazz == 1 | data.tuning$clazz == 2, arr.ind = FALSE),]
    data.rain.row <- nrow(data.rain)
    data.rain.ntrain <- round(data.rain.row * 0.7)
    data.rain.tindex <- sample(data.rain.row, data.rain.ntrain)
    data.rain.xtrain <- as.matrix(data.rain[data.rain.tindex, index.feature])
    data.rain.xtest <- as.matrix(data.rain[-data.rain.tindex, index.feature])
    data.rain.ytrain <- matrix(data.rain$clazz[data.rain.tindex])
    data.rain.ytest <- matrix(data.rain$clazz[-data.rain.tindex])
    # dry data
    data.dry <- data.tuning[which(data.tuning$clazz == -1, arr.ind = FALSE),]
    data.dry.row <- nrow(data.dry)
    data.dry.ntrain <- round(data.dry.row * 0.7)
    data.dry.tindex <- sample(data.dry.row, data.dry.ntrain)
    data.dry.xtrain <- as.matrix(data.dry[data.dry.tindex, index.feature])
    data.dry.xtest <- as.matrix(data.dry[-data.dry.tindex, index.feature])
    data.dry.ytrain <- matrix(data.dry$clazz[data.dry.tindex])
    data.dry.ytest <- matrix(data.dry$clazz[-data.dry.tindex])
    # merge data for train
    data.xtrain <- rbind(data.rain.xtrain, data.dry.xtrain)
    data.ytrain <- rbind(data.rain.ytrain, data.dry.ytrain)
    # merge data for test
    data.xtest <- rbind(data.rain.xtest, data.dry.xtest)
    data.ytest <- rbind(data.rain.ytest, data.dry.ytest)
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
        rain.acc <- c(sum(data.ypred[which(data.ytest==1)] == 1)/(sum(data.ytest==1)))
        dry.acc <- c(sum(data.ypred[which(data.ytest==-1)]==-1)/(sum(data.ytest==-1)))
        acc <- sum(data.ypred==data.ytest)/length(data.ytest)
        c <- c(i_c)
        sigma <- c(i_sig)
        accuracy <- c(acc)
        df <- data.frame(feature, c, sigma, rain.acc, dry.acc, accuracy)
        line <- toJSON(df)
        write(line,file=svm.file,append=TRUE)
      }
    }
  }
}

