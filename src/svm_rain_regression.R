# load library
library(jsonlite)
library(binaryLogic)
require(e1071)
require(stats)
#
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
interval <- 102
data <- rbind(rain.extreme, rain.normal)
data <- data[which(data$rainfall > interval), ]
# data[which(data$rainfall <= interval), ]
data.tuning <- data.frame(data$P_z, data$P8_z, data$P8_u, data$P5_u, data$P850, data$P8_f, data$R850, data$P5_f, data$P5_z, data$R500, data$P_u, data$P8th, data$P_v, data$Mslp, data$P500, data$Rhum, data$P5th, data$P_f, data$P8_v, data$P_zh, data$P_th, data$Temp, data$Shum, data$rainfall)
colnames(data.tuning) <- c("P_z", "P8_z", "P8_u", "P5_u", "P850", "P8_f", "R850", "P5_f", "P5_z", "R500", "P_u", "P8th", "P_v", "Mslp", "P500", "Rhum", "P5th", "P_f", "P8_v", "P_zh", "P_th", "Temp", "Shum", "rainfall")
# feature selection
n.feature <- ncol(data.tuning) -1
# # file to save
file.regression <- "/home/bigdata/workspace/project/r/ncku-rainfalls/result/svm_output_regression.txt"
#
for(i in c(39664:(2^n.feature-1))){
  x.bi <- as.binary(i, n=n.feature)
  index.feature <- which(x.bi)
  print(x.bi)
  print(index.feature)
  head(data.tuning[,index.feature])
  #
  feature_vec <- colnames(data.tuning)[index.feature]
  feature <- paste(feature_vec, collapse=", ")
  # raining data
  data.tuning.row <- nrow(data.tuning)
  data.tuning.ntrain <- round(data.tuning.row * 0.7)
  data.tuning.tindex <- sample(data.tuning.row, data.tuning.ntrain)
  data.tuning.xtrain <- as.matrix(data.tuning[data.tuning.tindex, index.feature])
  colnames(data.tuning.xtrain) <- feature_vec
  data.tuning.xtest <- as.matrix(data.tuning[-data.tuning.tindex, index.feature])
  colnames(data.tuning.xtest) <- feature_vec
  data.tuning.ytrain <- matrix(data.tuning$rainfall[data.tuning.tindex])
  data.tuning.ytest <- data.tuning$rainfall[-data.tuning.tindex]
  # merge data for train
  data.train <- data.frame(cbind(data.tuning.xtrain, data.tuning.ytrain))
  colnames(data.train) <- c(feature_vec, "rainfall")
  # merge data for test
  data.xtest <- data.tuning.xtest
  colnames(data.xtest) <- feature_vec
  data.ytest <- data.tuning.ytest
  ## svm regression | perform a grid search
  tuneResult <- tune(svm, rainfall ~.,  data = data.train, ranges = list(epsilon = c(0.1), cost = c(1), gamma = c(0.1)))
  print(tuneResult)
  tunedModel <- tuneResult$best.model
  y.predict <- predict(tunedModel, data.xtest) 
  extreme.acc <- length(which(y.predict >= interval)) / length(y.predict)
  error <- data.ytest - y.predict  
  rmse <- sqrt(mean(error^2)) # rmse(error)
  df <- data.frame(feature, extreme.acc, rmse)
  line <- toJSON(df)
  print(line)
  write(line, file=file.regression, append=TRUE)
  #
  # qqplot(data.ytest, y.predict)
  # abline(a=0, b=1, lty="dotted")
}
