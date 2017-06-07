#
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
# interval > 186
# 32.81 < interval < 186
# 0 < interval < 32.81
interval.x <- 186
interval.y <- 1000 # , 102
data.merge <- rbind(rain.extreme, rain.normal)
x.name <- c("P5_z", "P5th", "P5zh", "P8_f", "P8_u", "P8_v", "P8_z", "P8zh", "P850", "P_f", "P_u", "P_v", "P_z", "Rhum", "Shum", "P5_u", "R850", "P8th", "Mslp", "P500", "P_zh", "P_th", "Temp", "rainfall")
data.merge <- data.frame(data.merge$P5_z, data.merge$P5th, data.merge$P5zh, data.merge$P8_f, data.merge$P8_u, data.merge$P8_v, data.merge$P8_z, data.merge$P8zh, data.merge$P850, data.merge$P_f, data.merge$P_u, data.merge$P_v, data.merge$P_z, data.merge$Rhum, data.merge$Shum, data.merge$P5_u, data.merge$R850, data.merge$P8th, data.merge$Mslp, data.merge$P500, data.merge$P_zh, data.merge$P_th, data.merge$Temp, data.merge$rainfall)
colnames(data.merge) <- x.name
#
data.tuning <- data.merge[which(interval.x < data.merge$rainfall & data.merge$rainfall < interval.y), ]
# data[which(data$rainfall <= interval), ]
n.feature <- ncol(data.tuning) -1
file.regression <- "/home/bigdata/workspace/project/r/ncku-rainfalls/result/3_intervals/svm_3_interval_03_regression.txt"
# 1:2000000
# 2000001:4000000
# 4000001:6000000
# 6000001:8388607
for(i in c(6000001:8388607)){ # 1:(2^n.feature -1)
  x.bi <- as.binary(i, n=n.feature)
  index.feature <- which(x.bi)
  print(index.feature)
  head(data.tuning[,index.feature])
  feature_vec <- colnames(data.tuning)[index.feature]
  feature <- paste(feature_vec, collapse=", ")
  #
  # raining data
  data.tuning.row <- nrow(data.tuning)
  data.tuning.ntrain <- round(data.tuning.row * 0.7)
  data.tuning.tindex <- c(1:data.tuning.ntrain) # sample(data.tuning.row, data.tuning.ntrain)
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
  tuneResult <- tune(svm, rainfall ~.,  data = data.train, ranges = list(epsilon = 0.1, cost = 1, gamma = 1))
  # test acc
  tunedModel <- tuneResult$best.model
  y.predict.test <- predict(tunedModel, data.xtest)
  error.test <- data.ytest - y.predict.test  
  rmse.test <- sqrt(mean(error.test^2)) # rmse(error)
  # rain acc
  y.predict.train <- predict(tunedModel, data.tuning.xtrain); error.train <- data.tuning.ytrain - y.predict.train  
  rmse.train <- sqrt(mean(error.train^2)) # rmse(error)
  # mean, sd
  sd.ytest <- sd(data.ytest)
  mean.ytest <- mean(data.ytest)
  sd.y.predict.test <- sd(y.predict.test)
  mean.y.predict.test <- mean(y.predict.test)
  sd.ytrain <- sd(data.tuning.ytrain)
  mean.ytrain <- mean(data.tuning.ytrain)
  sd.y.predict.train <- sd(y.predict.train)
  mean.y.predict.train <- mean(y.predict.train)
  df <- data.frame(i, feature, sd.ytest, mean.ytest, sd.y.predict.test, mean.y.predict.test, sd.ytrain, mean.ytrain, sd.y.predict.train, mean.y.predict.train, rmse.test, rmse.train)
  line <- toJSON(df)
  print(line)
  write(line, file=file.regression, append=TRUE)
}
