# load library
library(jsonlite)
library(binaryLogic)
require(e1071)
require(stats)
#
data.season.rain <- read.csv("/home/cpu10869-local/R/ncku-rainfalls/data/input/twoseason.rain.csv", header=TRUE)
data.season.dry <- read.csv("/home/cpu10869-local/R/ncku-rainfalls/data/input/twoseason.dry.csv", header=TRUE)
# extreme rain data
ind.extreme.rain <- which(data.season.rain$clazz == 2)
ind.extreme.dry <- which(data.season.dry$clazz == 2)
rain.extreme <- rbind(data.season.rain[ind.extreme.rain, ], data.season.dry[ind.extreme.dry, ])
# normal rain data
ind.normal.rain <- which(data.season.rain$clazz == 1)
ind.normal.dry <- which(data.season.dry$clazz == 1)
rain.normal <- rbind(data.season.rain[ind.normal.rain, ], data.season.dry[ind.normal.dry, ])
#
interval.x <- 106
interval.y <- 1000
data.merge <- rbind(rain.extreme, rain.normal)
data <- data.merge[which(interval.x < data.merge$rainfall & data.merge$rainfall < interval.y), ]
# data[which(data$rainfall <= interval), ]
data.tuning <- data.frame(data$P5_z, data$P850, data$rainfall)
colnames(data.tuning) <- c("P5_z", "P850", "rainfall")
# feature selection
n.feature <- ncol(data.tuning) -1
# # file to save
file.regression <- "/home/cpu10869-local/R/ncku-rainfalls/result/2_intervals/svm_2_interval_03_params_regression.txt"
#
x.bi <- as.binary(2^n.feature-1, n=n.feature)
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
# scan result
# epsilon = seq(0,1,0.01)
# cost = c(1:100)
# gamma = seq(0, 10, 0.01)
##### "ep_i":0,"c_i":7,"g_i":0.72,
epsilon = seq(0.2,0.3,0.0001)
cost = seq(6, 8, 0.1)
gamma = seq(0, 0.8, 0.001)
##
for(ep_i in epsilon){
  for(c_i in cost){
    for(g_i in gamma){
      # tuneResult <- tune(svm, rainfall ~.,  data = data.train, ranges = list(epsilon = ep_i, cost = c_i, gamma = g_i))
      tunedModel <- svm(rainfall ~.,  data = data.train, cost = c_i, gamma = g_i, epsilon = ep_i)
      # test acc
      # tunedModel <- tuneResult$best.model
      y.predict.test <- predict(tunedModel, data.xtest)
      error.test <- data.ytest - y.predict.test  
      rmse.test <- sqrt(mean(error.test^2)) # rmse(error)
      # rain acc
      y.predict.train <- predict(tunedModel, data.tuning.xtrain); error.train <- data.tuning.ytrain - y.predict.train  
      rmse.train <- sqrt(mean(error.train^2)) # rmse(error)
      #
      sd.ytest <- sd(data.ytest)
      mean.ytest <- mean(data.ytest)
      sd.y.predict.test <- sd(y.predict.test)
      mean.y.predict.test <- mean(y.predict.test)
      sd.ytrain <- sd(data.tuning.ytrain)
      mean.ytrain <- mean(data.tuning.ytrain)
      sd.y.predict.train <- sd(y.predict.train)
      mean.y.predict.train <- mean(y.predict.train)
      #
      df <- data.frame(feature, ep_i, c_i, g_i, rmse.test, rmse.train, sd.ytest, sd.y.predict.test, mean.ytest, mean.y.predict.test, sd.ytrain, sd.y.predict.train, mean.ytrain, mean.y.predict.train)
      line <- toJSON(df)
      print(line); #  
      write(line, file=file.regression, append=TRUE)
    }
  }
}

