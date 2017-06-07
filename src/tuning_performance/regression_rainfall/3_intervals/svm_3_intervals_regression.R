
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
interval.x <- 0
interval.y <- 32.81 # , 102
data.merge <- rbind(rain.extreme, rain.normal)
data <- data.merge[which(interval.x < data.merge$rainfall & data.merge$rainfall < interval.y), ]
# data[which(data$rainfall <= interval), ]
data.tuning <- data.frame(data$P8_z, data$R500, data$R850, data$Rhum, data$rainfall)
colnames(data.tuning) <- c("P8_z", "R500", "R850", "Rhum", "rainfall")
# feature selection
n.feature <- ncol(data.tuning) -1
# # file to save
file.regression <- "/home/bigdata/workspace/project/r/ncku-rainfalls/result/svm_scanner_output_regression.txt"
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
tuneResult <- tune(svm, rainfall ~.,  data = data.train, ranges = list(epsilon = 0.01, cost = 1, gamma = 0.1))
# test acc
tunedModel <- tuneResult$best.model
y.predict.test <- predict(tunedModel, data.xtest)
error.test <- data.ytest - y.predict.test  
rmse.test <- sqrt(mean(error.test^2)) # rmse(error)
# rain acc
y.predict.train <- predict(tunedModel, data.tuning.xtrain); error.train <- data.tuning.ytrain - y.predict.train  
rmse.train <- sqrt(mean(error.train^2)) # rmse(error)
#
df <- data.frame(feature, rmse.test, rmse.train)
line <- toJSON(df)
print(line)
# visualization
# # plot data test
x.bar.test <- c(1:length(y.predict.test))
plot(x.bar.test, data.ytest, pch=16)
points(x.bar.test, y.predict.test, col = "yellow", pch=4)
qqplot(data.ytest, y.predict.test)
abline(a=1, b=1, lty="dotted")
# plot data train 
x.bar.train <- c(1:length(data.tuning.ytrain))
plot(x.bar.train, data.tuning.ytrain)
points(x.bar.train, y.predict.train, col = "yellow", pch=4)
qqplot(data.tuning.ytrain, y.predict.train)
abline(a=0, b=0.5, lty="dotted")

