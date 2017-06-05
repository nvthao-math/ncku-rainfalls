# load library
library(jsonlite)
library(binaryLogic)
require(e1071)
require(stats)
#
data.season.rain <- read.csv("data/input/twoseason.rain.csv", header=TRUE)
data.season.dry <- read.csv("data/input/twoseason.dry.csv", header=TRUE)
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
interval.y <- 1000 # 186, 102
data.merge <- rbind(rain.extreme, rain.normal)
data <- data.merge[which(interval.x < data.merge$rainfall & data.merge$rainfall < interval.y), ]
# data[which(data$rainfall <= interval), ]
data.tuning <- data.frame(data$P8_z, data$R500, data$R850, data$Rhum, data$rainfall)
colnames(data.tuning) <- c("P8_z", "R500", "R850", "Rhum", "rainfall")
# feature selection
n.feature <- ncol(data.tuning) -1
# # file to save
file.regression <- "result/poly_scanner_output_regression.txt"
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
# tuneResult <- tune(svm, rainfall ~.,  data = data.train, ranges = list(epsilon = ep_i, cost = c_i, gamma = g_i))
model <- lm(rainfall ~., data = data.train)
# test acc
# tunedModel <- tuneResult$best.model
# y.predict.test <- predict(tunedModel, data.xtest)
y.predict.test <- as.vector(predict(model,data.frame(data.xtest),interval='confidence', level=0.99)[,1]) 
error.test <- data.ytest - y.predict.test
rmse.test <- sqrt(mean(error.test^2)) # rmse(error)
# rain acc
y.predict.train <- predict(model, data.frame(data.tuning.xtrain)); 
error.train <- data.tuning.ytrain - y.predict.train
rmse.train <- sqrt(mean(error.train^2)) # rmse(error)
 #
df <- data.frame(feature, rmse.test, rmse.train);
line <- toJSON(df);
print(line); #
#
# # plot data test
x.bar.test <- c(1:length(y.predict.test))
plot(x.bar.test, data.ytest, pch=16)
lines(x.bar.test, y.predict.test, col = "yellow", pch=4)
qqplot(data.ytest, y.predict.test)
abline(a=1, b=1, lty="dotted")
sd(data.ytest)
mean(data.ytest)
sd(y.predict.test)
mean(y.predict.test)
# plot data train
x.bar.train <- c(1:length(data.tuning.ytrain))
sd(data.tuning.ytrain)
mean(data.tuning.ytrain)
sd(y.predict.train)
mean(y.predict.train)
plot(x.bar.train, data.tuning.ytrain)
lines(x.bar.train, y.predict.train, col = "yellow", pch=4)
qqplot(data.tuning.ytrain, y.predict.train)
abline(a=1, b=1, lty="dotted")

y.pred.combine <- as.vector(c(y.predict.test, y.predict.train))
qqplot(y.pred.combine, data.tuning$rainfall)
abline(a=1, b=1, lty="dotted")

      