# 
# 1964-1990
# 1991-2000
# load library
library(jsonlite)
library(binaryLogic)
require(e1071)
require(stats)
#
data.season.rain <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.rain.csv", header=TRUE)
# extreme rain data
ind.dry <- which(data.season.rain$clazz == -1)
data.tuning <- data.season.rain[-ind.dry, ]
# data.tuning$time
# raining data
data.tuning.row <- nrow(data.tuning)
data.tuning.ntrain <- round(data.tuning.row * 0.7304)
data.tuning.tindex <- c(1:data.tuning.ntrain) 
data.tuning.xtrain <- data.frame(data.tuning[data.tuning.tindex, ])
colnames(data.tuning.xtrain) <- colnames(data.tuning)
data.tuning.xtest <- data.frame(data.tuning[-data.tuning.tindex, ])
colnames(data.tuning.xtest) <- colnames(data.tuning)
# statistic on data test
sd.test <- sd(data.tuning.xtrain$rainfall)
mean.test <- mean(data.tuning.xtrain$rainfall)
# statistic on data train
sd.train <- sd(data.tuning.xtest$rainfall)
mean.train <- mean(data.tuning.xtest$rainfall)
#
df.statistic <- data.frame(mean.train, sd.train, mean.test, sd.test)
line <- toJSON(df.statistic)
print(line)
# data.tuning.xtrain$time

##########
##########
data.season.dry <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.dry.csv", header=TRUE)
ind.extreme.dry <- which(data.season.dry$clazz == 2)
rain.extreme <- data.season.dry[ind.extreme.dry, ]
ind.normal.dry <- which(data.season.dry$clazz == 1)






