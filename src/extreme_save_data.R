#
# train data: 1964-1990
# test data: 1991-2000
#
data.season.rain <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.rain.csv", header=TRUE)
data.season.dry <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.dry.csv", header=TRUE)
# rain split to train and test
rain.train.index <- c(1:4654)
data.season.rain.train <- data.season.rain[rain.train.index,]
data.season.rain.test <- data.season.rain[-rain.train.index,]
# dry split to train and test
dry.train.index <- c(1:4691)
data.season.dry.train <- data.season.dry[dry.train.index,]
data.season.dry.test <- data.season.dry[-dry.train.index,]
# data train
ind.rain.train <- which(data.season.rain.train$clazz == 2 | data.season.rain.train$clazz == 1) 
ind.dry.train <- which(data.season.dry.train$clazz == 2 | data.season.dry.train$clazz == 1)
data.train <- rbind(data.season.rain.train[ind.rain.train, ], data.season.dry.train[ind.dry.train, ])
# write.table(data.train, file="data/input/extreme/extreme.data.train.dat", row.names=FALSE, sep="\t", quote=FALSE)
write.csv(data.train, file = "data/input/extreme/extreme.data.train.csv", row.names = FALSE)
# data test
ind.rain.test <- which(data.season.rain.test$clazz == 1 | data.season.rain.test$clazz == 2) 
ind.dry.test <- which(data.season.dry.test$clazz == 1 | data.season.dry.test$clazz == 2)
data.test <- rbind(data.season.rain.test[ind.rain.test, ], data.season.dry.test[ind.dry.test, ])
# write.table(data.test, file="data/input/extreme/extreme.data.test.dat", row.names=FALSE, sep="\t", quote=FALSE)
write.csv(data.test, file = "data/input/extreme/extreme.data.test.csv", row.names = FALSE)


