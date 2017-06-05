# load library
require(MASS)
library(jsonlite)
library(caret)
library(binaryLogic)
data.season.dry <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.dry.csv", header=TRUE)
# extreme rain data
ind.extreme.dry <- which(data.season.dry$clazz == 2)
rain.extreme <-  data.season.dry[ind.extreme.dry, ]
# normal rain data
ind.normal.dry <- which(data.season.dry$clazz == 1)
rain.normal <- data.season.dry[ind.normal.dry, ]
#
data <- rbind(rain.extreme, rain.normal)
data.tuning <- data.frame(data$P_z, data$P8_z, data$P8_u, data$P5_u, data$P850, data$P8_f, data$R850, data$P5_f, data$P5_z, data$R500, data$P_u, data$P8th, data$P_v,  data$Mslp, data$P500, data$Rhum, data$P5th, data$P_f,  data$P8_v, data$P_zh, data$P_th, data$Temp, data$Shum, data$clazz)
colnames(data.tuning) <- c("P_z", "P8_z", "P8_u", "P5_u", "P850", "P8_f", "R850", "P5_f", "P5_z", "R500", "P_u",  "P8th", "P_v",  "Mslp", "P500", "Rhum", "P5th", "P_f",  "P8_v", "P_zh", "P_th", "Temp", "Shum", "clazz")
# feature selection
n.feature <- ncol(data.tuning) -1
# # file to save
file <- "/home/bigdata/workspace/project/r/ncku-rainfalls/result/lda_dry_output_extreme_normal.txt"
#
threshold <- 0.6
for(i in c(58316:(2^n.feature-1))){
  tryCatch({
    x.bi <- as.binary(i, n=n.feature)
    index.feature <- which(x.bi)
    print(x.bi)
    print(index.feature)
    head(data.tuning[,index.feature])
    #
    feature_vec <- colnames(data.tuning)[index.feature]
    feature <- paste(feature_vec, collapse=", ")
    ## Prepare a training and a test set ##      
    z.data.select <-cbind((data.tuning)[index.feature], data.tuning$clazz)
    z.data <-z.data.select[sample(nrow(z.data.select)),]
    folds <- cut(seq(1,nrow(z.data)),breaks=4,labels=FALSE)
    #Perform 10 fold cross validation
    temp.result <- matrix(0,1,6)
    for(j in 1:4){
      #Segement your data by fold using the which() function 
      testIndexes <- which(folds==j, arr.ind=TRUE)
      testData <- z.data[testIndexes, ] # number of test samples
      data.xtest <- as.matrix(testData[,-ncol(testData)])
      data.ytest <- testData[,ncol(testData)] # 
      trainData <- z.data[-testIndexes, ] # number of training samples
      data.xtrain <- as.matrix(trainData[,-ncol(trainData)])
      data.ytrain <- trainData[,ncol(trainData)]
      # build model
      ytrain <- factor(data.ytrain, labels = c("yes", "no"))
      lda.model <- lda(data.xtrain, ytrain)
      ytest <- factor(data.ytest, labels = c("yes", "no"))
      pred.result.test <- predict(lda.model,data.xtest)
      data.ypred.test <- pred.result.test$class
      table(data.ypred.test, ytest)
      # accuracy on testing set
      extreme.acc.test <- sum(data.ypred.test[which(ytest=="no")] == "no")/(sum(ytest=="no"))
      normal.acc.test <- sum(data.ypred.test[which(ytest=="yes")]=="yes")/(sum(ytest=="yes"))
      acc.test <- sum(data.ypred.test==ytest)/length(ytest)
      # accuracy on training set
      pred.result.train <- predict(lda.model, data.xtrain)
      data.ypred.train <- pred.result.train$class
      extreme.acc.train <- sum(data.ypred.train[which(ytrain=="no")] == "no")/(sum(ytrain=="no"))
      normal.acc.train <- sum(data.ypred.train[which(ytrain=="yes")]=="yes")/(sum(ytrain=="yes"))
      acc.train <- sum(data.ypred.train==ytrain)/length(ytrain)
      #
      acc.temp <- c(extreme.acc.test, normal.acc.test, acc.test, extreme.acc.train, normal.acc.train, acc.train)
      temp.result <- rbind(temp.result, acc.temp);
    }
    tar.temp <- as.vector(colMeans(temp.result))
    extreme.acc.test <- tar.temp[1]
    normal.acc.test  <- tar.temp[2]
    acc.test  <- tar.temp[3]
    extreme.acc.train  <- tar.temp[4]
    normal.acc.train  <- tar.temp[5]
    acc.train  <- tar.temp[6]
    #
    result.df <- data.frame(feature, extreme.acc.test, normal.acc.test, acc.test, extreme.acc.train, normal.acc.train, acc.train)
    line <- toJSON(result.df)
    print(line)
    write(line,file=file,append=TRUE)
    # save data train to build model
    if(extreme.acc.test > threshold){
      threshold <- extreme.acc.test
      train2BuildModel <- cbind(data.xtrain, data.ytrain)
      colnames(train2BuildModel) <- c(feature, "clazz")
      write.csv(train2BuildModel, file = "/home/bigdata/workspace/project/r/ncku-rainfalls/data/buildModel/extrem_vs_normal/lda_dry_trainData.csv")
    }
  })
  
}
