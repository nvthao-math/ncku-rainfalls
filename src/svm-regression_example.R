#
# Load the data from the csv file
dataDirectory <- "/home/bigdata/workspace/project/r/ncku-rainfalls/data/test/regression.csv"
data <- read.csv(dataDirectory, header = TRUE)
# Plot the data
plot(data, pch=16)

# Create regression model
model <- svm(Y ~ X , data)
predictedY <- predict(model, data)
points(data$X, predictedY, col = "yellow", pch=4)

#
qqplot(predictedY, data$Y)
abline(a=0, b=1, lty="dotted")

