# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load data
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
data <- rbind(rain.extreme, rain.normal)
data.tuning <- data.frame(data[, -c(1,2,3, (ncol(data)-1))])
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(data.tuning[,1:26], data.tuning[,27], sizes=c(1:26), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

### result ####
# The top 5 variables (out of 26):
#   R500, R850, P_z, P8_z, P500

