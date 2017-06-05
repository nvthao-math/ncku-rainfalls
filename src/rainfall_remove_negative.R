#
library(kernlab)
library(jsonlite)
library(caret)
require(gdata)
library(binaryLogic)
#
data.season.rain <- read.xls(("data/input/tunning_mean_rainfall_two_season.xlsx"), sheet = 1, header = TRUE)
data.season.dry <- read.xls(("data/input/tunning_mean_rainfall_two_season.xlsx"), sheet = 2, header = TRUE)
#
neg.ind.season.rain <- which(data.season.rain$rainfall < 0)
twoseason.rain <- data.season.rain[-neg.ind.season.rain, ]
write.csv(twoseason.rain, file = "data/input/twoseason.rain.csv")
#
neg.ind.season.dry <- which(data.season.dry$rainfall < 0)
twoseason.dry <- data.season.dry[-neg.ind.season.dry, ]
write.csv(twoseason.dry, file = "data/input/twoseason.dry.csv")
