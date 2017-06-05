# ensure the results are repeatable
set.seed(7)
# load the library
library(mlbench)
library(caret)
# load data
data.tuning <- read.table("data/rainfall_tuning/rainfall_tuning.dat", header=TRUE)
# define the control using a random forest selection function
control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# run the RFE algorithm
results <- rfe(data.tuning[,2:27], data.tuning[,29], sizes=c(1:26), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))

### result ####
# The top 5 variables (out of 26):
#   R500, R850, P_z, P8_z, P500

