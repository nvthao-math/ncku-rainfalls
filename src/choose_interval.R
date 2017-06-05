#
#
data.season.rain <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.rain.csv", header=TRUE)
data.season.dry <- read.csv("/home/bigdata/workspace/project/r/ncku-rainfalls/data/input/twoseason.dry.csv", header=TRUE)
#
data <- rbind(data.season.rain, data.season.dry)
data.tuning <- data[which(data$rainfall > 0), ]
x <- c(data.tuning$rainfall)
km2 = kmeans(x, 2)
x.cluster <- data.frame(x, km2$cluster)
names(x.cluster) <- c("val", "cluster")
##
x.clazz.1 <- x.cluster[which(x.cluster$cluster==1), ]
summary(x.clazz.1$val)

#
x.clazz.2 <- x.cluster[which(x.cluster$cluster==2), ]
summary(x.clazz.2$val)
#

table(x.cluster$cluster)

sd(data$rainfall)
sd(x.clazz.1$val)
sd(x.clazz.2$val)


