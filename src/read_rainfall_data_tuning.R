# read and combine features as a data frame
# Mslp, P5_f, P5_u, P5_v, P5_z, P5th, P5zh, P8_f
# P8_u, P8_v, P8_z, P8th, P8zh, P500, P850, P_f
# P_u, P_v, P_z, P_th, P_zh, R500, R850, Rhum, Shum, Temp
Mslp <- as.vector(unlist(read.table("data/weather_factor/ncepmslpas.dat")))
P5_f <- as.vector(unlist(read.table("data/weather_factor/ncepp5_fas.dat", sep="")))
P5_u <- as.vector(unlist(read.table("data/weather_factor/ncepp5_uas.dat", sep="")))
P5_v <- as.vector(unlist(read.table("data/weather_factor/ncepp5_vas.dat", sep="")))
P5_z <- as.vector(unlist(read.table("data/weather_factor/ncepp5_zas.dat", sep="")))
P5th <- as.vector(unlist(read.table("data/weather_factor/ncepp5thas.dat", sep="")))
P5zh <- as.vector(unlist(read.table("data/weather_factor/ncepp5zhas.dat", sep="")))
P8_f <- as.vector(unlist(read.table("data/weather_factor/ncepp8_fas.dat", sep="")))
P8_u <- as.vector(unlist(read.table("data/weather_factor/ncepp8_uas.dat", sep="")))
P8_v <- as.vector(unlist(read.table("data/weather_factor/ncepp8_vas.dat", sep="")))
P8_z <- as.vector(unlist(read.table("data/weather_factor/ncepp8_zas.dat", sep="")))
P8th <- as.vector(unlist(read.table("data/weather_factor/ncepp8thas.dat", sep="")))
P8zh <- as.vector(unlist(read.table("data/weather_factor/ncepp8zhas.dat", sep="")))
P500 <- as.vector(unlist(read.table("data/weather_factor/ncepp500as.dat", sep="")))
P850 <- as.vector(unlist(read.table("data/weather_factor/ncepp850as.dat", sep="")))
P_f <- as.vector(unlist(read.table("data/weather_factor/ncepp__fas.dat", sep="")))
P_u <- as.vector(unlist(read.table("data/weather_factor/ncepp__uas.dat", sep="")))
P_v <- as.vector(unlist(read.table("data/weather_factor/ncepp__vas.dat", sep="")))
P_z <- as.vector(unlist(read.table("data/weather_factor/ncepp__zas.dat", sep="")))
P_th <- as.vector(unlist(read.table("data/weather_factor/ncepp_thas.dat", sep="")))
P_zh <- as.vector(unlist(read.table("data/weather_factor/ncepp_zhas.dat", sep="")))
R500 <- as.vector(unlist(read.table("data/weather_factor/ncepr500as.dat", sep="")))
R850 <- as.vector(unlist(read.table("data/weather_factor/ncepr850as.dat", sep="")))
Rhum <- as.vector(unlist(read.table("data/weather_factor/nceprhumas.dat", sep="")))
Shum <- as.vector(unlist(read.table("data/weather_factor/ncepshumas.dat", sep="")))
Temp <- as.vector(unlist(read.table("data/weather_factor/nceptempas.dat", sep="")))
# combine features
# combine.feature <- data.frame(Mslp, P5_f, P5_u, P5_v, P5_z, P5th, P5zh, P8_f, P8_u, 
#                               P8_v, P8_z, P8th, P8zh, P500, P850, P_f, P_u, P_v, P_z,
#                               P_th, P_zh, R500, R850, Rhum, Shum, Temp)

# read file xlsx, dataset was collected from 01/01/1964 to 31/12/2000
require(gdata)
rainfall = read.xls ("data/rainfall/rainfall.xlsx", sheet = 1, header = TRUE)
# View(rainfall)
rows <- nrow(rainfall)
# cols <- ncol(rainfall)
time <- numeric(0)
precipitation <- character(0)
clazz <- character(0)
for(i in c(2:rows)){
  x.line <- as.vector(unname((unlist(rainfall[i,]))))
  x.year <- as.numeric(x.line[1]) 
  if(x.year <= 2000){
    time.prefix <- paste(x.line[1], "-",sprintf("%02d", as.numeric(x.line[2])), sep='')
    n.x <- length(x.line)
    for(j in c(3:n.x)){
      if(x.line[j] != "" && x.line[j] !="缺測"){
        out.time <- paste(time.prefix, "-", sprintf("%02d", (j-2)), sep='')
        time <- append(time, out.time)
        out.precipitation <- x.line[j] 
        precipitation <- append(precipitation, out.precipitation)
        if(out.precipitation > 0){
          clazz <- append(clazz, 1) # rain_day: 1
        } else {
          clazz <- append(clazz, -1) # dry_day: -1
        }
      }
    }
  }
}
data.tuning <- data.frame(time, Mslp, P5_f, P5_u, P5_v, P5_z, P5th, P5zh, P8_f, P8_u, 
                          P8_v, P8_z, P8th, P8zh, P500, P850, P_f, P_u, P_v, P_z,
                          P_th, P_zh, R500, R850, Rhum, Shum, Temp, precipitation, clazz,
                          stringsAsFactors = FALSE)
write.table(data.tuning, file="data/rainfall_tuning/rainfall_tuning.dat", row.names=FALSE, sep="\t", quote=FALSE)
