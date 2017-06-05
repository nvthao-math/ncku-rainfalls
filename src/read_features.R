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
combine.feature <- data.frame(Mslp, P5_f, P5_u, P5_v, P5_z, P5th, P5zh, P8_f, P8_u, 
                              P8_v, P8_z, P8th, P8zh, P500, P850, P_f, P_u, P_v, P_z,
                              P_th, P_zh, R500, R850, Rhum, Shum, Temp)

