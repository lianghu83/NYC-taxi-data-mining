#################################################################################
## This files calculates spatial statistics. 



# First, convert GPS to New York State Plane Coordinates
rm(list = ls())
setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data")
df <- read.csv("By vehicle_GPS_dwell_without_charge.csv", header = F)
colnames(df) <- c('dropoff_longitude', 'dropoff_latitude')
library(rgdal)
library(sp)
coordinates(df) <- ~ dropoff_longitude+dropoff_latitude
proj4string(df) <- CRS("+init=epsg:4326") #WGS84, which is GPS coordinate
df.proj <- as.data.frame(spTransform(df, CRS("+init=esri:102718"))) #NAD 1983 StatePlane New York Long Island FIPS 3104 Feet
write.csv(df.proj, file="By vehicle_Stateplane_dwell_without_charge.csv", row.names = F)



# Spatial distribution of dwell locations
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")

setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")
total <- read.csv(filename[1])[,c('dropoff_longitude', 'dropoff_latitude', 'dwell_time_yes')]
total <- subset(total, total$dwell_time_yes != 0)
total <- total[,-3]

for (i in 2:length(filename)){
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")
  df <- read.csv(filename[i])[,c('dropoff_longitude', 'dropoff_latitude', 'dwell_time_yes')]
  df <- subset(df, df$dwell_time_yes != 0)
  df <- df[,-3]
  
  total <- rbind(total, df)

  print(i)
}

write.csv(total, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_GPS_dwell.csv', row.names = F)



# Spatial distribution of dwell with charging locations
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")

setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")
total <- read.csv(filename[1])[,c('dropoff_longitude', 'dropoff_latitude', 'dwell_time_yes', 'charge_yes')]
total <- subset(total, total$dwell_time_yes != 0 & total$charge_yes == 1)
total <- total[,-c(3,4)]

for (i in 2:length(filename)){
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")
  df <- read.csv(filename[i])[,c('dropoff_longitude', 'dropoff_latitude', 'dwell_time_yes', 'charge_yes')]
  df <- subset(df, df$dwell_time_yes != 0 & df$charge_yes == 1)
  df <- df[,-c(3,4)]
  
  total <- rbind(total, df)
  
  print(i)
}

write.csv(total, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_GPS_dwell_with_charge.csv', row.names = F)



# Spatial distribution of dwell without charging opportunities
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")

setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
total <- read.csv(filename[1])[,c('dropoff_longitude', 'dropoff_latitude', 'dwell_yes', 'charge_yes')]
total <- subset(total, total$dwell_yes == 1 & total$charge_yes == 0)
total <- total[,-c(3,4)]

for (i in 2:length(filename)){
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
  df <- read.csv(filename[i])[,c('dropoff_longitude', 'dropoff_latitude', 'dwell_yes', 'charge_yes')]
  df <- subset(df, df$dwell_yes == 1 & df$charge_yes == 0)
  df <- df[,-c(3,4)]
  
  total <- rbind(total, df)
  
  if(i%%500==0) print(i)
}

write.csv(total, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_GPS_dwell_without_charge.csv', row.names = F)

#merge
rm(list=ls())

library(parallel)
no_cores <- 7
cl <- makeCluster(no_cores)

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")

f <- function(filename){
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
  df <- read.csv(file=filename)[,c('dropoff_longitude', 'dropoff_latitude', 'dwell_yes', 'charge_yes')]
  df <- subset(df, df$dwell_yes == 1 & df$charge_yes == 0)
  df <- df[,-c(3,4)]
  
  #output but remove header
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_GPS_dwell_without_charge")
  write.table(df, file = filename, row.names = F, col.names = F, sep = ',')
}

parLapply(cl, filename, f)
stopCluster(cl)



# Spatial distribution of running-out-of-electricity locations
# 200 mile scenario
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")

setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")
total <- read.csv(filename[1])[,c('dropoff_longitude', 'dropoff_latitude', 'SOC_2', 'flag')]
aa <- diff(total$flag)
aa[nrow(total)] <- NA
total$diff <- aa
total <- subset(total, total$diff == -1)
total <- total[,-c(3,4,5)]

for (i in 2:length(filename)){
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")
  df <- read.csv(filename[i])[,c('dropoff_longitude', 'dropoff_latitude', 'SOC_2', 'flag')]
  aa <- diff(df$flag)
  aa[nrow(df)] <- NA
  df$diff <- aa
  df <- subset(df, df$diff == -1)
  df <- df[,-c(3,4,5)]
  
  total <- rbind(total, df)
  
  print(i)
}

write.csv(total, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_GPS_no_electricity_200.csv', row.names = F)



# Spatial distribution of running-out-of-electricity locations
# 300 mile scenario
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")

setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")
total <- read.csv(filename[1])[,c('dropoff_longitude', 'dropoff_latitude', 'SOC_4', 'flag_3')]
aa <- diff(total$flag_3)
aa[nrow(total)] <- NA
total$diff <- aa
total <- subset(total, total$diff == -1)
total <- total[,-c(3,4,5)]

for (i in 2:length(filename)){
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F6")
  df <- read.csv(filename[i])[,c('dropoff_longitude', 'dropoff_latitude', 'SOC_4', 'flag_3')]
  aa <- diff(df$flag_3)
  aa[nrow(df)] <- NA
  df$diff <- aa
  df <- subset(df, df$diff == -1)
  df <- df[,-c(3,4,5)]
  
  total <- rbind(total, df)
  
  print(i)
}

write.csv(total, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_GPS_no_electricity_300.csv', row.names = F)


