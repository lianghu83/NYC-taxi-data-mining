###############################################################################
# This file checks whether each dwell location is within Manhattan.

rm(list=ls())

library(sp)
library(maptools)
library(rgeos)
library(rgdal)
library(XML)
library(dismo)

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
  
#set Manhattan map
Manhattan <- SpatialPoints(gmap("Manhattan, NY", lonlat = TRUE), 
                           proj4string = CRS("+proj=longlat"))
Manhattan <- gConvexHull(Manhattan)
  
#loop over each taxi  
for (j in 1:length(filename)){ 
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
  df <- read.csv(file=filename[j])[,c('dwell_yes', 'dropoff_longitude', 'dropoff_latitude')]
  
  df$Manhattan_yes <- 0
  
  for (i in 1:nrow(df)){
    if (df$dwell_yes[i] == 1){
      pt <- matrix(nrow = 1, ncol = 2)
      pt[1,1] <- df$dropoff_longitude[i]
      pt[1,2] <- df$dropoff_latitude[i]
      point <- SpatialPoints(pt, proj4string = CRS("+proj=longlat"))
      if (gContains(Manhattan, point, byid = F)==TRUE) {df$Manhattan_yes[i]=1} 
    }
  }
  
  #write
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F7")
  write.csv(df, file = filename[j], row.names = F)
  
}
  
  
  
  

## Calculate Manhattan dwells / all dwells
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F7")
total <- as.data.frame(filename)
total$Vehicle_ID <- substr(total[,1], 1, 10)

for (i in 1:length(filename)){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F7")
  df <- read.csv(file=filename[i])[,c('dwell_yes', 'Manhattan_yes')]
  
  total$Manhattan[i] <- round(sum(df$Manhattan_yes)/sum(df$dwell_yes), 4)
  
  print(i)
}

total <- total[, -1]

driver <- read.csv('C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_driver_stat.csv')

aa <- merge(driver, total, by='Vehicle_ID', all.x=TRUE)

write.csv(aa, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_driver_stat.csv', row.names = F)


