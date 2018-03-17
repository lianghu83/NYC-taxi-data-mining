#######################################################################
## This file extracts travel pattern variables.



## % of dwells in Manhattan
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F7")
total <- as.data.frame(filename)
total$Vehicle_ID <- substr(total[,1], 1, 10)

for (i in 1:length(filename)){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F7")
  df <- read.csv(file=filename[i])[,c('dwell_yes', 'Manhattan_yes')]
  
  total$Manhattan[i] <- round(sum(df$Manhattan_yes)/sum(df$dwell_yes), 4)
  
  print(i)
}

total <- total[, -1]

setwd('C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data')
write.csv(total, file = 'By vehicle_F8.csv', row.names = F)



## BEV 200 feasibility, BEV 300 feasibility
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
total <- as.data.frame(filename)
total$Vehicle_ID <- substr(total[,1], 1, 10)

for (i in 1:length(filename)){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
  df <- read.csv(file=filename[i])[,c('flag', 'flag_3')]
  
  total$Feasibility_200[i] <- round(sum(df$flag)/nrow(df), 4)
  total$Feasibility_300[i] <- round(sum(df$flag_3)/nrow(df), 4)
  
  print(i)
}

total <- total[, -1]

table <- read.csv('C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8.csv')

aa <- merge(table, total, by='Vehicle_ID', all.x=TRUE)

write.csv(aa, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8.csv', row.names = F)



## average occupied trip distance
## number of days with records
## average DVMT, daily number of dwells
## number of drivers with records
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
total <- as.data.frame(filename)
total$Vehicle_ID <- substr(total[,1], 1, 10)

for (i in 1:length(filename)){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
  df <- read.csv(file=filename[i])[,c('date', 'trip_distance', 'total_distance', 'dwell_time_yes', 'dwell_yes', 'hack_license', 'pickup_datetime')]
  
  #use new updated date
  df$pickup_datetime <- as.POSIXct(df$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  df$date_new <- as.Date(df$pickup_datetime, tz=attr(df$pickup_datetime, "tzone"))
  
  total$num_day_Review[i] <- length(unique(df$date_new))
  #total$ave_trip_distance[i] <- round(mean(df$trip_distance), 2)
  total$DVMT_Review[i] <- round(sum(df$total_distance)/total$num_day_Review[i], 2)
  #total$ave_dwell_time[i] <- round(mean(df$dwell_time_yes[df$dwell_time_yes>0])/60, 0) #in minutes
  total$daily_dwell_Review[i] <- round(sum(df$dwell_yes)/total$num_day_Review[i], 2)
  #total$num_driver[i] <- length(unique(df$hack_license))
  
  if(i%%500==0) print(i)
}

total <- total[, -1]

table <- read.csv('C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8_Review.csv')

aa <- merge(table, total, by='Vehicle_ID', all.x=TRUE)

write.csv(aa, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8_Review.csv', row.names = F)







## average dwell times
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
total <- as.data.frame(filename)
total$Vehicle_ID <- substr(total[,1], 1, 10)

for (i in 1:length(filename)){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
  df <- read.csv(file=filename[i])[, c('pickup_datetime', 'dwell_time_yes', 'dwell_yes')]
  
  df$pickup_datetime <- as.POSIXct(df$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  df$date <- as.Date(df$pickup_datetime, tz=attr(df$pickup_datetime, "tzone"))
  df$date_diff <- append(as.numeric(diff(df$date)), 0)
  df <- subset(df, df$dwell_time_yes>0)
  for (j in 1:nrow(df)){
    if (df$date_diff[j]>=2) {
      df$dwell_time_yes[j] <- df$dwell_time_yes[j] - (df$date_diff[j] - 1)*86400
    }
  }
  total$ave_dwell_time_3[i] <- round(mean(df$dwell_time_yes)/60, 0) #in minutes
  
  if(i%%500==0) print(i)
}

total <- total[, -1]

table <- read.csv('C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8_Review.csv')

aa <- merge(table, total, by='Vehicle_ID', all.x=TRUE)

write.csv(aa, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8_Review.csv', row.names = F)



## number of shift per day
rm(list=ls())

library(parallel)
no_cores <- 7
cl <- makeCluster(no_cores)

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")

f <- function(filename){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
  df <- read.csv(file=filename, stringsAsFactors = F)[,c('date', 'hack_license')]
  
  day <- unique(df$date)
  day <- day[order(day)]
  
  s <- split(df, df$date)
  
  # a new shift if hack_license changes
  num_shift <- sapply(s, function(x) {
    length(unique(x[,c("hack_license")],
                  na.rm = T))
  })
  aa <- as.data.frame(num_shift)
  aa$date <- day
  
  aa <- aa[,c(2,1)]
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_Shift")
  write.csv(aa, file = filename, row.names = F)
}

parLapply(cl, filename, f)
stopCluster(cl)



## descriptive statistics
rm(list = ls())
source("U:\\Desktop\\NYC Taxi Code\\getmode.R")
filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_Shift")
total <- as.data.frame(filename)
total$Vehicle_ID <- substr(total[,1], 1, 10)

for (i in 1:length(filename)){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_Shift")
  df <- read.csv(file=filename[i])
  
  total$ave_shift[i] <- round(mean(df$num_shift), 2)
  total$mode_shift[i] <- getmode(df$num_shift)
  
  if(i%%500==0) print(i)
}

total <- total[, -1]

table <- read.csv('C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8.csv')

aa <- merge(table, total, by='Vehicle_ID', all.x=TRUE)

write.csv(aa, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8.csv', row.names = F)



## travel distance between 2 charging opportunities
## under future expanded charging infrastructure
rm(list=ls())

library(parallel)
no_cores <- 7
cl <- makeCluster(no_cores)

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F5X")

f <- function(filename){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F5X")
  df <- read.csv(file=filename, stringsAsFactors = F)[,c('total_distance', 'charge_yes')]
  
  df$charge_num[1] <- 1
  for(i in 2:nrow(df)){
    if(df$charge_yes[i] == 1){
      df$charge_num[i] <- df$charge_num[i-1] + 1
    } else{
      df$charge_num[i] <- df$charge_num[i-1]
    }
  }
  
  charge <- unique(df$charge_num)
  charge <- charge[order(charge)]
  
  s <- split(df, df$charge_num)
  
  # travel distance between 2 charge opportunities
  between_distance <- sapply(s, function(x) {
    sum(x[,c("total_distance")],
        na.rm = T)
  })
  aa <- as.data.frame(between_distance)
  aa$charge_num <- charge
  
  aa <- aa[,c(2,1)]
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_Between_X")
  write.csv(aa, file = filename, row.names = F)
}

parLapply(cl, filename, f)
stopCluster(cl)



## descriptive statistics
rm(list = ls())
filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_Between_X")
total <- as.data.frame(filename)
total$Vehicle_ID <- substr(total[,1], 1, 10)

for (i in 1:length(filename)){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_Between_X")
  df <- read.csv(file=filename[i])
  
  total$between_distance[i] <- round(mean(df$between_distance), 2)
  
  if(i%%500==0) print(i)
}

total <- total[, -1]

colnames(total) <- c('Vehicle_ID', 'between_distance_X')

table <- read.csv('C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8_Review.csv')

aa <- merge(table, total, by='Vehicle_ID', all.x=TRUE)

write.csv(aa, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8_Review.csv', row.names = F)



## number of shifts that a driver took on average
rm(list = ls())

table <- read.csv('C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8.csv')

table$ave_shift_driver <- round(table$num_day * table$ave_shift / table$num_driver, 2)
table$ave_shift_driver_new <- round(table$num_day * table$ave_shift_new / table$num_driver, 2)

write.csv(table, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8.csv', row.names = F)



## time duration of each shift
rm(list=ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
total <- as.data.frame(filename)
total$Vehicle_ID <- substr(total[,1], 1, 10)
total$ave_shift_new_duration <- NA

for (i in 1:length(filename)){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")
  
  df <- read.csv(file=filename[i], stringsAsFactors = F)[,c('date', 'hack_license', 'pickup_datetime', 'dropoff_datetime')]
  
  df$pickup_datetime <- as.POSIXct(df$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  df$dropoff_datetime <- as.POSIXct(df$dropoff_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  
  df$shift[1] <- 1
  for(j in 2:nrow(df)){
    if(df$hack_license[j] != df$hack_license[j-1] | df$date[j] != df$date[j-1]){
      df$shift[j] <- df$shift[j-1] + 1
    } else{
      df$shift[j] <- df$shift[j-1]
    }
  }
  
  #shift duration
  ShiftDuration <- function(x) {
    return(x$dropoff_datetime[nrow(x)] - x$pickup_datetime[1])
  }
  df <- ddply(df, .(shift), ShiftDuration)
  
  total$ave_shift_new_duration[i] <- as.numeric(round(mean(df[, 2])/60, 0))

  if(i%%500==0) print(i)
}

total <- total[, -1]

table <- read.csv('C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8_Review.csv')

aa <- merge(table, total, by='Vehicle_ID', all.x=TRUE)

write.csv(aa, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8_Review.csv', row.names = F)


