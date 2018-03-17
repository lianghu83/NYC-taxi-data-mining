####################################################################
## This file calculates descriptive statistics for electric taxis.

# number of occupied trips per day
# occupied trip length
# occupied trip time
# total # occupied trips in 2013 for all taxis
# number of days with records in 2013

rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F4")

setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F4")

trip_distance_all <- read.csv("2013000001.csv")[,c("trip_distance")]
trip_time_all <- read.csv("2013000001.csv")[,c("trip_time_in_secs")]
date_all <- read.csv("2013000001.csv", stringsAsFactors = F)[,c("date")]
num_day_all <- c(length(unique(date_all)))
num_trip_all <- c(length(trip_distance_all))

for (i in 2:length(filename)){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F4")

  trip_distance <- read.csv(file = filename[i])[,c("trip_distance")]
  trip_time <- read.csv(file = filename[i])[,c("trip_time_in_secs")]
  date <- read.csv(file = filename[i], stringsAsFactors = F)[,c("date")]
  num_day <- c(length(unique(date)))
  num_trip <- c(length(trip_distance))
  
  trip_distance_all <- c(trip_distance_all, trip_distance)
  trip_time_all <- c(trip_time_all, trip_time)
  num_day_all <- c(num_day_all, num_day)
  num_trip_all <- c(num_trip_all, num_trip)
  
  if(i%%500==0) print(i)
}

sum(num_trip_all)
sum(trip_distance_all)
summary(trip_distance_all)
sd(trip_distance_all)
summary(trip_time_all)
sd(trip_time_all)
summary(num_day_all)
summary(num_trip_all)

#write csv
setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data")
write(trip_distance_all, file = 'trip_distance_all.csv')
write(trip_time_all, file = 'trip_time_all.csv')
write(num_day_all, file = 'num_day_all.csv')
write(num_trip, file = 'num_trip.csv')



# DVMT
# number of occupied trips per day
# number of dwells per day

rm(list = ls())

library(parallel)
no_cores <- 7
cl <- makeCluster(no_cores)

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F4")

f <- function(filename){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F4")
  df <- read.csv(file=filename, stringsAsFactors = F)[,c('date', 'total_distance', 'dwell_yes')]
  
  day <- unique(df$date)
  day <- day[order(day)]
  
  s <- split(df, df$date)
  
  total_distance <- sapply(s, function(x) {
    sum(x[,c("total_distance")],
        na.rm = T)
  })
  aa <- as.data.frame(total_distance)
  aa$date <- day
  
  dwell_yes <- sapply(s, function(x) {
    sum(x[,c("dwell_yes")],
        na.rm = T)
  })
  aa <- cbind(aa, as.data.frame(dwell_yes))

  library(plyr)
  freq <- count(df$date)
  aa$daily_trip <- freq[,2]
  
  aa <- aa[,c(2,4,1,3)]
  
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_Aggregate by day")
  write.table(aa, file = filename, row.names = F, col.names = F, sep = ',')
}

parLapply(cl, filename, f)
stopCluster(cl)

# use Merge.bat to merge all csv
# use Merge.csv to get statistics
rm(list = ls())
df <- read.csv("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_Aggregate by day\\Merge.csv",
               header = F)
head(df)
summary(df[,2])
summary(df[,3])
summary(df[,4])
apply(df[,c(2:4)], 2, sd)



# empty trip summary
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F4")

setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F4")

### merge multiple csv into one large csv
f <- function(file) {
  read.csv(file, header=T)[, c(18:20)]
}
total <- do.call("rbind", lapply(filename, f))
summary(total)
sd(total$empty_speed)
write.csv(total, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\empty trip summary.csv', row.names = F)



# emergency charging, charge to 80%, what is the time till next trip?
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")

setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6")

df <- read.csv(filename[1], stringsAsFactors = F)[, c('pickup_datetime', 'flag', 'flag_3')]
df$pickup_datetime <- as.POSIXct(df$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
temp <- as.data.frame(cbind(diff(df$pickup_datetime), diff(df$flag), diff(df$flag_3)))
total_200 <- round(temp$V1[temp$V2==1]/60, 0)
total_300 <- round(temp$V1[temp$V3==1]/60, 0)

for (i in 2:length(filename)) {
  df <- read.csv(filename[i], stringsAsFactors = F)[, c('pickup_datetime', 'flag', 'flag_3')]
  df$pickup_datetime <- as.POSIXct(df$pickup_datetime, format="%Y-%m-%d %H:%M:%S", tz="America/New_York")
  temp <- as.data.frame(cbind(diff(df$pickup_datetime), diff(df$flag), diff(df$flag_3)))
  
  range_200 <- round(temp$V1[temp$V2==1]/60, 0)
  range_300 <- round(temp$V1[temp$V3==1]/60, 0)
  
  total_200 <- append(total_200, range_200)
  total_300 <- append(total_300, range_300)
  
  if (i%%500==0) print(i)
}

write.csv(total_200, file='C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\80percent_200.csv',
          row.names = F)
write.csv(total_300, file='C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\80percent_300.csv',
          row.names = F)

summary(total_200)
summary(total_300)
boxplot(total_200)
quantile(total_200, probs=c(0.75, 0.8, 0.9))



# average speed in NYC
rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F4")

setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_1500_F4")

### merge multiple csv into one large csv
f <- function(file) {
  read.csv(file, header=T)[, c(16:17)]
}
total <- do.call("rbind", lapply(filename, f))
total <- subset(total, total$speed<100)
summary(total)
#mean=13mph
