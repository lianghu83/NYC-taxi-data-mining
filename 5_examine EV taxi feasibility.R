

###########################################################################################################
# use % of occupied trips that can be completed to infer EV taxi feasibility.
# EV 200 feasibility
# EV 300 feasibility

rm(list = ls())

filename <- dir("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6_Review")
total <- as.data.frame(filename)
total$Vehicle_ID <- substr(total[,1], 1, 10)

for (i in 1:length(filename)){
  setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F6_Review")
  df <- read.csv(file=filename[i])[,c('flag', 'flag_3')]
  
  total$Feasibility_200[i] <- round(sum(df$flag)/nrow(df), 4)
  total$Feasibility_300[i] <- round(sum(df$flag_3)/nrow(df), 4)
  
  if(i%%500==0) print(i)
}

total <- total[, -1]

colnames(total) <- c('Vehicle_ID', 'Feasibility_200_Review', 'Feasibility_300_Review')

driver <- read.csv('C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8.csv')

aa <- merge(driver, total, by='Vehicle_ID', all.x=TRUE)

write.csv(aa, file = 'C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\By vehicle_F8_Review.csv', row.names = F)


