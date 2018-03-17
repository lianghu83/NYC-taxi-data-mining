######################################################################
## This file processes NYC charging station data from DOE,
## and connect charging station to taxi trip data.



rm(list = ls())

df <- read.csv("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data\\alt_fuel_stations (Dec 22 2016).csv",
               stringsAsFactors = F)


# State of New York
NY <- subset(df, df$State == 'NY')
NY$City <- as.factor(NY$City)
summary(NY$City)
library(ggplot2)
ggplot(NY, aes(x=NY$Longitude, y=NY$Latitude, label=City)) + 
  geom_point() + geom_text(aes(label=City),hjust=0, vjust=0)


# New York City
NYC <- subset(NY, Latitude >= 40.4 & Latitude <= 41.0 & Longitude >= -74.25 & Longitude <= -73.5)
# remove private station
NYC <- subset(NYC, NYC$Groups.With.Access.Code != 'Private')
#not_in_NYC <- c('Nyack', 'Blauvelt', '', 'Cortlandt', 'Katonah', 'Haverstraw', 'Ossining',
#                'Mt. Kisco', 'Mount Kisco', 'Chappaqua', 'Spring Vall')
#NYC <- subset(NYC, NYC$City %in% not_in_NYC == FALSE)
ggplot(NYC, aes(x=Longitude, y=Latitude, label=City)) + 
  geom_point() + geom_text(aes(label=City),hjust=0, vjust=0)


# Locate each charging station on Google map
library(ggmap)
# creating a sample data.frame with your lat/lon points
GPS <- as.data.frame(cbind(NYC$Longitude,NYC$Latitude))

# getting the map
map_NYC <- get_map(location = c(lon = mean(GPS$V1), lat = mean(GPS$V2)), zoom = 10,
                      maptype = "roadmap")

# plotting the map with some points on it
ggmap(map_NYC) +
  geom_point(data = NYC, aes(x = Longitude, y = Latitude, fill = "red", alpha = 0.8), size = 5, shape = 21) +
  guides(fill=FALSE, alpha=FALSE, size=FALSE)



# Add charging level
NYC$Charge.Level <- 1
NYC$Charge.Level[is.na(NYC$EV.DC.Fast.Count) == FALSE] <- 3
NYC$Charge.Level[is.na(NYC$EV.DC.Fast.Count) == TRUE & is.na(NYC$EV.Level2.EVSE.Num) == FALSE] <- 2



# Output NYC charging station file
setwd("C:\\Users\\lianghu\\Desktop\\NYC Taxi Trip Data")
write.csv(NYC, file = 'NYC_alt_fuel_stations (Dec 22 2016).csv', row.names = F)
