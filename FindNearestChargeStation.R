# Function
# Connect to NYC existing/future charging station
# find nearest charging station
# return (distance, travel time, charging power)

library(sp)
library(rgeos)
library(geosphere)
source("U:\\Desktop\\NYC Taxi Code\\earth.dist.R")

FindNearestChargeStation <- function(NYC, dropoff_longitude, dropoff_latitude) {
  
  NYC_GPS <- SpatialPoints(cbind(NYC$Longitude, NYC$Latitude))
  
  min_NYC_index <- apply(gDistance(NYC_GPS, 
                                   SpatialPoints(cbind(dropoff_longitude, dropoff_latitude)), 
                                   byid=T), 1, which.min)
  
  min_distance <- round(earth.dist(dropoff_longitude, dropoff_latitude, 
                                   NYC$Longitude[min_NYC_index], NYC$Latitude[min_NYC_index]), 4)
  
  min_distance_charge_real <- round(1.4413*min_distance + 0.1383, 2) #in miles
  
  speed_average_NYC <- 13 #mph
  travel_time_to_charge <- round(min_distance_charge_real / speed_average_NYC * 3600, 0) #sec
  
  charge_level <- NYC$Charge.Level[min_NYC_index]
  
  if(charge_level==0) charge_power <- 0
  if(charge_level==1) charge_power <- 7
  if(charge_level==2) charge_power <- 20
  if(charge_level==3) charge_power <- 50
  
  return(c(min_distance_charge_real, travel_time_to_charge, charge_power))
  
}




