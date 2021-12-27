set.seed(1234567890)
# Package used to calculate distance based on coordinates
library(geosphere)
# Importing the given data
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")
# Smoothing coefficients for the three Gaussian kernels
h_distance <- 50000
h_date <- 38
h_time <- 2.7

# Point (location) to be predicted
a <- 58.40505
b <- 15.5791
# Date and time of the prediction
date <- "2011-11-15" # The date to predict (up to the students)
times <- c("04:00:00", "06:00:00", "08:00:00", "10:00:00",
           "12:00:00", "14:00:00", "16:00:00", "18:00:00",
           "20:00:00","22:00:00","24:00:00")
# Predicted temperatures
temp <- vector(length=length(times))
tempMult <- vector(length=length(times))

# Calculates Kernel values for different physical distances.
physicalX <- seq(from = 0, to = 300000, by =25000)
physicalY <- exp(-(physicalX/h_distance)^2)
kilometerX <- seq(from = 0, to = 25*12, by = 25)
plot(kilometerX,physicalY, col = "blue", pch=c("o"),
     xlab = "Distance in kilometers" , ylab = "Kernel value",
     main = "Kernel values for physical distance, h = 50000")

# Calculates Kernel values for different distances in days.
daysX <- seq(from = 0, to = 185, by =5)
daysY <- exp(-((daysX)/h_date)^2)
plot(daysX,daysY, col = "blue", pch=c("o"),
     xlab = "Distance in number of days" , ylab = "Kernel value",
     main = "Kernel values, distance in nr of days, h = 38")

hoursX <- seq(from = 0, to = 12, by =1)
hoursY <- exp(-((hoursX)/h_time)^2)
plot(hoursX, hoursY, col = "blue", pch=c("o"),
     xlab = "Distance in number of hours" , ylab = "Kernel value",
     main = "Kernel values, distance in nr of hours, h = 2.7")

# Physical distance
distanceKernel <- function(predLocation, stationLocation){
  
  return(exp(-((distHaversine(predLocation,stationLocation))/h_distance)^2))
}
# Difference in days.
dateKernel <- function(predDate, stationDate){
  # Calculates the nr of the day out of 365 (mounth*30 + day in month)
  predDay <- (as.integer(substring(predDate,6,7))-1)*30 +
    as.integer(substring(predDate,9,10))
  stationDay <- (as.integer(substring(stationDate,6,7))-1)*30 +
    as.integer(substring(stationDate,9,10))
  
  # Since we want last of december and first of january to be only one day
  # apart and not 364 we do the following. The maximum difference in days
  # between two dates are 365/2 = 182.5 if we consider going both ways.
  if(abs(predDay-stationDay) > 183){
    daysDifference <- 365 - abs(predDay - stationDay)
  }else{
    daysDifference <- abs(predDay-stationDay)
  }
  
  return(exp(-(daysDifference/h_date)^2))
}
# Difference in hour.
hourKernel <- function(predTime, stationTime){
  # Since we want 23:00 and 01:00 to be only two hours apart and not 22 hours
  # we do the following. Maximum difference in days between two times are
  # 12 hours if we consider going both ways.
  predHour <- as.integer(substring(predTime,1,2))
  stationHour <- as.integer(substring(stationTime,1,2))
  
  if(abs(predHour-stationHour) > 12){
    hourDifference <- 24 - abs(predHour - stationHour)
  }else{
    hourDifference <- abs(predHour - stationHour)
  }
  
  return(exp(-(abs(hourDifference)/h_time)^2))
}
# Sum of all three kernals
sumKernel <-function( predTime, stationTime, stationDate, stationNr,
                      predLat = a , predLong = b, predDate = date){
  # Extract the coordinates of the station at interest
  stationLat <- stations$latitude[stations[,1] == stationNr]
  stationLong <- stations$longitude[stations[,1] == stationNr]
  
  
  return(distanceKernel(c(predLat,predLong),c(stationLat,stationLong)) +
           dateKernel(predDate,stationDate) +
           hourKernel(predTime, stationTime))
}

#sumKernel(times[1], temps$time[1], temps$date[1], temps$station_number[1])


# Only observations made before the date we want to predict are considerd.
observationBeforePred <- function(predDate = date, stationDate,
                                  predTime, stationTime){
  # Ok if observation is from previous year
  if(as.integer(substring(predDate,1,4)) <
     as.integer(substring(stationDate,1,4))){
    return(TRUE)
    # Ok if same year but previous month
  }else if(as.integer(substring(predDate,1,4)) ==
           as.integer(substring(stationDate,1,4)) &&
           as.integer(substring(predDate,6,7)) <
           as.integer(substring(stationDate,6,7))
  ){
    return(TRUE)
    # Ok if same year, same month but previous month
  }else if(as.integer(substring(predDate,1,4)) ==
           as.integer(substring(stationDate,1,4)) &&
           as.integer(substring(predDate,6,7)) ==
           as.integer(substring(stationDate,6,7)) &&
           as.integer(substring(predDate,9,10)) <
           as.integer(substring(stationDate,9,10))){
    return(TRUE)
  }
  return(FALSE)
}


# Goes through all the times we want to predict.
for(i in 1: length(times)){
  denominator <- 0
  numerator <- 0
  print(time)
  # Goes through all the observations given.
  for(j in 1:dim(temps)[1]){
    # Only observations made before the date we want to predict are allowed
    if(observationBeforePred(temps$date[j],date)){
      k <- sumKernel(times[i],temps$time[j],temps$date[j],
                     temps$station_number[j])
      
      numerator = numerator + k*temps$air_temperature[j]
      denominator = denominator + k
    }
  }
  print(numerator)
  print(denominator)
  temp[i] = numerator/denominator
}
# Plot the result
xAxis <- seq(from = 4, to = 24, by = 2)
plot(xAxis, temp, type="o",col = "blue",
     xlab = "Hour of day" , ylab = "Temperature",
     main = "Temp predict sum Kernel Linköping 2011-11-15")


# Kernel based on the multiplication of the three distance kernels
multKernel <-function( predTime, stationTime, stationDate, stationNr,
                       predLat = a , predLong = b, predDate = date){
  # Extract the coordinates of the station at interest
  stationLat <- stations$latitude[stations[,1] == stationNr]
  stationLong <- stations$longitude[stations[,1] == stationNr]
  
  
  return(distanceKernel(c(predLat,predLong),c(stationLat,stationLong)) *
           dateKernel(predDate,stationDate) *
           hourKernel(predTime, stationTime))
}
# Goes through all the times we want to predict.
for(i in 1: length(times)){
  denominatorMult <- 0
  numeratorMult <- 0
  # Goes through all the observations given.
  for(j in 1:dim(temps)[1]){
    # Only observations made before the date we want to predict are allowed
    if(observationBeforePred(temps$date[j],date)){
      k <- multKernel(times[i],temps$time[j],temps$date[j],
                      temps$station_number[j])
      
      numeratorMult = numeratorMult + k*temps$air_temperature[j]
      denominatorMult = denominatorMult + k
    }
  }

  tempMult[i] = numeratorMult/denominatorMult
}
# Plot the result
xAxis <- seq(from = 4, to = 24, by = 2)
plot(xAxis, tempMult, type="o",col = "blue",
     xlab = "Hour of day", ylab = "Temperature",
     main = "Temp predict mult Kernel Linköping 2011-11-15")