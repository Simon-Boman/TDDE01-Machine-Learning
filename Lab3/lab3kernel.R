set.seed(1234567890)
library(geosphere)
stations = read.csv("stations.csv")
temps = read.csv("temps50k.csv")
st = merge(stations,temps,by="station_number")

a = 17.9513 #longitude
b = 59.3537 #latitude
testLoc = c(longitude = a, latitute = b)

#The date to predict
testDate = as.Date("2000-10-23")

#Real time and temperature for above coordinates and date:00.00, 2.7 degrees

#delete all data after our observation
#convert our dates into Date objects
st$date = as.Date(st$date, format = "%Y-%m-%d")

#difftime - calculates a difference of two date/time objects 
#To delete data after our observation: if difftime of our date and the ones in our data is smaller or equal to 0 (
#i.e. our data is "larger", i.e. after the other date), then we keep that observation.
stFiltered = subset(st, difftime(testDate, st$date) >= 0);
#stFiltered = st[which(difftime(testDate, st$date) >= 0),]

#The times we want to predict temperature for
testTimes = c("04:00:00", "06:00:00","08:00:00","10:00:00", "12:00:00","14:00:00","16:00:00", "18:00:00","20:00:00",
              "22:00:00", "24:00:00")



########### - Selecting smoothing coefficients##########
#The larger the h, the more impact observations far away from our observation have
#Gaussian kernel:  exp(-(||(x*-xi)||/h)^2), which is the same as exp(-||x*-x'||^2/(2l^2)),
#where a certain h would correspond to a certain l. 


#Kernel 1 - Physical distance from a station to the point of interest
h_distance = 50000 #using smoothing factor 50000 makes distances > 100 km have almost 0 impact

distances = seq(from = 0 , to = 300000 , by = 1000)
kernelDist = exp(-(distances/h_distance)^2)
plot(distances, kernelDist, type='l', xlab="distance in km", ylab="kernel-value")


#Kernel 2 - Distance between days a temperature measurement was made and the day of interest.
h_date = 15 #using smoothing factor 15 makes day difference > 30 days have almost 0 impact

days = seq(from = 0, to = floor(365/2), by = 1)
kernelDays = exp(-(days/h_date)^2)

plot(days, kernelDays, type='l', xlab="difference in days", ylab="kernel-value")


#Kernel 3 - Distance between the hour of the day a temperature measurement was made and the hour of interest.
h_time = 3 #using smoothing factor 3 makes time difference > 5 hours have almost 0 impact

hours = seq(from = 0, to = 12, by = 1)
kernelHours = exp(-(hours/h_time)^2)

plot(hours, kernelHours, type='l', xlab="difference in hours", ylab="kernel-value")



#########Functions for the three kernels##########

#function to calculate kernel-value for distance between our observation and another observation
KernelDistance = function (testLoc, stationLoc) {
  kernelvalue = exp(-(distHaversine(testLoc, stationLoc)/h_distance)^2)
  return (kernelvalue)
  #distHaversine - The shortest distance between two points (i.e., the 'great-circle-distance' or 'as the crow flies')
  #takes input as: c(longitude, langitude)
}

#function to calculate kernel-value for days between our observation and another observation
KernelDays = function(testDate, stationDate) {
  #extract year
  year = substring(testDate,1,4)
  #extract month + date
  monthday = substring(stationDate,5,11)
  #does not take into factor skottår, if 29 feb set date to 28 feb
  if (monthday == "-02-29") {
    monthday = "-02-28"
  }
  #recombine the year and month+day
  combined = as.Date(paste(year,monthday,sep=""))
  #use difftime again to calculate difference between the two dates. 
  daysdist = as.numeric(difftime(testDate, combined))
  daysdist = abs(daysdist)
  #since the maximum difference between two dates should be 365/2 and not 365
  #thus if distance > 365/2, subtract this distance from 365. 
  if (daysdist > floor(365/2)) {
    daysdist = floor(365 - daysdist)
  }

  kernelvalue = exp(-(daysdist/h_date)^2)
  return (kernelvalue)
}

#function to calculate kernel-value for hours between our observation and another observation
KernelTime = function(testTime, stationTime) {
  testTime = strptime(testTime, format="%H:%M:%S")
  stationTime = strptime(stationTime, format="%H:%M:%S")
  timediff = difftime(testTime, stationTime)
  timediff = abs(as.numeric(timediff))
  if (timediff > 12) {
    timediff = 24 - timediff
  }
  
  kernelvalue = exp(-(timediff/h_time)^2)
  return (kernelvalue)
}



#########Creating new kernels using the previous three and making predictions######

#Creating new kernel by summing the three above kernels together 
KernelSum = function (testLoc, testDate, testTime, stationLoc, stationDate, stationTime) {
  kernelvalue = KernelDistance(testLoc, stationLoc) + KernelDays(testDate, stationDate) + KernelTime(testTime, stationTime)
  return (kernelvalue)
}


#vector in which to store the predicted tempereatures 
temps_SumKernel = vector(length=length(testTimes))

#for each time in our testTimes, we want to make a prediction by looking at all data points in st
#since they will all contribute but with different weights (kernel-values).
#I.e.: sum of all temps * kernel-value, divided by the sum of all kernel values
for (time in 1:length(testTimes)) {
  print(time)
  
  sumkernelvals = vector(length=nrow(stFiltered))
  sumkernelvals_temps = vector(length=nrow(stFiltered))
  
  for (obs in 1:nrow(stFiltered)) {
    sumkernelvals[obs] = KernelSum(testLoc, testDate, testTimes[time], c(stFiltered$longitude[obs],stFiltered$latitude[obs]),
                                   as.Date(stFiltered$date[obs]), stFiltered$time[obs])
    sumkernelvals_temps[obs] = sumkernelvals[obs] * stFiltered$air_temperature[obs]
  }
  
  temp = sum(sumkernelvals_temps)/sum(sumkernelvals)
  temps_SumKernel[time] = temp
}

plot(seq(from=4, to=24, by=2), temps_SumKernel, type = "o", xlab = "hour of the day", ylab="predicted temperature", main="temperature prediction using sum kernel")


#Repeating above steps but for multiplication 
#Creating new kernel by multiplicating the three above kernels together 
KernelMult = function (testLoc, testDate, testTime, stationLoc, stationDate, stationTime) {
  kernelvalue = KernelDistance(testLoc, stationLoc) * KernelDays(testDate, stationDate) * KernelTime(testTime, stationTime)
  return (kernelvalue)
}


temps_MultKernel = vector(length=length(testTimes))

for (time in 1:length(testTimes)) {
  print(time)
  
  multkernelvals = vector(length=nrow(stFiltered))
  multkernelvals_temps = vector(length=nrow(stFiltered))
  
  for (obs in 1:nrow(stFiltered)) {
    multkernelvals[obs] = KernelMult(testLoc, testDate, testTimes[time], c(stFiltered$longitude[obs],stFiltered$latitude[obs]),
                                     as.Date(stFiltered$date[obs]), stFiltered$time[obs])
    multkernelvals_temps[obs] = multkernelvals[obs] * stFiltered$air_temperature[obs]
  }
  temp = sum(multkernelvals_temps)/sum(multkernelvals)
  temps_MultKernel[time] = temp
  
}

plot(seq(from=4, to=24, by=2), temps_MultKernel, type="o", xlab = "hour of the day", ylab="predicted temperature", main="temperature prediction using multiplication kernel")