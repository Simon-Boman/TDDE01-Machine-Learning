set.seed(1234567890)
library(geosphere)
stations <- read.csv("stations.csv")
temps <- read.csv("temps50k.csv")
st <- merge(stations,temps,by="station_number")

a <- 17.9513 #longitude
b <- 59.3537 #latitude
testLoc <- c(longitude = a, latitute = b)

#The date to predict
testDate <- as.Date("2000-10-23")

#Verklig tid och temperatur för ovan: 00.00  2.7grader

#delete all data after our observation
#convert our dates into Date objects
st$date = as.Date(st$date, format = "%Y-%m-%d")

#difftime - calculates a difference of two date/time objects 
#if difftime of our date and the ones in our data is smaller or equal to 0 (i.e. our data is "larger", i.e. after the other date),
#then keep it. so we make a sub set based on this condition. 
stFiltered = subset(st, difftime(testDate, st$date) >= 0);
#stFiltered = st[which(difftime(testDate, st$date) >= 0),]

#The times we want to predict temperature for
testTimes <- c("04:00:00", "06:00:00","08:00:00","10:00:00", "12:00:00","14:00:00","16:00:00", "18:00:00","20:00:00",
           "22:00:00", "24:00:00")


###########1 - Smoothing coefficients##########

#Choose an appropriate smoothing coefficient or width for each of the three kernels above:
#No cross-validation should be used. Instead, choose manually a width that gives large kernel
#values to closer points and small values to distant points.
#Show this with a plot of the kernel value as a function of distance. 

#larger h - points further away have larger impact


##################
#Kernel 1 - Physical distance from a station to the point of interes
##################
h_distance = 50000

#distHaversine - The shortest distance between two points (i.e., the 'great-circle-distance' or 'as the crow flies')
#c(longitude, langitude) format
#sweden is 1570 km long, so potential distances are 0 - 1570km i.e. 0 - 1570 000.
#take half this, so around 800 000

#gaussian-kernel-value = exp (- ||(x*-xi)/h)||^2)

distances = seq(from = 0 , to = 300000 , by = 1000)
kernelDist = exp(-(distances/h_distance)^2)
plot(distances, kernelDist, type='l')

#dist_km = seq(from = 0, to = 300, by = 1)
#plot(dist_km, kernelDist, type='l')


#using smoothing factor 50000 for this makes distances > 100 km have almost 0 impact, which is reasonable. 

##################
#Kernel 2 - Distance between days a temperature measurement was made and the day of interest.
##################
h_date <- 15

days = seq(from = 0, to = floor(365/2), by = 1)
kernelDays = exp(-(days/h_date)^2)
plot(days, kernelDays, type='l')

#using smoothing factor 15 for this makes distances > 30 days have almost 0 impact, which is reasonable. 

##################
#Kernel 3 - Distance between the hour of the day a temperature measurement was made and the hour of interest.
##################
h_time <- 3
hours = seq(from = 0, to = 12, by = 1)
kernelHours = exp(-(hours/h_time)^2)
plot(hours, kernelHours, type='l')

#using smoothing factor 3 for this makes distances > 5 hours have almost 0 impact, which is reasonable. 



##################
#combine the three kernels (by summing them) to make predicitons
#################

#functions to calculate kernel-value for distance, days, hour between our observation and another observation
#and one to sum the three kernel values together using above 3. 


#distHaversine - 2 points of long/lat
KernelDistance <- function (testLoc, stationLoc) {
  kernelvalue = exp(-(distHaversine(testLoc, stationLoc)/h_distance)^2)
  return (kernelvalue)
}



KernelDays <- function(testDate, stationDate) {
  #calculate the difference in days between the 2 dates. 
  #Since we filtered away dates after our testDate, this difference will always be >=. 
  #daysapart = as.numeric(difftime(testDate, stationDate))
  #daysapart == daysapart%%365.25
  
  year = substring(testDate,1,4)
  monthday = substring(stationDate,5,11)
  if (monthday == "-02-29") {
    monthday = "-02-28"
  }
  combined = as.Date(paste(year,monthday,sep=""))
  daysdist = as.numeric(difftime(testDate, combined))
  daysdist = abs(daysdist)
  if (daysdist > floor(365/2)) {
    daysdist = floor(365 - daysdist)
  }
 
  #denna tar hänsyn till skottår, får ej error om tex 02-29
  #print(daysdist)
  #daysdist = as.numeric(difftime(testDate, stationDate))
  #daysdist = daysdist%%365.25
  #print(daysdist)
  #if (daysdist > floor(365.25/2)) {
  #  daysdist = round(365.25 - daysdist)
 # }
  #print(daysdist)
  
  kernelvalue = exp(-(daysdist/h_date)^2)
  return (kernelvalue)
}


KernelTime <- function(testTime, stationTime) {
  testTime = strptime(testTime, format="%H:%M:%S")
  stationTime = strptime(stationTime, format="%H:%M:%S")
  timediff = difftime(testTime, stationTime)
  timediff = abs(as.numeric(timediff))
  if (timediff > 12) {
    timediff = 24 - timediff
  }
  #print(timediff)
  #allt köra substring och jämfora as numeric 

  kernelvalue = exp(-(timediff/h_time)^2)
  #print(kernelvalue)
  return (kernelvalue)
}


#Combined kernel using the sum of the 3 above kernels, 
#giving us kernelvalue of 2 different locations, 2 different dates, 2 different times
#all the points in st will contribute, but with different weight (kernelvalue)
#see equation on slides. sum of all kernels temp * kernelvalue /sum of all kernel values
KernelSum <- function (testLoc, testDate, testTime, stationLoc, stationDate, stationTime) {
  kernelvalue = KernelDistance(testLoc, stationLoc) + KernelDays(testDate, stationDate) + KernelTime(testTime, stationTime)
  return (kernelvalue)
}

KernelSum(testLoc, testDate, testTimes[1], stationCords, stationDate, stationTime)

tempSumKernel <- vector(length=length(testTimes))

for (time in 1:length(testTimes)) {
  print(time)

  sumkernelvals <- vector(length=nrow(stFiltered))
  sumkernelvals_temps <- vector(length=nrow(stFiltered))
  
  for (obs in 1:nrow(stFiltered)) {
    sumkernelvals[obs] = KernelSum(testLoc, testDate, testTimes[time], c(stFiltered$longitude[obs],stFiltered$latitude[obs]),
                                  as.Date(stFiltered$date[obs]), stFiltered$time[obs])
    sumkernelvals_temps[obs] = sumkernelvals[obs] * stFiltered$air_temperature[obs]
  }
  
  
  temp = sum(sumkernelvals_temps)/sum(sumkernelvals)
  tempSumKernel[time] = temp
}

plot(seq(from=4, to=24, by=2), tempSumKernel, type = "o")



####KERNEL MULT
KernelMult <- function (testLoc, testDate, testTime, stationLoc, stationDate, stationTime) {
  kernelvalue = KernelDistance(testLoc, stationLoc) * KernelDays(testDate, stationDate) * KernelTime(testTime, stationTime)
  return (kernelvalue)
}

tempMultKernel <- vector(length=length(testTimes))

for (time in 1:length(testTimes)) {
  print(time)
  
  multkernelvals <- vector(length=nrow(stFiltered))
  multkernelvals_temps <- vector(length=nrow(stFiltered))

  for (obs in 1:nrow(stFiltered)) {
    multkernelvals[obs] = KernelMult(testLoc, testDate, testTimes[time], c(stFiltered$longitude[obs],stFiltered$latitude[obs]),
                                   as.Date(stFiltered$date[obs]), stFiltered$time[obs])
    multkernelvals_temps[obs] = multkernelvals[obs] * stFiltered$air_temperature[obs]
  }
  temp = sum(multkernelvals_temps)/sum(multkernelvals)
  tempMultKernel[time] = temp

}

plot(seq(from=4, to=24, by=2), tempMultKernel, type="o")

plot(tempMultKernel, type="o")