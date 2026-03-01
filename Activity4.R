install.packages(c("dplyr","lubridate","ggplot2"))
library(dplyr)
library(lubridate)
library(ggplot2)

weather<-read.csv("/cloud/project/activity04/campus_weather.csv", na.strings = c("#N/A","NA"))
metaDat<- read.csv("/cloud/project/activity04/meter_weather_metadata.csv")

#date can be confusing with time zones,etc. Put in UTC. We in UTC-4 EST. 
#Make sure time zones match b/w data. Present/Interpret data in local time.
#data loggers get slower unless given the right time. Esp low battery.


weather$DateF<-mdy_hm(weather$Date)
weather$DateET<-mdy_hm(weather$Date, tz="America/New_York")
weather_check<- weather%>%
  filter(is.na(weather$DateET))
weather$DateET

weather$DateF[2]%--%weather$DateF[3]
int_length(weather$DateF[2]%--%weather$DateF[3])
#default in seconds

test<-weather$DateF[1:10]
test
test[-1] #removes the number we ask

#Eg of a function
#x is a date vector 
timeCheck900<-function(x){
  intervals<-x[-length(x)] %--%x[-1]
 #[-length(x)] subtracts last
  interval_times<-int_length(intervals)
  intervals[interval_times!=900]
}

timeCheck900(weather$DateF)

#forloops- less common bec it is computationally slow
#lists allow you to store multiple data types

soil.Files<- list.files("/cloud/project/activity04/soil")
# set up variable to be used in for loop
soillist<-list()

for (i in length(soil.Files)){
  soillist[i]<-read.csv(paste0("/cloud/project/activity04/soil/", soil.Files[i]))
}
str(soillist)
soilData<-do.call("rbind",soillist)

#Prompt 1
#calc moving average
airMA<-numeric()

for (i in 8: length(weather$AirTemp)){
  airMA[i]<- mean(weather$AirTemp[(i-7):i])
}
weather$airMA<-airMA

#Prompt 2:
#Time window we want:
solartime<- weather%>%
  filter(DateF)
#couldn't finish this in class




  





