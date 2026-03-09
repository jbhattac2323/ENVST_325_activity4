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
weather$doy <- yday(weather$DateF)
weather$year <- year(weather$DateF)
weather$month <- month(weather$DateF)

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
#solartime<- weather%>%
#  filter(DateF)
#couldn't finish this in class


##### Homework-----
#Q1: weather station data manager - precipitation data with the village of Clinton
#Ensure no issues w bird excrement or frozen precipitation
#Exclude precipitation if air temp below 0
#Exclude if X and Y level observations for more than 2 degrees

# add a column to weather:
weather$precip.QC <- ifelse(weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021, 
                            # evaluate if the doy is between May 1 and July 7 2021
                            NA, # value if true
                            weather$Precip) # value if false: uses original precipitation observation

weatherreport<-weather%>%
  filter(XLevel<2)%>%
  filter(YLevel<2)%>%
  filter(AirTemp>=0)


# Cld monitor pH levels for understanding bird excrement presence

#Q2: Flag battery below 8.5V


weather$BatFlag<- ifelse(weather$BatVolt <= 8500, # check if at or below zero
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero


#Q3: 



#Q5: Format data to each day

#filter 2021 month 3 and 4, 1.6 C

totalprecMarchApril<- weather%>%
  filter(year==2021)%>%
  filter(month==4 | month==3)%>%
  group_by(doy)%>%
  summarize(totprecip=sum(Precip),mintemp=min(AirTemp))

precipQC<-as.numeric(NA)

for (i in 2: nrow(totalprecMarchApril)){ 
  precipQC[i]<-ifelse(totalprecMarchApril$mintemp[i]<=1.6 | totalprecMarchApril$mintemp[i-1]<=1.6,NA, 
                      totalprecMarchApril$totprecip[i])
} 

totalprecMarchApril$precipqc<-precipQC







