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

for (i in 1: length(soil.Files)){
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

#addressing bird excrement by assigning NA to the bad dates

weather$precip.QC <- ifelse(
  weather$doy >= 121 & weather$doy <= 188 & weather$year == 2021 |
    abs(weather$XLevel) > 2 |
    abs(weather$YLevel) > 2 |
    weather$AirTemp < 0,
  NA,
  weather$Precip
)

sum(is.na(weather$precip.QC))

#Q2: Flag battery below 8.5V
weather$BatFlag<- ifelse(weather$BatVolt < 8500, # check if Battery is below 8.5V or 8500mV (the units of the data).
                             1, # if true: set flag to 1
                             0) # if false: set flag to zero


#Q3:Check unrealistic data ranges. 
#Possible Ranges: Radiation: 0 to 1750 W/m^2. Air temp: -50 to 60 C
outlierremoval <- function(x, min, max) {
  clean <- ifelse(x < min | x > max, NA, x)
}
weather$SolRad.QC <- outlierremoval(weather$SolRad, 0, 1750) #full range of sensor since don't have intuition about solar radiation values

weather$AirTemp.QC <- outlierremoval(weather$AirTemp, -35, 48) #realistic min and max temps

#Q4: plot of winter air temperatures in Jan - Mar of 2021
winter2021 <- weather %>%
  filter(year==2021 & month<=3)

ggplot(winter2021)+
  aes(x=DateET, y=AirTemp)+
  geom_line()+
  labs(title = "Winter Air Temperatures in 2021",
       x = "Winter 2021",
       y = "Air Temperature (in °C)")+
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5)) #center


#Q5:
totalprecMarchApril<- weather%>%
  filter(year==2021)%>%
  filter(month==4 | month==3)%>%
  group_by(doy)%>%
  summarize(totprecip=sum(Precip),mintemp=min(AirTemp))

precipQC<-as.numeric(NA)

for (i in 2: nrow(totalprecMarchApril)){ 
  precipQC[i]<-ifelse(totalprecMarchApril$mintemp[i]<=1.6 | totalprecMarchApril$mintemp[i-1]<=1.6,NA, 
                      totalprecMarchApril$totprecip[i])
} #35 F corresponds to approx. 1.6 C.
totalprecMarchApril$precipqc<-precipQC

#How many non NA values?
nonNA<-nrow(totalprecMarchApril)-sum(is.na(totalprecMarchApril$precipqc))
nonNA

#Q6 

timeCheck<-function(x,seconds){ #updated to allow time input (seconds)
  intervals<-x[-length(x)] %--%x[-1]
  interval_times<-int_length(intervals)
  intervals[interval_times!=seconds]
}

for (i in 1:nrow(soilData)){
  test <- ymd_hm(c(soilData[i,]))
  print(timeCheck(test,3600))
}



