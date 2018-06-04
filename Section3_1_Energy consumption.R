####load packages####
install.packages("dplyr")
install.packages("tidyr")
install.packages(c("tidyr", "devtools"))
install.packages("stats")
install.packages("openair")
install.packages("reshape2")
library(lubridate)
library(dplyr)
library(tidyr)
library(stats)
library(openair)
library(ggplot2)
library(reshape2)
####Import Dataset####
House_Power <- read.csv("C:/Users/alexw/Desktop/Ubiqum/Section3/Task1/household_power_consumption.txt",
                                        sep=";", na.strings="\"?\"",
                                        stringsAsFactors=FALSE)
View(House_Power)
summary(House_Power)
####Organise data####
House_Power<-na.omit(House_Power)
House_Power$Sub_metering_2<- as.numeric(House_Power$Sub_metering_2)
House_Power$Sub_metering_1<- as.numeric(House_Power$Sub_metering_1)
House_Power$Global_intensity<- as.numeric(House_Power$Global_intensity)
House_Power$Voltage<- as.numeric(House_Power$Voltage)
House_Power$Global_reactive_power<- as.numeric(House_Power$Global_reactive_power)
House_Power$Global_active_power<- as.numeric(House_Power$Global_active_power)
str(House_Power)
House_Power$DateTime<- as.POSIXct(House_Power$DateTime)
summarise(House_Power)
#
House_Power <-cbind(House_Power,paste(House_Power$Date,House_Power$Time), stringsAsFactors=FALSE)
colnames(House_Power)[10] <-"DateTime"
House_Power <- House_Power[,c(ncol(House_Power), 1:(ncol(House_Power)-1))]
head(House_Power)
#
House_Power$DateTime <- strptime(House_Power$DateTime, "%d/%m/%Y %H:%M:%S")
House_Power$Date <- as.Date(House_Power$Date, "%d/%m/%Y")
str(House_Power)
length(House_Power$DateTime)
nrow(House_Power)
#
summary(House_Power)
sd(House_Power$Global_active_power)
sd(House_Power$Sub_metering_1)
StDev<-apply(House_Power,2,sd)
StDev
vignette("dplyr")
####convert relev. to KWh####
House_Power$Global_active_power_KWh<-House_Power$Global_active_power /60
House_Power$Global_reactive_power_KWh<-House_Power$Global_reactive_power /60
#
House_Power$Sub_metering_1_KWh<-House_Power$Sub_metering_1 /1000
House_Power$Sub_metering_2_KWh<-House_Power$Sub_metering_2 /1000
House_Power$Sub_metering_3_KWh<-House_Power$Sub_metering_3 /1000
#
House_Power$Rem_EnCons_KWh<-House_Power$Global_active_power_KWh-House_Power$Sub_metering_1_KWh-House_Power$Sub_metering_2_KWh-House_Power$Sub_metering_3_KWh
House_Power$Weekday<-weekdays(House_Power$DateTime)
House_Power$Month<-month(House_Power$DateTime,label = TRUE)
#
Df2006<- House_Power %>%
  filter(year(DateTime)==2006) %>%
#
####Analyse data over time####
House_Power$Hour <- cut(as.POSIXct(paste(House_Power$date, House_Power$time),
                              format="%d/%m/%Y %H:%M:%S"), breaks="hour")
#
lubridate:: month(House_Power$DateTime, label=TRUE)
lubridate:: hour(House_Power$DateTime)
lubridate:: day(House_Power$DateTime)
hour(House_Power$DateTime[1:5])
day(House_Power$DateTime[10000:10005])
weekdays(House_Power$DateTime[1:5])
head(House_Power)
####Overall_Plot####
Global_avr_cons_plot<-plot(House_Power_monthly$Month,House_Power_monthly$Global_active_power_KWh,type = "l",xlab = "Time",ylab = "GlobalAvr")
Global_avr_cons_plot
#
GlobalActivePower_alltime<-ggplot(data=House_Power, aes(x=DateTime,y=Global_active_power_KWh)) +
  geom_line(color="yellow",size=2)+
  geom_point(size=2)
GlobalActivePower_alltime
####Create new Data frames####
#Default
#Sum <- aggregate(Total_Solar_Gesamt["TotalSolar_MW"], 
#                 list(hour=cut(as.POSIXct(Total_Solar_Gesamt$Timedate), "hour")),
#                 sum)                           sum)
#Create hourly df
House_Power_hourly<-aggregate(House_Power[11:16],
                              list(hour=cut(as.POSIXct(House_Power$DateTime),"hour")),
                              sum)
House_Power_hourly$hour<-as.POSIXct(House_Power_hourly$hour)
colnames(House_Power_hourly)[1]<-"Hour"
#Create daily df
House_Power_daily<-aggregate(House_Power[11:16],
                              list(day=cut(as.POSIXct(House_Power$DateTime),"day")),
                              sum, labels= TRUE)
House_Power_daily$day<-as.POSIXct(House_Power_daily$day)
colnames(House_Power_daily)[1]<-"Day"
House_Power_daily$Weekday<-weekdays(House_Power_daily$Day)
#Create monthly df
House_Power_monthly<-aggregate(House_Power[11:16],
                             list(month=cut(as.POSIXct(House_Power$DateTime),"month")),
                             sum)
House_Power_monthly$month<-as.POSIXct(House_Power_monthly$month)
colnames(House_Power_monthly)[1]<-"Month"
House_Power_monthly$Month_of_year<-month(House_Power_monthly$Month, label = TRUE)
####Filter by hour####
Filtered_results_hour<- House_Power_hourly %>% 
  group_by(hour(House_Power_hourly$Hour)) %>% 
  summarise(GlobalAvr = mean(Global_active_power_KWh),
            Sub1 = mean(Sub_metering_1_KWh),
            Sub2 = mean(Sub_metering_2_KWh),
            Sub3 = mean(Sub_metering_3_KWh),
            Rem = mean(Rem_EnCons_KWh))
head(Filtered_results_hour)
colnames(Filtered_results_hour)[1]<-"Hour"
#Filtered_results<-Filtered_results[-c(nrow(Filtered_results_hour)),]
#
str(Filtered_results_hour)
View(Filtered_results_hour)
summary(Filtered_results_hour)
#
Comparative_plot_hour<-ggplot(data=Filtered_results_hour, aes(x=Hour,y=value, color=variable)) +
  geom_line(aes(y = Rem, col = "Remainder"))+ 
  geom_line(aes(y = Sub1, col = "Kitchen"))+
  geom_line(aes(y = Sub2, col = "Laundry"))+
  geom_line(aes(y = Sub3, col = "Water heater"))+
  ylab("KWh")
Comparative_plot_hour
#
####Filter by Weekday####
Filtered_results_Wkd<- House_Power_daily %>% 
  group_by(Weekday) %>% 
  summarise(GlobalAvr = mean(Global_active_power_KWh),
            Sub1 = mean(Sub_metering_1_KWh),
            Sub2 = mean(Sub_metering_2_KWh),
            Sub3 = mean(Sub_metering_3_KWh),
            Rem = mean(Rem_EnCons_KWh))
head(Filtered_results_Wkd)
#Filtered_results_Wkd<-Filtered_results_Wkd[-c(nrow(Filtered_results_Wkd)),]
Filtered_results_Wkd$Weekday<-as.factor(Filtered_results_Wkd$Weekday)
Filtered_results_Wkd$Weekday<-ordered(Filtered_results_Wkd$Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
str(Filtered_results_Wkd)
Filtered_results_Wkd<-ord
View(Filtered_results_Wkd)
#
Comparative_plot_Weekday<-ggplot(data=Filtered_results_Wkd, aes(x=Weekday, group = 1), color=variable) +
  geom_line(aes(y = Rem, col = "Remainder"))+ 
  geom_line(aes(y = Sub1, col = "Kitchen"))+
  geom_line(aes(y = Sub2, col = "Laundry"))+
  geom_line(aes(y = Sub3, col = "Water heater"))+
  ylab("KWh")#+
#scale_x_datetime("",format)#+
#scale_x_discrete(breaks=c("2.5", "5.0", "7.5","10.0","12.5"),
#labels=c("Feb", "May", "Jul","Oct","Dec"))
Comparative_plot_Weekday
#
####Filter by month####

month.abb[House_Power$Date[2]]
month.abb[2]
####Golden Rule####
lubridate:: month(House_Power$DateTime, label=TRUE)
####End Golden Rule####
####+++####
head(month(House_Power$DateTime))

House_Power$Month <- month(House_Power$DateTime, label = TRUE)

Filtered_results_month<- House_Power_monthly %>% 
  group_by(Month_of_year) %>% 
  summarise(GlobalAvr = mean(Global_active_power_KWh),
            Sub1 = mean(Sub_metering_1_KWh),
            Sub2 = mean(Sub_metering_2_KWh),
            Sub3 = mean(Sub_metering_3_KWh),
            Rem = mean(Rem_EnCons_KWh))
#colnames(Filtered_results_month)[1]<-"Month"
#Filtered_results_month<-Filtered_results_month[-c(nrow(Filtered_results_month)),]
View(Filtered_results_month)
str(Filtered_results_month)
head(Filtered_results_month)
#
#month(as.Date(Filtered_results_month$Month))
Filtered_results_month$Mon<- month.abb[Filtered_results_month$Month]
Filtered_results_month$Mon<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
Filtered_results_month$Month<-as.Date(Filtered_results_month$Month,origin = "January")
#
Comparative_plot_month<-ggplot(data=Filtered_results_month, aes(x=Month_of_year, group = 1), color=variable) +
  geom_line(aes(y = Rem, col = "Remainder"))+ 
  geom_line(aes(y = Sub1, col = "Kitchen"))+
  geom_line(aes(y = Sub2, col = "Laundry"))+
  geom_line(aes(y = Sub3, col = "Water heater"))+
  ylab("KWh")#+
#scale_x_datetime("",format)#+
#scale_x_discrete(breaks=c("2.5", "5.0", "7.5","10.0","12.5"),
#labels=c("Feb", "May", "Jul","Oct","Dec"))
Comparative_plot_month
#
Alltime_months<-ggplot(data=House_Power_monthly, aes(x=Month, group = 1), color=variable) +
  geom_line(aes(y = Rem_EnCons_KWh, col = "Remainder"))+ 
  geom_line(aes(y = Sub_metering_1_KWh, col = "Kitchen"))+
  geom_line(aes(y = Sub_metering_2_KWh, col = "Laundry"))+
  geom_line(aes(y = Sub_metering_3_KWh, col = "Water heater"))+
  ylab("KWh")
Alltime_months
####StDev hour####
Filtered_SD_hour<- House_Power %>% 
  group_by(hour(DateTime)) %>% 
  summarise(GlobalAvrgSD = sd(Global_active_power_KWh),
            Sub1_SD = sd(Sub_metering_1_KWh),
            Sub2_SD = sd(Sub_metering_2_KWh),
            Sub3_SD = sd(Sub_metering_3_KWh),
            Rem= sd(Rem_EnCons_KWh))
#
colnames(Filtered_SD_hour)[1]<-"Hour"
Filtered_SD_hour<-Filtered_SD_hour[-c(nrow(Filtered_SD_hour)),]
View(Filtered_SD_hour)
#
Comp_SD_hour<-ggplot(data=Filtered_SD_hour, aes(x=Hour,group = 1, color=variable)) +
  geom_line(aes(y = Rem, col = "Remainder"))+ 
  geom_line(aes(y = Sub1_SD, col = "Kitchen"))+
  geom_line(aes(y = Sub2_SD, col = "Laundry"))+
  geom_line(aes(y = Sub3_SD, col = "Water heater"))+
  ylab("KWh")
Comp_SD_hour
#___________________________________________________________________________________________________________
Filtered_AVR_SD_hour<- House_Power %>% 
  group_by(hour(DateTime)) %>% 
  summarise(GlobalPowerUsg_SD = sd(Global_active_power_KWh),
            GlobalPowerUsg_Avg= mean(Global_active_power_KWh))
#
View(Filtered_AVR_SD_hour)
#______Defining Avrg + and Avrg -
Filtered_AVR_SD_hour$AVG_Plus_SD<-Filtered_AVR_SD_hour$GlobalPowerUsg_Avg+Filtered_AVR_SD_hour$GlobalPowerUsg_SD
Filtered_AVR_SD_hour$AVG_Minus_SD<-Filtered_AVR_SD_hour$GlobalPowerUsg_Avg-Filtered_AVR_SD_hour$GlobalPowerUsg_SD
colnames(Filtered_AVR_SD_hour)[1]<-"Hour"
#Plot
Comp_AVR_SD_hour<-ggplot(data=Filtered_AVR_SD_hour, aes(x=Hour,group = 1, color=variable)) +
    geom_line(aes(y = GlobalPowerUsg_Avg, col = "GlobalPowerUsg_Avg"))+
    geom_line(aes(y = AVG_Plus_SD, col = "Avr+1_SD"))+
    geom_line(aes(y = AVG_Minus_SD, col = "Avr-1_SD"))+ 
  ylab("KWh")
Comp_AVR_SD_hour
####PLOTS####
GlobalActivePower_Plot<-ggplot(data=Filtered_results_hour, aes(x=Hour,y=GlobalAvr, group=1)) +
  geom_line(color="yellow",size=2)+
  geom_point(size=2)
GlobalActivePower_Plot
#
Sub1_Plot<-ggplot(data=Filtered_results_hour, aes(x=Hour,y=Sub1, group=1)) +
   geom_line(color="purple", size=2)+
   geom_point(size=2)
Sub1_Plot
#
Sub2_Plot<-ggplot(data=Filtered_results_hour, aes(x=Hour,y=Sub2, group=1)) +
  geom_line(color="blue", size=2)+
  geom_point(size=2)
Sub2_Plot
#
Sub3_Plot<-ggplot(data=Filtered_results_hour, aes(x=Hour,y=Sub3, group=1)) +
  geom_line(color="green", size=2)+
  geom_point(size=2)
Sub3_Plot
#
GlobalActivePower_Plot_month<-ggplot(data=Filtered_results_month, aes(x=Month,y=GlobalAvr, group=1)) +
  geom_line(color="yellow", size=2)+
  geom_point(size=2)
GlobalActivePower_Plot_month
#
Sub1_Plot_month<-ggplot(data=Filtered_results_month, aes(x=Month,y=Sub1, group=1)) +
  geom_line(color="purple", size=2)+
  geom_point(size=2)
Sub1_Plot_month
#
Sub2_Plot_month<-ggplot(data=Filtered_results_month, aes(x=Month,y=Sub2, group=1)) +
  geom_line(color="blue", size=2)+
  geom_point(size=2)
Sub2_Plot_month
#
Sub3_Plot_month<-ggplot(data=Filtered_results_month, aes(x=Month,y=Sub3, group=1)) +
  geom_line(color="green", size=2)+
  geom_point(size=2)
Sub3_Plot_month
####yyyyyy####
Filtered_hours<- House_Power %>%
  filter(Time >="17:00:00" & Time <= "18:00:00")%>%
  select(Time, Global_active_power)
#
Filtered_hours$Average_hour <- mean(Filtered_hours$Global_active_power)
#
nrow(House_Power)
nrow(Filtered_hours)
View(Filtered_hours)
#
#if(Filtered_hours%time = "17:xx:xx") 
    #{
    #Filtered_hours$Average_hour <- mean(Filtered_hours$Time)
    #}
#
summary(House_Power)
####MELT fct####
df<- melt(Filtered_results_month,id=c("Month"),na.rm = T)
View(df)
####Lessons learned####
#Create new data frames by aggregating time series data and using the cut function
#Use the dplyr package to subset these data frames by grouping selected data and summarise by e.g. mean/ sum