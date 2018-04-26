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
####convert the sub_meters into minutes####
House_Power$Global_active_power_1<-House_Power$Global_active_power * (1000/60)
str(House_Power)
House_Power$Global_reactive_power_1<-House_Power$Global_reactive_power * (1000/60)
House_Power$Remaining_energyCons<-House_Power$Global_active_power_1-House_Power$Sub_metering_1-House_Power$Sub_metering_2-House_Power$Sub_metering_3
####Analyse data over time####
House_Power$Hour <- cut(as.POSIXct(paste(House_Power$date, House_Power$time),
                              format="%d/%m/%Y %H:%M:%S"), breaks="hour")
Monthly_cons<-data.frame()
Hourly_cons<-data.frame()
Avg.Hourly_cons<-timeAverage(House_Power$Time,avg.time = "hour", statistic = "mean")
#
hour(House_Power$DateTime[1:5])
head(House_Power)
####Overall_Plot####
Global_avr_cons_plot<-plot(House_Power$DateTime,House_Power$Global_active_power_1,type = "l",xlab = "Time",ylab = "Global avr elect cons")
Global_avr_cons_plot
####Filter by hour####
GlobalActivePower_alltime<-ggplot(data=House_Power, aes(x=DateTime,y=Global_active_power_1)) +
  geom_line(color="yellow",size=2)+
  geom_point(size=2)
GlobalActivePower_alltime
#
Filtered_results<- House_Power %>% 
  group_by(hour(DateTime)) %>% 
  summarise(meangap = mean(Global_active_power_1),
            Sub1 = mean(Sub_metering_1),
            Sub2 = mean(Sub_metering_2),
            Sub3 = mean(Sub_metering_3),
            Rem = mean(Remaining_energyCons))
colnames(Filtered_results)[1]<-"Hour"
Filtered_results<-Filtered_results[-c(25),]
#
str(Filtered_results)
View(Filtered_results)
summary(Filtered_results)
#
Filtered_Stand.Dev.<- House_Power %>% 
  group_by(hour(DateTime)) %>% 
  summarise(GlobalAvrgSD = sd(Global_active_power_1),
            Sub1_SD = sd(Sub_metering_1),
            Sub2_SD = sd(Sub_metering_2),
            Sub3_SD = sd(Sub_metering_3),
            Rem= sd(Remaining_energyCons))
#
View(Filtered_Stand.Dev.)
summary(Filtered_Stand.Dev.)
#
Comparative_plot<-ggplot(data=Filtered_results, aes(x=Hour,y=value, color=variable)) +
  geom_line(aes(y = Rem, col = "Remainder"))+ 
  geom_line(aes(y = Sub1, col = "Kitchen"))+
  geom_line(aes(y = Sub2, col = "Laundry"))+
  geom_line(aes(y = Sub3, col = "Water heater"))
Comparative_plot
#
GlobalActivePower_Plot<-ggplot(data=Filtered_results, aes(x=Hour,y=meangap, group=1)) +
  geom_line(color="yellow",size=2)+
  geom_point(size=2)
GlobalActivePower_Plot
#
Sub1_Plot<-ggplot(data=Filtered_results, aes(x=Hour,y=Sub1, group=1)) +
            geom_line(color="purple", size=2)+
            geom_point(size=2)
Sub1_Plot
#
Sub2_Plot<-ggplot(data=Filtered_results, aes(x=Hour,y=Sub2, group=1)) +
  geom_line(color="blue", size=2)+
  geom_point(size=2)
Sub2_Plot
#
Sub3_Plot<-ggplot(data=Filtered_results, aes(x=Hour,y=Sub3, group=1)) +
  geom_line(color="green", size=2)+
  geom_point(size=2)
Sub3_Plot
####Filter by month####

month.abb[House_Power$Date[2]]

month.abb[2]
####Golden Rule####
lubridate:: month(House_Power$DateTime, label=TRUE)
####End Golden Rule####
head(month(House_Power$DateTime))

House_Power$Month <- month(House_Power$DateTime, label = TRUE)

Filtered_results_month<- House_Power %>% 
  group_by(Month) %>% 
  summarise(meangap = mean(Global_active_power_1),
            Sub1 = mean(Sub_metering_1),
            Sub2 = mean(Sub_metering_2),
            Sub3 = mean(Sub_metering_3),
            Rem = mean(Remaining_energyCons))
colnames(Filtered_results_month)[1]<-"Month"
Filtered_results_month<-Filtered_results_month[-c(13),]
View(Filtered_results_month)
str(Filtered_results_month)


head(Filtered_results_month)

df<- melt(Filtered_results_month,id=c("Month"),na.rm = T)
View(df)

#month(as.Date(Filtered_results_month$Month))
Filtered_results_month$Mon<- month.abb[Filtered_results_month$Month]
Filtered_results_month$Mon<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
Filtered_results_month$Month<-as.Date(Filtered_results_month$Month,origin = "January")
#
Comparative_plot_month<-ggplot(data=Filtered_results_month, aes(x=Month, group = 1), color=variable) +
  geom_line(aes(y = Rem, col = "Remainder"))+ 
  geom_line(aes(y = Sub1, col = "Kitchen"))+
  geom_line(aes(y = Sub2, col = "Laundry"))+
  geom_line(aes(y = Sub3, col = "Water heater"))+
  ylab("Watthours")#+
  #scale_x_datetime("",format)#+
  #scale_x_discrete(breaks=c("2.5", "5.0", "7.5","10.0","12.5"),
                    #labels=c("Feb", "May", "Jul","Oct","Dec"))
Comparative_plot_month
#
GlobalActivePower_Plot_month<-ggplot(data=Filtered_results_month, aes(x=Month,y=meangap, group=1)) +
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