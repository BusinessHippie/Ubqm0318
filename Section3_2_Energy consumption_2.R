####______TASK 1________####
####load packages####
install.packages("dplyr")
install.packages("tidyr")
install.packages(c("tidyr", "devtools"))
install.packages("stats")
install.packages("openair")
install.packages("reshape2")
install.packages("forecast")
library(lubridate)
library(dplyr)
library(tidyr)
library(stats)
library(openair)
library(scales)
library(ggplot2)
library(reshape2)
library(forecast)
library(data.table)
library(plotly)
####Import Dataset####
House_Power <- read.csv("C:/Users/alexw/Desktop/Ubiqum/Section3/Task1/household_power_consumption.txt",
                        sep=";", na.strings="?",
                        stringsAsFactors=FALSE)
####Organise data####
House_Power[is.na(House_Power)]<-0
#House_Power$Sub_metering_2<- as.numeric(House_Power$Sub_metering_2)
#House_Power$Sub_metering_1<- as.numeric(House_Power$Sub_metering_1)
#House_Power$Global_intensity<- as.numeric(House_Power$Global_intensity)
#House_Power$Voltage<- as.numeric(House_Power$Voltage)
#House_Power$Global_reactive_power<- as.numeric(House_Power$Global_reactive_power)
#House_Power$Global_active_power<- as.numeric(House_Power$Global_active_power)
#summarise(House_Power)
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
House_Power$DateTime<- as.POSIXct(House_Power$DateTime)
#
#summary(House_Power)
#sd(House_Power$Global_active_power)
#StDev
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
#Df2006<- House_Power %>%
 # filter(year(DateTime)==2006) %>%
  #
  ####Analyse data over time####
#House_Power$Hour <- cut(as.POSIXct(paste(House_Power$DateTime),
 #                                  format="%d/%m/%Y %H:%M:%S"), breaks="hour")
#
lubridate:: month(House_Power$DateTime, label=TRUE)
lubridate:: hour(House_Power$DateTime)
lubridate:: day(House_Power$DateTime)
#hour(House_Power$DateTime[1:5])
#day(House_Power$DateTime[10000:10005])
#weekdays(House_Power$DateTime[1:5])
head(House_Power)
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
House_Power_daily$Month<-lubridate:: month(House_Power_daily$Day,label = TRUE)
#
House_Power_daily$Season[House_Power_daily$Month == "Oct"|"Nov"|"Dec"|"Jan"|"Feb"|"Mar"]<-"Winter Season"
####Seasons___(IF)####
House_Power_daily$Season <- House_Power_daily$Month %>%
  filter("Oct","Nov","Dec","Jan","Feb","Mar")%>%
  summarise("Winter season")
#the easy way
#House_Power_daily$Season[House_Power_daily$Month %in% c("Oct","Nov","Dec","Jan","Feb","Mar")]<- "Winter season"
#House_Power_daily$Season[House_Power_daily$Month %in% c("Apr","May","Jun","Jul","Aug","Sep")]<- "Summer season"
#
#Loop and if
House_Power_daily$Season[House_Power_daily$Month == "Apr"|"May"|"Jun"|"Jul"|"Aug"|"Sep"]<-"Summer Season"
for(i in 1:nrow(House_Power_daily))
                            { if(House_Power_daily$Month[i] =="Oct"|House_Power_daily$Month[i] =="Nov"|House_Power_daily$Month[i] =="Dec"|House_Power_daily$Month[i] =="Jan"|House_Power_daily$Month[i] =="Feb"|House_Power_daily$Month[i] =="Mar")
                              {
                               (House_Power_daily$Season[i] = "Winter season")
                               }else
                              {
                               (House_Power_daily$Season[i] ="Summer season")
                              }
                            }
#Create monthly df
House_Power_monthly<-aggregate(House_Power[11:16],
                               list(month=cut(as.POSIXct(House_Power$DateTime),"month")),
                               sum)
House_Power_monthly$month<-as.POSIXct(House_Power_monthly$month)
colnames(House_Power_monthly)[1]<-"Month"
House_Power_monthly$Month_of_year<-month(House_Power_monthly$Month, label = TRUE)
####Overall_Plot####
#GlobalActivePower_alltime<-ggplot(data=House_Power, aes(x=DateTime,y=Global_active_power_KWh)) +
#  geom_line(color="yellow",size=2)+
#  geom_point(size=2)
#GlobalActivePower_alltime
Global_avr_cons_plot<-plot(House_Power_monthly$Month,House_Power_monthly$Global_active_power_KWh,type = "l",xlab = "Time",ylab = "GlobalAvr")
Global_avr_cons_plot
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
#Summerdays
Summer_Season<-House_Power_daily%>%
  filter(Season == "Summer season") %>%
  group_by(Weekday)%>%
  #arrange(Weekday) %>% 
  #arrange(Weekday=ordered(Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))%>%
  summarise(Sub1 = round(mean(Sub_metering_1_KWh),digits = 1),
            Sub2 = round(mean(Sub_metering_2_KWh),digits = 1),
            Sub3 = round(mean(Sub_metering_3_KWh),digits = 1),
            Rem = round(mean(Rem_EnCons_KWh),digits = 1))
#
Summer_Season$Weekday<-as.factor(Summer_Season$Weekday)
Summer_Season$Weekday<-ordered(Summer_Season$Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
#Winterdays
Winter_Season<-House_Power_daily%>%
  filter(Season == "Winter season") %>%
  group_by(Weekday)%>%
  #arrange(Weekday) %>% 
  #arrange(Weekday=ordered(Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))%>%
  summarise(Sub1 = round(mean(Sub_metering_1_KWh),digits = 1),
            Sub2 = round(mean(Sub_metering_2_KWh),digits = 1),
            Sub3 = round(mean(Sub_metering_3_KWh),digits = 1),
            Rem = round(mean(Rem_EnCons_KWh),digits = 1))
#
Winter_Season$Weekday<-as.factor(Winter_Season$Weekday)
Winter_Season$Weekday<-ordered(Winter_Season$Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
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
ggplotly_weekdays<-ggplotly(Comparative_plot_Weekday)
####Seasonal days####
Filtered_results_Wkd<- House_Power_daily %>% 
  group_by(Weekday) %>% 
  summarise(GlobalAvr = mean(Global_active_power_KWh),
            Sub1 = mean(Sub_metering_1_KWh),
            Sub2 = mean(Sub_metering_2_KWh),
            Sub3 = mean(Sub_metering_3_KWh),
            Rem = mean(Rem_EnCons_KWh))
####Filter by month####
####>>>Golden Rule####
lubridate:: month(House_Power$DateTime, label=TRUE)
head(month(House_Power$DateTime))
#
House_Power$Month <- month(House_Power$DateTime, label = TRUE)
#
Filtered_results_month<- House_Power_monthly %>% 
  group_by(Month_of_year) %>% 
  summarise(GlobalAvr = mean(Global_active_power_KWh),
            Sub1 = mean(Sub_metering_1_KWh),
            Sub2 = mean(Sub_metering_2_KWh),
            Sub3 = mean(Sub_metering_3_KWh),
            Rem = mean(Rem_EnCons_KWh))
#colnames(Filtered_results_month)[1]<-"Month"
#Filtered_results_month<-Filtered_results_month[-c(nrow(Filtered_results_month)),]
str(Filtered_results_month)
head(Filtered_results_month)
#
#month(as.Date(Filtered_results_month$Month))
#Filtered_results_month$Mon<-c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
#Filtered_results_month$Month<-as.Date(Filtered_results_month$Month,origin = "January")
####Facet wrap - seperate by dimension####
Comparative_plot_month<-ggplot(data=Filtered_results_month, aes(x=Month_of_year, group = 1), color=variable) +
  geom_line(aes(y = Rem, col = "Remainder"))+ 
  geom_line(aes(y = Sub1, col = "Kitchen"))+
  geom_line(aes(y = Sub2, col = "Laundry"))+
  geom_line(aes(y = Sub3, col = "Water heater"))+
  facet_wrap(~Year)
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
#Filtered_SD_hour<- House_Power %>% 
#  group_by(hour(DateTime)) %>% 
#  summarise(GlobalAvrgSD = sd(Global_active_power_KWh),
#            Sub1_SD = sd(Sub_metering_1_KWh),
#            Sub2_SD = sd(Sub_metering_2_KWh),
#            Sub3_SD = sd(Sub_metering_3_KWh),
#            Rem= sd(Rem_EnCons_KWh))
#
#colnames(Filtered_SD_hour)[1]<-"Hour"
#Filtered_SD_hour<-Filtered_SD_hour[-c(nrow(Filtered_SD_hour)),]
#View(Filtered_SD_hour)
#
#Comp_SD_hour<-ggplot(data=Filtered_SD_hour, aes(x=Hour,group = 1, color=variable)) +
#  geom_line(aes(y = Rem, col = "Remainder"))+ 
#  geom_line(aes(y = Sub1_SD, col = "Kitchen"))+
#  geom_line(aes(y = Sub2_SD, col = "Laundry"))+
#  geom_line(aes(y = Sub3_SD, col = "Water heater"))+
#  ylab("KWh")
#Comp_SD_hour
#___________________________________________________________________________________________________________
#Filtered_AVR_SD_hour<- House_Power %>% 
#  group_by(hour(DateTime)) %>% 
#  summarise(GlobalPowerUsg_SD = sd(Global_active_power_KWh),
#            GlobalPowerUsg_Avg= mean(Global_active_power_KWh))
#
#View(Filtered_AVR_SD_hour)
#______Defining Avrg + and Avrg -
#Filtered_AVR_SD_hour$AVG_Plus_SD<-Filtered_AVR_SD_hour$GlobalPowerUsg_Avg+Filtered_AVR_SD_hour$GlobalPowerUsg_SD
#Filtered_AVR_SD_hour$AVG_Minus_SD<-Filtered_AVR_SD_hour$GlobalPowerUsg_Avg-Filtered_AVR_SD_hour$GlobalPowerUsg_SD
#colnames(Filtered_AVR_SD_hour)[1]<-"Hour"
#Plot
#Comp_AVR_SD_hour<-ggplot(data=Filtered_AVR_SD_hour, aes(x=Hour,group = 1, color=variable)) +
#  geom_line(aes(y = GlobalPowerUsg_Avg, col = "GlobalPowerUsg_Avg"))+
#  geom_line(aes(y = AVG_Plus_SD, col = "Avr+1_SD"))+
#  geom_line(aes(y = AVG_Minus_SD, col = "Avr-1_SD"))+ 
#  ylab("KWh")
#Comp_AVR_SD_hour
####PLOTS####
#GlobalActivePower_Plot<-ggplot(data=Filtered_results_hour, aes(x=Hour,y=GlobalAvr, group=1)) +
#  geom_line(color="yellow",size=2)+
#  geom_point(size=2)
#GlobalActivePower_Plot
#
#Sub1_Plot<-ggplot(data=Filtered_results_hour, aes(x=Hour,y=Sub1, group=1)) +
#  geom_line(color="purple", size=2)+
#  geom_point(size=2)
#Sub1_Plot
#
#Sub2_Plot<-ggplot(data=Filtered_results_hour, aes(x=Hour,y=Sub2, group=1)) +
#  geom_line(color="blue", size=2)+
#  geom_point(size=2)
####______TASK 2_______####
Outliers<- boxplot.stats(House_Power_hourly$Global_active_power_KWh)
boxplot(House_Power_hourly$Global_active_power_KWh)
Outliers$out
####Create Time Series####
#Monthly
TS_HP_mon_Glob<- ts(House_Power_monthly$Global_active_power_KWh, frequency = 12)
TS_HP_mon_sub1<- ts(House_Power_monthly$Sub_metering_1_KWh, frequency = 12)
TS_HP_mon_sub2<- ts(House_Power_monthly$Sub_metering_2_KWh, frequency = 12)
TS_HP_mon_sub3<- ts(House_Power_monthly$Sub_metering_3_KWh, frequency = 12)
TS_HP_mon_Rem<- ts(House_Power_monthly$Rem_EnCons_KWh, frequency = 12)
#Daily
TS_HP_day_Glob<-ts(House_Power_daily$Global_active_power_KWh, frequency = 365)
#Hourly
#Time_series_House_Power_monthly<-Time_series_House_Power_monthly[,-c(8)]
#Time_series_House_Power_hourly<- ts(House_Power_hourly, frequency = 720)
#Time_series_House_Power_hourly<-Time_series_House_Power_hourly[,-c(8)]
####Plot TS####
plot.ts(TS_HP_mon_Glob)
plot.ts(TS_HP_day_Glob)
plot.ts(TS_HP_hour_Glob)
#
Fit<-tslm(TS_HP_mon_Glob~trend+season)
Fit_day<-tslm(TS_HP_day_Glob~trend+season)
#Annual forecast
plot(forecast(Fit, h=12))#12 = predict for 1 year, 24 = predict for 2 years....
#Daily Forecast
plot(forecast(Fit_day, h=30))
####Compare FC vs Act####
Forecast_1y<-as.data.frame(forecast(Fit, h=12))
Forecast_1y<-rbind(Forecast_1y[2:12,],Forecast_1y[1,])
#Forecast_1y<-Forecast_1y %>% mutate(row(13)=row(1))
Forecast_1y$Actual_mean<-Filtered_results_month$GlobalAvr
Forecast_1y$Month<-Filtered_results_month$Month_of_year
colnames(Forecast_1y)[1]<-"Forecast"
#
Forecast_1y_plot<-ggplot(data=Forecast_1y, aes(x=Month, group = 1), color=variable) +
  geom_line(aes(y = Actual_mean, col = "4y_Avg"))+ 
  geom_line(aes(y = Forecast, col = "Forecast"))+
  ylab("KWh")
Forecast_1y_plot
#
Forecast_1y_plot_HW<-ggplot(data=Forecast_1y, aes(x=Month, group = 1), color=variable) +
  geom_line(aes(y = Actual_mean, col = "4y_Avg"))+ 
  geom_line(aes(y = Forecast_HW, col = "Forecast"))+
  ylab("KWh")+
  labs(title="Annual forecast vs. 4y Average")
Forecast_1y_plot_HW#___________________________________________________________________IMPORTANT PLOT
#
summary(Fit_Rem)
####Decompose Plots####
Decompose_TS_Monthly<-decompose(TS_HP_mon_Glob)
Decompose_TS_Daily<-decompose(TS_HP_day_Glob)
#Decompose_TS_Monthly_Sub1<-decompose(TS_HP_mon_sub1)
#Decompose_TS_Monthly_Sub2<-decompose(TS_HP_mon_sub2)
#Decompose_TS_Monthly_Sub3<-decompose(TS_HP_mon_sub3)
#Decompose_TS_Monthly_Rem<-decompose(TS_HP_mon_Rem)
plot(Decompose_TS_Monthly)
plot(Decompose_TS_Daily)
#plot(Decompose_TS_Monthly_Sub1)
#plot(Decompose_TS_Monthly_Sub2)
#plot(Decompose_TS_Monthly_Sub3)
#plot(Decompose_TS_Monthly_Rem)
plot(as.ts(Decompose_TS_Monthly$seasonal))
plot(as.ts(Decompose_TS_Monthly$trend))
plot(as.ts(Decompose_TS_Monthly$random))
Trend<-data.frame(Decompose_TS_Monthly$x,Decompose_TS_Monthly$trend)
####Plot Trend vs. Actual####
Trend$Forecast<-NA
Trend$Decompose_TS_Monthly.x<-as.numeric(Trend$Decompose_TS_Monthly.x)
Trend$Decompose_TS_Monthly.trend<-as.numeric(Trend$Decompose_TS_Monthly.trend)
Trend[nrow(Trend)+13,] <- 0
Trend[50:61,3]<-Forecast_1y[1:12,8]
colnames(Trend)[1]<-"Actual"
colnames(Trend)[2]<-"Trend"
Trend$Date <- seq(from=as.Date("2006-12-16"), to=as.Date("2011-12-16"), by = 'month')
Trend<-Trend[,c(4,1,2,3)]
Trend$Date<-as.POSIXct(Trend$Date)
#
Trend_plot<-ggplot(data=Trend, aes(x=Date, group = 1), color=variable) +
  geom_line(aes(y = Actual, col = "Actual"),size=1.5)+ 
  geom_line(aes(y = Trend, col = "Trend"),size=1.5)+
  geom_line(aes(y = Forecast, col = "Forecast"),size=1.5)+
  ylab("KWh")+
  xlab("Year")+
  labs(title="Actual, Trend and Forecast")#+
  #scale_x_continuous(breaks = seq(from=as.Date("2007-12-16"), to=as.Date("2011-12-16"), by = 'month'))
Trend_plot
#plot daily trend
plot(as.ts(Decompose_TS_Daily$trend))
#ggplot monthly trend
FC1_Dec_Plot<-ggplot(data=Decompose_TS_Monthly$trend, aes(x,y, group=1)) +
  geom_line(color="purple", size=2)+
  geom_point(size=2)
####Holt Winters - Smooting####
#Annual
Exp.Smooth.Glob.<-HoltWinters(TS_HP_mon_Glob)
plot(Exp.Smooth.Glob.)
HW_Forecast<-predict(Exp.Smooth.Glob.,n.ahead = 12,prediction.interval = T,level = 0.95)
plot(Exp.Smooth.Glob.,HW_Forecast)
HW_Forecast_DF<-as.data.frame(HW_Forecast)
HW_Forecast_DF_mod<- as.data.frame(HW_Forecast_DF)
HW_Forecast_DF_mod<-rbind(HW_Forecast_DF_mod[2:12,],HW_Forecast_DF_mod[1,])
Forecast_1y$Forecast_HW<-HW_Forecast_DF_mod$fit
#Daily
Exp.Smooth.Glob._day<-HoltWinters(TS_HP_day_Glob)
plot(Exp.Smooth.Glob._day)
HW_Forecast_day<-predict(Exp.Smooth.Glob._day,n.ahead = 28,prediction.interval = T,level = 0.80)
plot(Exp.Smooth.Glob._day,HW_Forecast_day)
#Hourly
#Exp.Smooth.Glob._hour<-HoltWinters(TS_HP_hour_Glob)
#plot(Exp.Smooth.Glob._hour)
#HW_Forecast_hour<-predict(Exp.Smooth.Glob._hour,n.ahead = 24,prediction.interval = T,level = 0.90)
#plot(Exp.Smooth.Glob._hour,HW_Forecast_hour)
####Melt DF####
#Stacked by hour______________________________________________________________IMPORTANT PLOT
Stacked_hour<-melt(data = Filtered_results_hour,id="Hour")
Stacked_hour <- Stacked_hour[!Stacked_hour$variable == "GlobalAvr",]
Stacked_hour$value<-round(Stacked_hour$value,digits = 1)
Stacked_col_hour<-ggplot(Stacked_hour, aes(x=Hour,y=value, fill=variable)) +
  geom_bar(stat = 'identity')+
  ylab("KWh")+
  labs(title="Daily energy consumption by hour")+
  geom_text(aes(label=value),size=3,position = position_stack(vjust = 0.5))
Stacked_col_hour + scale_fill_discrete(labels=c("Kitchen","Laundry","WaterHeater","Remaining"))
#Stacked by day______________________________________________________________IMPORTANT PLOT
Stacked_day<-melt(data = Filtered_results_Wkd)
Stacked_day <- Stacked_day[!Stacked_day$variable == "GlobalAvr",]
Stacked_day$value<-round(Stacked_day$value,digits = 1)
Stacked_col_Weekday<-ggplot(Stacked_day, aes(x=Weekday,y=value, fill=variable)) +
  geom_bar(stat = 'identity')+
  ylab("KWh")+
  labs(title="Weekly energy consumption by day")+
  geom_text(aes(label= value),size=3,position = position_stack(vjust = 0.5))
Stacked_col_Weekday + expand_limits(y=c(0,40)) + scale_fill_discrete(labels=c("Kitchen","Laundry","WaterHeater","Remaining"))
#by summerday______________________________________________________________IMPORTANT PLOT
Stacked_summerday<-melt(data=Summer_Season)
Stacked_summerday<-Stacked_summerday[!Stacked_summerday$variable =="GlobalAvr",]
Stacked_summerday$value<-round(Stacked_summerday$value,digits = 1)
Stacked_col_summerday<-ggplot(Stacked_summerday, aes(x=Weekday,y=value, fill=variable)) +
  geom_bar(stat = 'identity')+
  ylab("KWh")+
  labs(title="Weekly energy consumption during summer")+
  geom_text(aes(label= value),size=3,position = position_stack(vjust = 0.5))
Stacked_col_summerday + expand_limits(y=c(0,40)) + scale_fill_discrete(labels=c("Kitchen","Laundry","WaterHeater","Remaining"))
#by winterday______________________________________________________________IMPORTANT PLOT
Stacked_winterday<-melt(data=Winter_Season)
Stacked_winterday<-Stacked_winterday[!Stacked_winterday$variable =="GlobalAvr",]
Stacked_winterday$value<-round(Stacked_winterday$value,digits = 1)
Stacked_col_winterday<-ggplot(Stacked_winterday, aes(x=Weekday,y=value, fill=variable)) +
  geom_bar(stat = 'identity')+
  ylab("KWh")+
  labs(title="Weekly energy consumption during winter")+
  geom_text(aes(label= value),size=3,position = position_stack(vjust = 0.5))
Stacked_col_winterday + expand_limits(y=c(0,40)) + scale_fill_discrete(labels=c("Kitchen","Laundry","WaterHeater","Remaining"))
#Stacked by summerday
#Stacked_col_summerday<-ggplot(House_Power_daily%>%
#                                filter(Season == "Summer season") %>%
#                                group_by(Weekday)%>%
#                                mutate(Day_of_week = as.factor(Weekday)) %>%
                                #arrange(Weekday) %>% 
                                #arrange(Weekday=ordered(Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")))%>%
#                                summarise(Sub1 = round(mean(Sub_metering_1_KWh),digits = 1),
#                                          Sub2 = round(mean(Sub_metering_2_KWh),digits = 1),
#                                          Sub3 = round(mean(Sub_metering_3_KWh),digits = 1),
#                                          Rem = round(mean(Rem_EnCons_KWh),digits = 1)) %>%
#                                melt,
#                              aes(x=Day_of_week,y=value, fill=variable)) +
#                              geom_bar(stat = 'identity')+
#                              ylab("KWh")+
#                              labs(title="Weekly energy consumption during summer")+
#                              geom_text(aes(label= value),size=3,position = position_stack(vjust = 0.5))+
#                              expand_limits(y=c(0,40))
#                              
Filtered_results_Wkd$Weekday<-as.factor(Filtered_results_Wkd$Weekday)
Filtered_results_Wkd$Weekday<-ordered(Filtered_results_Wkd$Weekday, levels=c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
#Stacked by winterday
#Stacked_col_winterday<-ggplot(House_Power_daily%>%
#                                filter(Season == "Winter season")%>%
#                                group_by(Weekday) %>% 
#                                summarise(Sub1 = round(mean(Sub_metering_1_KWh),digits = 1),
#                                          Sub2 = round(mean(Sub_metering_2_KWh),digits = 1),
#                                          Sub3 = round(mean(Sub_metering_3_KWh),digits = 1),
#                                          Rem = round(mean(Rem_EnCons_KWh),digits = 1))%>%
#                                melt,
#                              aes(x=Weekday,y=value, fill=variable)) +
#  geom_bar(stat = 'identity')+
#  ylab("KWh")+
#  labs(title="Weekly energy consumption during winter")+
#  geom_text(aes(label= value),size=3,position = position_stack(vjust = 0.5))
#
#Stacked by month____________________________________________________________________________IMPORTANT PLOT
Stacked_month<-melt(data = Filtered_results_month)
Stacked_month <- Stacked_month[!Stacked_month$variable == "GlobalAvr",]
Stacked_month$value<-round(Stacked_month$value,digits = 0)
Stacked_col_month<-ggplot(Stacked_month, aes(x=Month_of_year,y=value, fill=variable,labels)) +
  geom_bar(stat = 'identity')+
  ylab("KWh")+
  labs(title="Annual energy consumption by month")+
  geom_text(aes(label=value),size=3,position = position_stack(vjust = 0.5))
Stacked_col_month + scale_fill_discrete(labels=c("Kitchen","Laundry","WaterHeater","Remaining"))
####Donut####
library(data.table)
Donut<- transpose(Filtered_results_Wkd)
colnames(Donut)<-rownames(Filtered_results_Wkd)
rownames(Donut)<-colnames(Filtered_results_Wkd)
Donut<-Donut[c(2:3)]
colnames(Donut)[1:2]<-c("Monday","Saturday")
Donut<-Donut[-c(1:2),]
Donut$Subs<-c("Sub1","Sub2","Sub3","Rem")
#
Donut$Monday<-as.numeric(Donut$Monday)
Donut$Saturday<-as.numeric(Donut$Saturday)
Donut$Subs<-as.factor(Donut$Subs)
Donut$Monday<-round(Donut$Monday,2)
Donut$Saturday<-round(Donut$Saturday,2)
#
Donut$fraction = Donut$Monday / sum(Donut$Monday)
Donut$fraction_Sat = Donut$Saturday / sum(Donut$Saturday)
Donut = Donut[order(Donut$fraction), ]
Donut$ymax = cumsum(Donut$fraction)
Donut$ymaxSat = cumsum(Donut$fraction_Sat)
Donut$ymin = c(0, head(Donut$ymax, n=-1))
Donut$yminSat = c(0, head(Donut$ymaxSat, n=-1))
#Chart 1
#Donut_chart<-ggplot(Donut,aes(fill=Subs, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
#  geom_rect() +
#  coord_polar(theta="y") +
#  xlim(c(0, 4)) +
#  theme(panel.grid=element_blank()) +
#  theme(axis.text=element_blank()) +
#  theme(axis.ticks=element_blank())+
#  annotate("text", x = 0, y = 0, label = "Mondays") +
#  labs(title="")
#Chart 2_____!!!
Donut_chart<-ggplot(Donut)+
  geom_rect(aes(fill=Subs, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
  geom_rect(aes(fill=Subs, ymax=ymaxSat, ymin=yminSat, xmax=2.8, xmin=1.8)) +
  coord_polar(theta="y") +
  xlim(c(0, 4)) +
  theme(aspect.ratio = 1)+
  theme(panel.grid=element_blank()) +
  theme(axis.text=element_blank()) +
  theme(axis.ticks=element_blank())+
  annotate("text", x = 2.5, y = 0, label = "Weekend") +
  annotate("text", x = 3.5, y = 0, label = "Weekday") +
  labs(title="Weekday vs. Weekend")#+
  #geom_text(mapping = NULL,data=NULL,stat = "identity", size=3)___________________________IMPORTANT GRAPH
Donut_chart + scale_fill_discrete(labels=c("Remaining","Laundry","Kitchen","WaterHeater"))
#Chart 3
#Donut_chart<-ggplot(Donut,aes(fill=Subs, ymax=ymax, ymin=ymin, xmax=4, xmin=3))+
#  geom_rect(aes(fill=Monday)) +
#  geom_rect(aes(fill=Saturday,xmax=2.5,xmin=1.5)) +
#  coord_polar(theta="y") +
#  xlim(c(0, 4)) +
#  theme(aspect.ratio = 1)
  #theme(panel.grid=element_blank()) +
  #theme(axis.text=element_blank()) +
  #theme(axis.ticks=element_blank())+
  #annotate("text", x = 0, y = 0, label = "Mondays") +
  #labs(title="")
####Pie Chart Mon####____________________________________________________________________IMPORTANT GRAPH
Bar_chart<-ggplot(Donut,aes(x="",y=Monday,fill=Subs))+
 geom_bar(width = 1,stat = "identity")
Pie<-Bar_chart +coord_polar("y",start = 0)
Pie<-Pie+ 
  geom_text(aes(label=Monday),size=5,position = position_stack(vjust = 0.5))+
  labs(title="Monday energy consumption allocation")+
  scale_fill_discrete(labels=c("Remaining","Laundry","Kitchen","WaterHeater"))
Pie
#Weekend (Saturday) chart________________________________________________________________IMPORTANT GRAPH
Bar_chart_Sat<-ggplot(Donut,aes(x="",y=Saturday,fill=Subs))+
  geom_bar(width = 1,stat = "identity")
Pie_Sat<-Bar_chart_Sat +coord_polar("y",start = 0)
Pie_Sat<-Pie_Sat+ 
  geom_text(aes(label=Saturday),size=5,position = position_stack(vjust = 0.5))+
  labs(title="Saturday energy consumption allocation")+
  scale_fill_discrete(labels=c("Remaining","Laundry","Kitchen","WaterHeater"))
Pie_Sat
#geom_text(aes(x=1,y = Monday/3 + c(0, cumsum(Monday)[-length(Monday)]), 
#           label = percent(fraction), size=5))
####Plot 2007####
DF_2007<-House_Power_daily%>%select(Global_active_power_KWh,Day) %>%filter(Day >= as.Date("2007-01-01") & Day<= as.Date("2007-12-31"))
DF_2007<-DF_2007[c(2,1)]
DF_2007_try<-cbind(DF_2007[,2,drop = FALSE],DF_2007[,1,drop = FALSE])
#DF_2007<-cbind(DF_2007[,c(2)],as.POSIXct(DF_2007[,c(1)]))

x<-as.Date("2009-01-01")
y<-as.Date("2009-01-31")
#
Scenario<-c("a","b","c","d")
Date<-as.POSIXct(c("2006-12-01","2007-12-01","2008-12-01","2009-12-01"))
EndDate<-as.POSIXct(c("2006-12-31","2007-12-31","2008-12-31","2009-12-31"))
ReferenceData<-data.frame(Scenario,Date,EndDate)
#
huhuhu<-ggplot(House_Power_daily%>%
         select(Day,Global_active_power_KWh) %>%
        filter(Day >= x & Day<= y),
        aes(x=Day,y=Global_active_power_KWh,group=1))+  
        geom_line(color="blue")#+
huhuhu  
#
House_Power_daily%>%
  select(Day,Global_active_power_KWh) %>%
  filter(Day >= as.Date("2009-12-01") & Day<= as.Date("2009-12-31"))
#
House_Power_daily%>%
  select(Day,Global_active_power_KWh) %>%
  filter(Day >= ReferenceData[2,2] & Day<= ReferenceData[2,3])
#
weekdays.Date(2008-01-01,abbreviate = F)
#theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank())
#+title("2007")
#  scale_x_continuous(breaks = seq(from = as.Date("2007-01-01"), to = as.Date("2007-01-31"), by = 'month'))
#They may want to get a forecast of their next energy bill based on their current and past consumption
#how and when could they save money?
#What kind of family is this particular end customer, i.e. any energy cons. patterns?
#based on energy consumptinon, one may want to switch the energy provider
#plot forecast against trend
#______________________________________________________________________________________________________________________
#_______________________________________________________Learnings______________________________________________________
#next time in ggplot, include +facet_wrap - will seperate the graphs by respective time dimension
#
#Use ggplot
library(plotly)
