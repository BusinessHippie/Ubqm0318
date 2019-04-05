# -*- coding: utf-8 -*-
"""
Created on Tue Mar 12 09:43:51 2019

@author: AlexanderWinkler
"""
""" Import packages """

import numpy as np # used for numerical feature calculations
import pandas as pd # used for data modification and preparation
from datetime import datetime, date, time
import datetime as dt

import matplotlib.pyplot as plt
import matplotlib
import seaborn as sb #used for more sophisticated visualisations

from sklearn.cross_validation import KFold   #For K-fold cross validation
from sklearn.ensemble import RandomForestClassifier
from sklearn.tree import DecisionTreeClassifier, export_graphviz
from sklearn import metrics

""" matplotlib setup """
#%matplotlib inline
#%pylab inline
matplotlib.matplotlib_fname()
import matplotlib.rcsetup as rcsetup
print(rcsetup.all_backends)
matplotlib.get_backend()
plt.switch_backend('nbAgg')
matplotlib.use('Qt5Agg')


""" Import dataset """

BikeShare = pd.read_csv("C:/Users/AlexanderWinkler/Desktop/Github/Ubqm0318-master/hour.csv")
#Remove Dataframe
#BikeShare = BikeShare.drop

#Datatypes
BikeShare.dtypes

#Get statistical overview
BikeShare.describe()

BikeShare.rename(columns={"hr":"hour"},inplace =True)
BikeShare.rename(columns={"yr":"year"},inplace =True)
BikeShare.rename(columns={"mnth":"month"},inplace =True)
BikeShare.rename(columns={"dteday":"date"},inplace =True)
BikeShare.rename(columns={"weekday":"WeekdayNo"},inplace=True)


""" Convert string to date 
"""

BikeShare['date'] = pd.to_datetime(BikeShare['date'])
#BikeShare['date'] = datetime.strptime(BikeShare['date'],'%y-%m-%d') ___ Here needs to be an object, not a column

""" Check for missing values """

#Remove NA's (missing values)
#Firstly check if there are missing values in any of the columns
BikeShare.isna().any()
#There are no missing values
#BikeShare.dropna()

""" Check for duplicated values """

#Check for duplicate values
BikeShare.duplicated().any()
#There are no duplicated values

""" Check for Outliers """

sb.boxplot(y = BikeShare['cnt'])
BikeShare.boxplot(column = 'cnt')
plt.show()

#By looking at the stats of the 'describe ()' function the upper outlier limit has been determined to be 643
#3rd quartile[281] + 1.5 * (3rd quartile[281]-1st quartile[40])
#No outliers have been detected below the 1st quartile

#Manually take out the outliers, as there is no such function

# Equivalent to the R which function: df[df['columnname'] = condition].index
#y = BikeShare[BikeShare['cnt'] > 643].index
BikeShare = BikeShare[BikeShare['cnt'] < 643]
#This version only yields an array
#np.where(BikeShare['cnt'] < 643)

#Count a variable by its different features
Hours = pd.DataFrame(BikeShare.hour.value_counts())
Hours = Hours.sort_index()

#Convert the temperatures back to standard temperatures
BikeShare['temp'] = BikeShare['temp'] * 39
BikeShare['atemp'] = BikeShare['atemp'] * 50
#Round values

BikeShare['temp'] = BikeShare['temp'].round(0)
BikeShare['atemp'] = BikeShare['atemp'].round(0)

#Create new Weekday dataframe
WeekdayDF = pd.DataFrame({"Weekdays" : ["Monday","Tuesday","Wednsday","Thursday","Friday","Saturday","Sunday"],
                         "WeekdayNo" : [0,1,2,3,4,5,6]},
                         index = [1,2,3,4,5,6,7])

BikeShare = pd.merge(BikeShare,WeekdayDF,how='left',on= 'WeekdayNo')

#In order to conditionally create a new column, this works, but only for one (two) arguments
#BikeShare['Weather'] = np.where(BikeShare['weathersit']==1, 'bad','sunny')

''' Create new column 'Weather' '''

row_indices1 = BikeShare[BikeShare['weathersit'] == 1].index
row_indices2 = BikeShare[BikeShare['weathersit'] == 2].index
row_indices3 = BikeShare[BikeShare['weathersit'] == 3].index
row_indices4 = BikeShare[BikeShare['weathersit'] == 4].index

BikeShare.loc[row_indices1,'Weather'] = 'bad'
BikeShare.loc[row_indices2,'Weather'] = 'cloudy'
BikeShare.loc[row_indices3,'Weather'] = 'mixed'
BikeShare.loc[row_indices4,'Weather'] = 'sunny'
''' This was one step '''

#BikeShare.groupby(['weathersit']).cnt.mean()
Weather_Bikes = BikeShare.groupby('Weather',as_index = False).agg({'cnt':np.mean,
                                                                    'casual':np.mean,
                                                                    'registered':np.mean,
                                                                    'hum':np.mean})

Weather_Bikes = Weather_Bikes.round(1)

Season_Bikes = BikeShare.groupby("season",as_index =False).agg({'cnt':np.mean,
                                                                'casual':np.mean,
                                                                'registered':np.mean,
                                                                'hum':np.mean})

Hours_df = BikeShare.groupby(['hour'], as_index = False).agg({'cnt':np.mean})

#Plot of bike use per hour of the day
plt.title("Use of bikes during the day")
plt.xlabel("Time of the day")
plt.ylabel("Average use")
plt.bar(Hours_df['hour'],Hours_df['cnt'],color = "orange")

Season_Bikes = Season_Bikes.round(1)

#Plot Bars

plt.title("Average cycles rented per weather type")
plt.ylabel("Average use")
plt.bar(Weather_Bikes['Weather'],Weather_Bikes['cnt'], color = 'yellow')

#Alternative plot
#Weather_Bikes.cnt.plot(kind ="bar")

plt.bar(Weather_Bikes['Weather'],Weather_Bikes['casual'], color = 'yellow')
plt.bar(Weather_Bikes['Weather'],Weather_Bikes['registered'], color = 'blue')

#Plot Stacked Bars!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Bars1 = Weather_Bikes['casual']
Bars2 = Weather_Bikes['registered']
Bars = np.add(Weather_Bikes['casual'],Weather_Bikes['registered']).tolist()

p1 = plt.bar(Weather_Bikes['Weather'],Bars1, color = 'yellow')
p2 = plt.bar(Weather_Bikes['Weather'],Bars2,bottom = Bars1, color = 'orange')
plt.title("User allocation depending on weather")
plt.xlabel("Weather situation")
plt.ylabel("Average count")
plt.legend((p1[0], p2[0]), ('Casual users', 'Registered users'))
plt.show()
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

Weather_Bikes1 = pd.melt(Weather_Bikes,id_vars='Weather',value_vars=['casual','registered'])

Stacked_temp = pd.crosstab(Weather_Bikes1['Weather'],Weather_Bikes['variable'])

""" Get Correlation among data """
Cordat = BikeShare.corr()
#Correlation Matrix
Cordat.style.background_gradient(cmap='coolwarm')

#drop (delete) a column by name
#BikeShare = BikeShare.drop('Weather', axis=1)
#BikeShare.drop('Weather',axis=1, inplace= True)
#BikeShare = BikeShare.drop(columns = "Weather")

#Analyse the average bike rental depending on the weather situation
#Weathersit = BikeShare.groupby(['weathersit']).aggregate(np.mean)
#Weathersit = Weathersit.drop

BikeShare.drop(["holiday","workingday","instant","windspeed","year"], 1, inplace=True)
#del BikeShare["holiday","workingday","instant","windspeed","yr","atemp","hum"]

''' Create and run models '''



#Export to CSV
BikeShare.to_csv("C:/Users/AlexanderWinkler/Desktop/Github/Ubqm0318-master/BikeShareResult.csv", index=False, encoding='utf8')
