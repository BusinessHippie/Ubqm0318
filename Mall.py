# -*- coding: utf-8 -*-
"""
Created on Tue Aug  6 10:36:10 2019

@author: alexw
"""

import numpy as np # used for numerical feature calculations
import pandas as pd # used for data modification and preparation
from datetime import datetime, date, time
import datetime as dt

import matplotlib.pyplot as plt
import matplotlib
import seaborn as sb #used for more sophisticated visualisations

from sklearn.cluster import KMeans

""" matplotlib setup """
#%matplotlib inline
#%pylab inline
matplotlib.matplotlib_fname()
import matplotlib.rcsetup as rcsetup
print(rcsetup.all_backends)
matplotlib.get_backend()
plt.switch_backend('nbAgg')
matplotlib.use('Qt5Agg')

Mall = pd.read_csv("C:/Users/alexw/Desktop/Practice_Datasets/Mall_customer_segmentation/Mall_Customers.csv")

print(Mall)
Mall.dtypes
Mall.shape
len(Mall.columns)
len(Mall)

Mall.rename(columns={"Annual Income (k$)":"Annual_Income"},inplace =True)
Mall.rename(columns={"Spending Score (1-100)":"Spending_Score"},inplace =True)


#Explore data
Mall['Gender'].describe()
Mall['Annual_Income'].describe()

#Check for Missings
Mall.isna().any()

""" Check for duplicated values """

#Check for duplicate values
Mall.duplicated().any()

Mall['Annual_Income'][Mall['Gender'] == "Male"].mean()
Mall['Annual_Income'][Mall['Gender'] == "Female"].mean()

sb.boxplot(y = Mall['Age'])
plt.show()

sb.boxplot(y = Mall['Annual_Income'])
Mall['Annual_Income'].describe()

#Two Outliers detected
# Remove Outliers
Mall = Mall[Mall['Annual_Income'] < 137]

Mall['Gender'].value_counts()

#___________________________________PLOTS__________________________________________________________

sb.countplot(Mall['Gender'])
sb.countplot(Mall['Annual_Income'])
#___Histogram(s)

sb.distplot(Mall['Age'] , bins = 20)
sb.distplot(Mall['Annual_Income'] , bins = 20)
sb.distplot(Mall['Spending_Score'] , bins = 20)
     
plt.bar(Mall['Gender'],Mall['Age'].mean(),color = "yellow")
plt.show()
plt.bar(Mall['Gender'],Mall['Annual_Income'].mean(),color = "yellow")

plt.plot(Mall['Annual_Income'],Mall['Spending_Score'],color = "orange")
plt.scatter(Mall['Annual_Income'],Mall['Spending_Score'],marker='o', color = "orange",s = 30 , alpha = 0.8)

plt.title("Annual Income vs Spending Score")
plt.ylabel("Spending Score")
plt.xlabel("Annual Income")
plt.show()

plt.scatter(Mall['Age'],Mall['Spending_Score'],color = "orange")
plt.title("Age vs Spending Score")
plt.ylabel("Spending Score")
plt.xlabel("Age")
plt.show()

#_________Correlation Heat Map_______________
sb.heatmap(Mall.corr(), cmap = 'Wistia', annot = True)
plt.title('Heatmap for the Data', fontsize = 12)
plt.show()
#____________________________________________

#________Create new column based on Annual Income_____________APPROVED______________________________________________
Mall['Income_Cat'] = np.where(Mall['Annual_Income'] <= 41.5,'Low',
                            (np.where(Mall['Annual_Income'] <= 61.5, 'Middle',
                                (np.where(Mall['Annual_Income'] <= 78, 'High','Rich Bitch')))))
#___________________________________________________________________________________________________________________


#__________________________________________________APPROVED_________________________________________________________
Mall['Income_Cat'] = np.select([Mall['Annual_Income'] <= 41.5,
                                     Mall['Annual_Income'] <= 61.5,
                                     Mall['Annual_Income'] <= 78],
                                    ['Low','Middle','High'],
                                    default = 'Rich bitch')
#___________________________________________________________________________________________________________________

#__________________________________________________APPROVED_________________________________________________________
#Note: elif statements don't work in lambda functions
Mall['Income_Cat'] = Mall['Annual_Income'].apply(lambda x: 'low' if x <= 41.5 
                                                             else ('middle' if x <= 61.5 
                                                                  else('high' if x <= 78 else 'rich bitch')))
#___________________________________________________________________________________________________________________

Mall['Income_Cat'].value_counts()
sb.countplot(Mall['Income_Cat'])
                       

#Mall = Mall.drop('Income_Cat',axis = 1)


#______________________________________THE SOLUTION:_BIN THE DATA in numeric cases_____________
bins = [0, 41.5, 61.5, 78, np.inf]
bin_names = ['low','middle','high','rich bitch']
Mall['Income_Cat'] = pd.cut(Mall['Annual_Income'],bins,labels= bin_names)
#______________________________________________________________________________________________


#For Clustering Purposes only utilize ___Mall.iloc[:,[3,4]] as a subset of the entire dataframe

#WCSS = Within Cluster Sum of Squares

#we always assume the max number of cluster would be 10
#you can judge the number of clusters by doing averaging
###Static code to get max no of clusters
Mallsub = Mall.iloc[:,[3,4]].values

wcss=[]
for i in range(1,11):
    kmeans = KMeans(n_clusters= i, init='k-means++', random_state=0)
    kmeans.fit(Mallsub)
    wcss.append(kmeans.inertia_)
#We graph the relationship between the number of clusters and Within Cluster Sum of Squares (WCSS)
#then we select the number of clusters where the change in WCSS begins to level off (elbow method).
#inertia_ is the formula used to segregate the data points into clusters
    
#Visualizing the ELBOW method to get the optimal value of K 
plt.plot(range(1,11), wcss)
plt.title('The Elbow Method')
plt.xlabel('no of clusters')
plt.ylabel('wcss')
plt.show()

#---> Optimal k = 5 ("lowest elbow", see plot)

#Model Build
kmeansmodel = KMeans(n_clusters= 5, init='k-means++', random_state=0)
y_kmeans= kmeansmodel.fit_predict(Mallsub)

#For unsupervised learning we use "fit_predict()" wherein for supervised learning we use "fit_tranform()"
#y_kmeans is the final model . Now how and where we will deploy this model in production is depends on what tool we are using.
#This use case is very common and it is used in BFS industry(credit card) and retail for customer segmenattion.

plt.scatter(Mallsub[y_kmeans == 0, 0], Mallsub[y_kmeans == 0, 1], s = 30, c = 'red', label = 'Cluster 1')
plt.scatter(Mallsub[y_kmeans == 1, 0], Mallsub[y_kmeans == 1, 1], s = 30, c = 'blue', label = 'Cluster 2')
plt.scatter(Mallsub[y_kmeans == 2, 0], Mallsub[y_kmeans == 2, 1], s = 30, c = 'green', label = 'Cluster 3')
plt.scatter(Mallsub[y_kmeans == 3, 0], Mallsub[y_kmeans == 3, 1], s = 30, c = 'cyan', label = 'Cluster 4')
plt.scatter(Mallsub[y_kmeans == 4, 0], Mallsub[y_kmeans == 4, 1], s = 30, c = 'magenta', label = 'Cluster 5')
plt.show()
