#Reading the R file
hub <- read.csv("HubwayTrips.csv")
str(hub)
summary(hub)
#
# Duration           Morning        Afternoon         Evening           Night        
# Min.   :     0.0   Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.00000  
# 1st Qu.:   352.0   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.00000  
# Median :   541.0   Median :0.000   Median :0.0000   Median :0.0000   Median :0.00000  
# Mean   :   716.6   Mean   :0.326   Mean   :0.3987   Mean   :0.2505   Mean   :0.02484  
# 3rd Qu.:   840.0   3rd Qu.:1.000   3rd Qu.:1.0000   3rd Qu.:1.0000   3rd Qu.:0.00000  
# Max.   :644688.0   Max.   :1.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.00000  
# Weekday          Weekend            Male             Age      
# Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :17.0  
# 1st Qu.:1.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:27.0  
# Median :1.0000   Median :0.0000   Median :1.0000   Median :32.0  
# Mean   :0.8284   Mean   :0.1716   Mean   :0.7421   Mean   :35.3  
# 3rd Qu.:1.0000   3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:42.0  
# Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :78.0 

#A (i)
#In clustreing, we always group data points of the dataset based on the distance between 
# them which is calculated by calculating square-root of sum of sqaures of different data 
#values.
# We will have to normalize data so that a particular large value column won't overwrite the
# weightage of other small values.
# For Hubway, if we won't normalize the data, the distance between data points would be based on 
# duration only as other values are not big as duration.


#A(ii)
#Normalizing the data
library(caret)
preproc <- preProcess(hub)
hubNorm <- predict(preproc, hub)
summary(hubNorm)

#We can see that the mean is 0 for all the normalized variables

# Duration            Morning          Afternoon          Evening            Night        
# Min.   : -0.23363   Min.   :-0.6955   Min.   :-0.8142   Min.   :-0.5781   Min.   :-0.1596  
# 1st Qu.: -0.11888   1st Qu.:-0.6955   1st Qu.:-0.8142   1st Qu.:-0.5781   1st Qu.:-0.1596  
# Median : -0.05726   Median :-0.6955   Median :-0.8142   Median :-0.5781   Median :-0.1596  
# Mean   :  0.00000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
# 3rd Qu.:  0.04022   3rd Qu.: 1.4378   3rd Qu.: 1.2282   3rd Qu.: 1.7298   3rd Qu.:-0.1596  
# Max.   :209.94236   Max.   : 1.4378   Max.   : 1.2282   Max.   : 1.7298   Max.   : 6.2652  
# Weekday           Weekend             Male              Age         
# Min.   :-2.1975   Min.   :-0.4551   Min.   :-1.6962   Min.   :-1.6671  
# 1st Qu.: 0.4551   1st Qu.:-0.4551   1st Qu.:-1.6962   1st Qu.:-0.7562  
# Median : 0.4551   Median :-0.4551   Median : 0.5895   Median :-0.3008  
# Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000   Mean   : 0.0000  
# 3rd Qu.: 0.4551   3rd Qu.:-0.4551   3rd Qu.: 0.5895   3rd Qu.: 0.6100  
# Max.   : 0.4551   Max.   : 2.1975   Max.   : 0.5895   Max.   : 3.8891  

#Shows the standard deviation is 1 for the nomalized variables

sd(hubNorm$Duration)
sd(hubNorm$Morning)
sd(hubNorm$Weekday)
sd(hubNorm$Male)
sd(hubNorm$Age)

#Using data mining with R as we would need to scale and then again unscale the data
#install.packages("DMwR")
library(DMwR)
hubNorm=scale(hub)
summary(hubNorm)
# We will get same summary what we got with use of library caret.

#B(i)
#K Means Clustering
set.seed(5000)
hubKMC <- kmeans(hubNorm, centers = 10)

#How many trips are in each of the clusters
sort(hubKMC$size)
# [1]  4712  4827 11060 13744 15564 18587 26911 27479 31305 40113

#table for clusters
table(hubKMC$cluster)

#Table of cluster
# 1     2     3     4     5     6     7     8     9    10 
# 40113 26911  4712 15564 13744 18587  4827 11060 27479 31305 

#B(ii)
#cluster centroids
#Cluster centroid for normalized data
normcluster1 <- subset(hubNorm, hubKMC$cluster == 1)
summary(normcluster1)

# We won't use scaled (normalized) cluster data to determine what type of cluster is it 
# means what is the speciality of this cluster as scaled values are confusing. Thus, we 
# will first unscale the cluster data to understand better.

#cluster centroids for unnormalized data

cluster1=unscale(normcluster1,hubNorm)
summary(cluster1)
#
# summary(cluster1)
# Duration          Morning    Afternoon    Evening      Night      Weekday     Weekend 
# Min.   :    0.0   Min.   :0   Min.   :0   Min.   :1   Min.   :0   Min.   :1   Min.   :0  
# 1st Qu.:  364.0   1st Qu.:0   1st Qu.:0   1st Qu.:1   1st Qu.:0   1st Qu.:1   1st Qu.:0  
# Median :  563.0   Median :0   Median :0   Median :1   Median :0   Median :1   Median :0  
# Mean   :  704.4   Mean   :0   Mean   :0   Mean   :1   Mean   :0   Mean   :1   Mean   :0  
# 3rd Qu.:  876.0   3rd Qu.:0   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0   3rd Qu.:1   3rd Qu.:0  
# Max.   :50031.0   Max.   :0   Max.   :0   Max.   :1   Max.   :0   Max.   :1   Max.   :0  
# Male             Age      
# Min.   :0.0000   Min.   :17.0  
# 1st Qu.:1.0000   1st Qu.:26.0  
# Median :1.0000   Median :30.0  
# Mean   :0.7502   Mean   :32.7  
# 3rd Qu.:1.0000   3rd Qu.:37.0  
# Max.   :1.0000   Max.   :72.0 

# Determination of cluster #1 for marketing department of hubway
# this cluster accomodates around 40113(20.6%  of the total trips) trips that are taken 
# in the evening of a week day. 
# Also, it has a diverse age group(17-72) having mostly males(75%)
# Thus, we can say these are popular trips.

normcluster2 <- subset(hubNorm, hubKMC$cluster == 2)
summary(normcluster2)
cluster2=unscale(normcluster2,hubNorm)
summary(cluster2)
# Determination of cluster #2
# This cluster accomodates trips that are taken in the morning of a weekday.
# The age group is between 17-40 and has only males.

normcluster3 <- subset(hubNorm, hubKMC$cluster == 3)
summary(normcluster3)
cluster3=unscale(normcluster3,hubNorm)
summary(cluster3)
# Determination of cluster #3
# This cluster accomodates trips that are taken in the afternoon of a weekday.
# The age group is between 20-72 but mostly over age of 45 having mostly (92%) females.

normcluster4 <- subset(hubNorm, hubKMC$cluster == 4)
summary(normcluster4)
cluster4=unscale(normcluster4,hubNorm)
summary(cluster4)
# Determination of cluster #4
# This cluster accomodates trips that are taken in the morning of a weekday.
# The age group is between 41-72 and has mostly (99%) males.

normcluster5 <- subset(hubNorm, hubKMC$cluster == 5)
summary(normcluster5)
cluster5=unscale(normcluster5,hubNorm)
summary(cluster5)
# Determination of cluster #5
# This cluster accomodates trips that are taken in the morning of a weekday.
# The age group is between 17-63 and has only females.

normcluster6 <- subset(hubNorm, hubKMC$cluster == 6)
summary(normcluster6)
cluster6=unscale(normcluster6,hubNorm)
summary(cluster6)
# Determination of cluster #6
# This cluster accomodates trips that are taken in the afternoon of a weekday.
# The age group is between 40-74 and has only males.

normcluster7 <- subset(hubNorm, hubKMC$cluster == 7)
summary(normcluster7)
cluster7=unscale(normcluster7,hubNorm)
summary(cluster7)
# Determination of cluster #7
# This cluster accomodates trips that are taken in the night on both weekdays and weekends.
# The age group is between 17-40 and has mostly(79%) males.

normcluster8 <- subset(hubNorm, hubKMC$cluster == 8)
summary(normcluster8)
cluster8=unscale(normcluster8,hubNorm)
summary(cluster8)
# Determination of cluster #8
# This cluster accomodates  trips that are taken in the afternoons of a weekday.
# The age group is between 17-40 and has only males.

normcluster9 <- subset(hubNorm, hubKMC$cluster == 9)
summary(normcluster9)
cluster9=unscale(normcluster9,hubNorm)
summary(cluster9)
# Determination of cluster #9
# This cluster accomodates trips that are taken in the afternoon of a weekday.
# The age group is between 17-39 and has only females.

normcluster10 <- subset(hubNorm, hubKMC$cluster == 10)
summary(normcluster10)
cluster10=unscale(normcluster10,hubNorm)
summary(cluster10)
# Determination of cluster #10
# This cluster accomodates trips that are taken in the morning, afternoon and evening
# of a weekend.
# The age group is between 17-78 and has mostly(69%) males.

# B(iii)
#Cluster 7 & 10are pretty interesting as they are having diverse distribution.
#for cluster #7 trips are distributed approximately equal over weekdays and weekends and 
#Also, cluster #7 has very few trips counting to 4827, i.e 2.5 % of the total data
#and trips were taken by both male(41%) and female(59%).
#for cluster 10, these are the only trips taken over the weekends in all times except night

# B(iv)
# Although, 10 clusters are giving clear picture as show in the excel sheet attached with homework
# Still, cluster#10 could be further divided using more cluster centroids.

# C(i)
# K Means clustering for 20 centers

set.seed(8000)
hubKMC2 <- kmeans(hubNorm, centers = 20)
(table(hubKMC2$cluster))
#Table of clusters
# 1     2     3     4     5     6     7     8     9    10    11    12    13    14    15    16 
# 12761  8090  4532 20445  7112 23118 14788  8481 10617  9944  4827  7117 15530    15  1137  2708 
# 17    18    19    20 
# 9239 20819  3776  9246 

#C(ii)
#cluster centroids
normclust1 <- subset(hubNorm, hubKMC2$cluster == 1)
clust1=unscale(normclust1,hubNorm)
summary(clust1)

normclust2 <- subset(hubNorm, hubKMC2$cluster == 2)
clust2=unscale(normclust2,hubNorm)
summary(clust2)

normclust3 <- subset(hubNorm, hubKMC2$cluster == 3)
clust3=unscale(normclust3,hubNorm)
summary(clust3)

normclust4 <- subset(hubNorm, hubKMC2$cluster == 4)
clust4=unscale(normclust4,hubNorm)
summary(clust4)

normclust5 <- subset(hubNorm, hubKMC2$cluster == 5)
clust5=unscale(normclust5,hubNorm)
summary(clust5)

normclust6 <- subset(hubNorm, hubKMC2$cluster == 6)
clust6=unscale(normclust6,hubNorm)
summary(clust6)

normclust7 <- subset(hubNorm, hubKMC2$cluster == 7)
clust7=unscale(normclust7,hubNorm)
summary(clust7)

normclust8 <- subset(hubNorm, hubKMC2$cluster == 8)
clust8=unscale(normclust8,hubNorm)
summary(clust8)

normclust9 <- subset(hubNorm, hubKMC2$cluster == 9)
clust9=unscale(normclust9,hubNorm)
summary(clust9)

normclust10 <- subset(hubNorm, hubKMC2$cluster == 10)
clust10=unscale(normclust10,hubNorm)
summary(clust10)

normclust11 <- subset(hubNorm, hubKMC2$cluster == 11)
clust11=unscale(normclust11,hubNorm)
summary(clust11)

normclust12 <- subset(hubNorm, hubKMC2$cluster == 12)
clust12=unscale(normclust12,hubNorm)
summary(clust12)

normclust13 <- subset(hubNorm, hubKMC2$cluster == 13)
clust13=unscale(normclust13,hubNorm)
summary(clust13)

normclust14 <- subset(hubNorm, hubKMC2$cluster == 14)
clust14=unscale(normclust14,hubNorm)
summary(clust14)

normclust15 <- subset(hubNorm, hubKMC2$cluster == 15)
clust15=unscale(normclust15,hubNorm)
summary(clust15)

normclust16 <- subset(hubNorm, hubKMC2$cluster == 16)
clust16=unscale(normclust16,hubNorm)
summary(clust16)

normclust17 <- subset(hubNorm, hubKMC2$cluster == 17)
clust17=unscale(normclust17,hubNorm)
summary(clust17)

normclust18 <- subset(hubNorm, hubKMC2$cluster == 18)
clust18=unscale(normclust18,hubNorm)
summary(clust18)

normclust19 <- subset(hubNorm, hubKMC2$cluster == 19)
clust19=unscale(normclust19,hubNorm)
summary(clust19)

normclust20 <- subset(hubNorm, hubKMC2$cluster == 20)
clust20=unscale(normclust20,hubNorm)
summary(clust20)
# C(ii)
# After the above are executed we can observe from the attached excel sheet
# that we have uch smaller groups determining the nature of the trips. 
# for example if we see cluster#14 represents all large trips. More informaion 
# regarding the trips (like time, day, gender , age, duration and no. of trips) 
# is detailed in the excel sheet

# C(iii)
# By having more number of clusters we can see more detailed information about the trips, 
# 20 clusters are giving a better insight to the information.