#Reading th csv File
parole=read.csv("parole.csv")
str(parole)

#Question A: 
# Number of violators
table(parole$Violator)
#Output
#0   1 
#597  78 
# 11.55% of parolees have violated the terms.

#Question B
#installing the caTools package
#install.packages("caTools")
#loading the package
library(caTools)
#splitting the data into training and testing set based on Violator, 70% of the data in training set
set.seed(88)
split = sample.split(parole$Violator, SplitRatio = 0.7)
Paroletrain = subset(parole, split == TRUE)
Paroletest = subset(parole, split == FALSE)

str(Paroletrain)
#'data.frame':	473 obs. of  9 variables:
#$ Male            : int  1 0 1 1 1 1 0 0 0 1 ...
#$ RaceWhite       : int  1 1 0 1 0 1 1 1 0 1 ...
#$ Age             : num  33.2 39.7 29.5 22.4 46.7 31 24.6 32.6 28.4 37.8 ...
#$ State           : Factor w/ 4 levels "Kentucky","Louisiana",..: 3 3 3 3 3 3 3 3 3 3 ...
#$ TimeServed      : num  5.5 5.4 5.6 5.7 6 6 4.8 4.5 4.5 5.3 ...
#$ MaxSentence     : int  18 12 12 18 18 18 12 13 12 8 ...
#$ MultipleOffenses: int  0 0 0 0 0 0 0 0 1 0 ...
#$ Crime           : Factor w/ 4 levels "Driving","Drugs",..: 1 2 2 4 1 2 4 2 4 2 ...
#$ Violator        : int  0 0 0 0 0 0 0 0 0 0 ...
str(Paroletest)
#'data.frame':	202 obs. of  9 variables:
#$ Male            : int  1 1 1 1 1 1 1 1 1 1 ...
#$ RaceWhite       : int  0 0 1 1 1 1 0 0 1 1 ...
#$ Age             : num  21.6 29.1 20.5 30.1 41.7 42.3 23 49.9 36.7 39.1 ...
#$ State           : Factor w/ 4 levels "Kentucky","Louisiana",..: 3 3 3 3 3 3 3 3 3 3 ...
#$ TimeServed      : num  5.4 4.7 5.9 5.3 5.5 5.8 5.3 4.7 0.9 0 ...
#$ MaxSentence     : int  12 12 12 16 8 16 8 16 16 16 ...
#$ MultipleOffenses: int  0 0 0 0 0 0 0 0 0 0 ...
#$ Crime           : Factor w/ 4 levels "Driving","Drugs",..: 4 3 4 2 2 4 4 4 2 2 ...
#$ Violator        : int  0 0 0 0 0 0 0 0 0 0 ...

#Building a logistic regression Model

ParolelogModel=glm(Violator ~ ., data=Paroletrain, family="binomial")
summary(ParolelogModel)
# Call:
#   glm(formula = Violator ~ ., family = "binomial", data = Paroletrain)
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -1.7221  -0.3959  -0.2403  -0.1494   2.8212  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      -3.29370    1.47788  -2.229 0.025836 *  
#   Male              0.65662    0.47189   1.391 0.164082    
# RaceWhite        -0.67930    0.42425  -1.601 0.109338    
# Age               0.01739    0.01662   1.046 0.295452    
# StateLouisiana    0.67688    0.60992   1.110 0.267087    
# StateOther       -0.17308    0.54082  -0.320 0.748949    
# StateVirginia    -3.38536    0.73642  -4.597 4.28e-06 ***
#   TimeServed       -0.06809    0.11415  -0.596 0.550863    
# MaxSentence       0.04536    0.05227   0.868 0.385552    
# MultipleOffenses  1.42426    0.39268   3.627 0.000287 ***
#   CrimeDrugs       -0.23931    0.67429  -0.355 0.722655    
# CrimeLarceny      0.99710    0.69991   1.425 0.154266    
# CrimeOther        0.19106    0.58920   0.324 0.745731    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 340.04  on 472  degrees of freedom
# Residual deviance: 242.18  on 460  degrees of freedom
# AIC: 268.18
# Number of Fisher Scoring iterations: 6

#Question B 
#Part-1
# The model Built here has the significant variables like multiple offenses & Statevirginia
#Question B
#Part-2
#Calculating the odds
Logodds<--3.29370+1*0.65662+1*(-0.67930)+50*0.01739+1*(-0.17308)+3*(-0.06809)+12*0.04536+0*(1.42426)+1*0.99710
Logodds
#exponential of the odds
Exp<-exp(-Logodds)
#Probability 
prob = 1/(1+Exp)
prob
#0.2170723 is the probability that the parolee is the violator


#refine the model

# Refining the model by removing 'crime
ParolelogModel = glm(Violator ~ Male + RaceWhite + Age + State + TimeServed + MaxSentence + MultipleOffenses, data = Paroletrain, family=binomial)
summary(ParolelogModel)

# Refining the model by removing 'Timeserved'
ParolelogModel = glm(Violator ~ Male + RaceWhite + Age + State + MaxSentence + MultipleOffenses, data = Paroletrain, family=binomial)
summary(ParolelogModel)

#Refining the model by removing 'MaxSentence'
ParolelogModel = glm(Violator ~ Male + RaceWhite + Age + State + MultipleOffenses, data = Paroletrain, family=binomial)
summary(ParolelogModel)

#Refining the model by removing 'Age'
ParolelogModel = glm(Violator ~ Male + RaceWhite + State + MultipleOffenses, data = Paroletrain, family=binomial)
summary(ParolelogModel)

#Refining the model by removing 'Male'
ParolelogModel = glm(Violator ~ RaceWhite + State + MultipleOffenses, data = Paroletrain, family=binomial)
summary(ParolelogModel)

#Refining the model by removing 'RaceWhite'
ParolelogModel = glm(Violator ~ State + MultipleOffenses, data = Paroletrain, family=binomial)
summary(ParolelogModel)
#Now, all the variables are significant

#Question B
#Part-3
# Maximum Predicted probability of violation
PredictTest = predict(ParolelogModel, type="response", newdata = Paroletest)
summary (PredictTest)
# output
# Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.006239 0.026580 0.077850 0.087510 0.079260 0.534100 

#confusion matrix with threshold of 0.5
table(Paroletest$Violator, PredictTest > 0.5)

# FALSE TRUE
# 0   176    3
# 1    16    7

#Sensitivity TP/TP+FN
7/(7+16)
#Output: 0.3043478

#specificity TN/TN+FP
176/(3+176)
#output:0.9832402

#False positive rate
1-(176/(176+3))
#output:0.01675978

#Accuracy 
(176+7)/(7+176+3+16)
#Output:  0.9059406
#Gives 90.59% accuracy


# Prediction of probability of violators on training set
PredictTrain = predict(ParoleModel, type="response", newdata = Paroletrain)
summary (PredictTrain)
#    Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.006239 0.026580 0.077850 0.087510 0.079260 0.534100 

#confusion matrix with threshold of 0.5
table(Paroletrain$Violator, PredictTrain > 0.5)

#   FALSE TRUE
# 0   396   22
# 1    32   23

#Sensitivity TP/TP+FN
23/(23+32)
#0.4181818

#Specificity TN/TN+FP
396/(396+22)
#0.9473684

#Accuracy TN+TP/No of observations
(396+23)/(396+22+32+23)
#0.8858351

#Question B
#Part 4
#Baseline method outcome
(176+3)/(176+3+16+7)
#0.8861386
#Gives 88.61% accuracy which is lower than accuracy of our predicted model using logistics regression

#Question B
#Part 5
#The board members consider the false negative values. 
#As the threshold increases the violators in the set increase, decresing the threshold increases the 
#the positivity(False positive)

#Question B
#Part-6
install.packages("ROCR")
library(ROCR)
#Calculating the AUC of the model, using the ROCR Package
ROCRpred = prediction(PredictTest, Paroletest$Violator)
ROCCurve = performance(ROCRpred, "tpr", "fpr")
plot(ROCCurve)
plot(ROCCurve, colorize=TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,0.7))
as.numeric(performance(ROCRpred, "auc")@y.values)

#Question C
#We should use a dataset tracking a group of parolees from the start of their parole until either they violated parole or they completed their term. 
  


