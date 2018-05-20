#Loading libraries
library(fastAdaboost)
library(caret)
library(adabag)

#Setting the working directory
#setwd('C:/Users/Javier/Documents/GitHub/FinalProject')
setwd('/home/javier/Work/FinalProject')
getwd()

#We read the data
mydata = read.csv('DataFeatures.csv', header = TRUE, sep = ",")

#DISTANCE
mindist<-min(mydata$Distance)
maxdist<-max(mydata$Distance)

mydata$Distance<-(mydata$School_GPA-mindist)/(maxdist-mindist)

#DISTANCE_T
mindistt<-min(mydata$Distance_T)
maxdistt<-max(mydata$Distance_T)

mydata$Distance_T<-(mydata$School_GPA-mindistt)/(maxdistt-mindistt)


##SAVING RESULTS##
write.csv(mydata, file = "DataFeatures.csv")