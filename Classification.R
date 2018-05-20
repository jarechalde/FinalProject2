#Loading libraries
library(rpart) #Classification tree library

#Setting the working directory
setwd("/home/javier/Work/FinalProject")
getwd()

#We read the data
cleandata = read.csv("DataClean.csv", header = TRUE, sep = ";")
enrichdata = read.csv('DataFeatures.csv', header = TRUE, sep = ";")

fit<-rpart(Completion~., method = "anova", data = cleandata)

printcp(fit)