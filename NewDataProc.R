#Setting the working directory
setwd("/home/javier/Work/FinalProject")
getwd()

#We read the data
require(gdata)
library(Hmisc)
library(stringi)
mydata = read.xls('NewData.xlsx', sheet = 1, header = TRUE)

##SAVING RESULTS##
write.csv(mydata, file = "NewData.csv")
