#Loading libraries
library(stringr)
library(gmapsdistance)

#Setting the working directory
#setwd('C:/Users/Javier/Documents/GitHub/FinalProject')
setwd('/home/javier/Work/FinalProject')
getwd()

#We read the data
data = read.csv('UniversitiesScorecard.csv', header = TRUE, sep = ",")
#datafeat = read.csv('DataFeatures.csv', header = TRUE, sep = ",")

#Select universities from MA
data<-data[data$STABBR=='MA',]

#Getting the different school names
#Schools<-unique(data$INSTNM)
Schools<-data$INSTNM

#Getting only unique schools
#data<-data[data$INSTNM %in% names]


#Creating the new dataframe with the schools from MA
newdata<-data.frame(Schools)

newdata$Latitude<-NA
newdata$Longitude<-NA
newdata$City<-NA

for (i in 1:length(Schools)){
  name<-as.character(Schools[i])
  
  #Getting the data from that school
  datasch<-data[data$INSTNM==name,]
  
  latitude<-as.character(datasch$LATITUDE)
  longitude<-as.character(datasch$LONGITUDE)
  
  city<-as.character(datasch$CITY)
  
  newdata$Longitude[newdata$Schools == name]<-longitude
  newdata$Latitude[newdata$Schools == name]<-latitude
  newdata$City[newdata$Schools == name]<-city
  
}

newdata$Schools<-factor(newdata$Schools)

options(digits = 12)

data$PCIP01[data$PCIP01=='NULL']<-0
data$PCIP01<-as.numeric(as.character(data$PCIP01))
data$PCIP01[data$PCIP01!=0]<-1
factor(data$PCIP01)
newdata$PCIP01<-data$PCIP01

data$PCIP03[data$PCIP03=='NULL']<-0
data$PCIP03<-as.numeric(as.character(data$PCIP03))
data$PCIP03[data$PCIP03!=0]<-1
factor(data$PCIP03)
newdata$PCIP03<-data$PCIP03

data$PCIP05[data$PCIP05=='NULL']<-0
data$PCIP05<-as.numeric(as.character(data$PCIP05))
data$PCIP05[data$PCIP05!=0]<-1
factor(data$PCIP05)
newdata$PCIP05<-data$PCIP05

data$PCIP09[data$PCIP09=='NULL']<-0
data$PCIP09<-as.numeric(as.character(data$PCIP09))
data$PCIP09[data$PCIP09!=0]<-1
factor(data$PCIP09)
newdata$PCIP09<-data$PCIP09

data$PCIP11[data$PCIP11=='NULL']<-0
data$PCIP11<-as.numeric(as.character(data$PCIP11))
data$PCIP11[data$PCIP11!=0]<-1
factor(data$PCIP11)
newdata$PCIP11<-data$PCIP11

data$PCIP13[data$PCIP13=='NULL']<-0
data$PCIP13<-as.numeric(as.character(data$PCIP13))
data$PCIP13[data$PCIP13!=0]<-1
factor(data$PCIP13)
newdata$PCIP13<-data$PCIP13

data$PCIP14[data$PCIP14=='NULL']<-0
data$PCIP14<-as.numeric(as.character(data$PCIP14))
data$PCIP14[data$PCIP14!=0]<-1
factor(data$PCIP14)
newdata$PCIP14<-data$PCIP14

data$PCIP15[data$PCIP15=='NULL']<-0
data$PCIP15<-as.numeric(as.character(data$PCIP15))
data$PCIP15[data$PCIP15!=0]<-1
factor(data$PCIP15)
newdata$PCIP15<-data$PCIP15

data$PCIP16[data$PCIP16=='NULL']<-0
data$PCIP16<-as.numeric(as.character(data$PCIP16))
data$PCIP16[data$PCIP16!=0]<-1
factor(data$PCIP16)
newdata$PCIP16<-data$PCIP16

data$PCIP23[data$PCIP23=='NULL']<-0
data$PCIP23<-as.numeric(as.character(data$PCIP23))
data$PCIP23[data$PCIP23!=0]<-1
factor(data$PCIP23)
newdata$PCIP23<-data$PCIP23

data$PCIP26[data$PCIP26=='NULL']<-0
data$PCIP26<-as.numeric(as.character(data$PCIP26))
data$PCIP26[data$PCIP26!=0]<-1
factor(data$PCIP26)
newdata$PCIP26<-data$PCIP26

data$PCIP27[data$PCIP27=='NULL']<-0
data$PCIP27<-as.numeric(as.character(data$PCIP27))
data$PCIP27[data$PCIP27!=0]<-1
factor(data$PCIP27)
newdata$PCIP27<-data$PCIP27

data$PCIP30[data$PCIP30=='NULL']<-0
data$PCIP30<-as.numeric(as.character(data$PCIP30))
data$PCIP30[data$PCIP30!=0]<-1
factor(data$PCIP30)
newdata$PCIP30<-data$PCIP30

data$PCIP38[data$PCIP38=='NULL']<-0
data$PCIP38<-as.numeric(as.character(data$PCIP38))
data$PCIP38[data$PCIP38!=0]<-1
factor(data$PCIP38)
newdata$PCIP38<-data$PCIP38

data$PCIP40[data$PCIP40=='NULL']<-0
data$PCIP40<-as.numeric(as.character(data$PCIP40))
data$PCIP40[data$PCIP40!=0]<-1
factor(data$PCIP40)
newdata$PCIP40<-data$PCIP40

data$PCIP42[data$PCIP42=='NULL']<-0
data$PCIP42<-as.numeric(as.character(data$PCIP42))
data$PCIP42[data$PCIP42!=0]<-1
factor(data$PCIP42)
newdata$PCIP42<-data$PCIP42

data$PCIP45[data$PCIP45=='NULL']<-0
data$PCIP45<-as.numeric(as.character(data$PCIP45))
data$PCIP45[data$PCIP45!=0]<-1
factor(data$PCIP45)
newdata$PCIP45<-data$PCIP45

data$PCIP50[data$PCIP50=='NULL']<-0
data$PCIP50<-as.numeric(as.character(data$PCIP50))
data$PCIP50[data$PCIP50!=0]<-1
factor(data$PCIP50)
newdata$PCIP50<-data$PCIP50

data$PCIP51[data$PCIP51=='NULL']<-0
data$PCIP51<-as.numeric(as.character(data$PCIP51))
data$PCIP51[data$PCIP51!=0]<-1
factor(data$PCIP51)
newdata$PCIP51<-data$PCIP51

data$PCIP52[data$PCIP52=='NULL']<-0
data$PCIP52<-as.numeric(as.character(data$PCIP52))
data$PCIP52[data$PCIP52!=0]<-1
factor(data$PCIP52)
newdata$PCIP52<-data$PCIP52

data$PCIP54[data$PCIP54=='NULL']<-0
data$PCIP54<-as.numeric(as.character(data$PCIP54))
data$PCIP54[data$PCIP54!=0]<-1
factor(data$PCIP54)
newdata$PCIP54<-data$PCIP54

hola<-data[data$INSTNM=='University of Massachusetts-Dartmouth',]

##SAVING RESULTS##
write.csv(newdata, file = "UniversitiesMA.csv")
