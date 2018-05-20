#Loading libraries
library(stringr)
<<<<<<< HEAD
library(gmapsdistance)
=======
>>>>>>> 01efeb49f4debac3247bedbcf37c496888bc0656

#Setting the working directory
#setwd('C:/Users/Javier/Documents/GitHub/FinalProject')
setwd('/home/javier/Work/FinalProject')
getwd()

#We read the data
data = read.csv('Universities_US.csv', header = TRUE, sep = ",")
<<<<<<< HEAD
datafeat = read.csv('DataFeatures.csv', header = TRUE, sep = ",")

#Select universities from MA
data<-data[data$Institution_State=='MA',]

#Getting the different school names
Schools<-unique(data$Institution_Name)
=======

#Select universities from MA

data<-data[data$Institution_State=='MA',]

Schools<-unique(data$Institution_Name)

>>>>>>> 01efeb49f4debac3247bedbcf37c496888bc0656
Schools<-sort(Schools)

#Creating the new dataframe with the schools from MA
newdata<-data.frame(Schools)
newdata$Street_Address<-NA
newdata$School_City<-NA

<<<<<<< HEAD
for (i in 1:length(Schools)){
  name<-as.character(Schools[i])
=======
for (i in 1:length(names)){
  name<-as.character(names[i])
>>>>>>> 01efeb49f4debac3247bedbcf37c496888bc0656
  
  #Getting the data from that school
  datasch<-data[data$Institution_Name==name,]
  
  s_address<-as.character(datasch$Institution_Address)
  city<-as.character(datasch$Institution_City)
  
  newdata$Street_Address[newdata$Schools == name]<-s_address
  newdata$School_City[newdata$Schools == name]<-city
}

newdata$Schools<-factor(newdata$Schools)

programs<-unique(data$Program_Name)

listp<-c()

for (i in 1:length(programs)){
<<<<<<< HEAD
=======
  print(program)
>>>>>>> 01efeb49f4debac3247bedbcf37c496888bc0656
  program<-as.character(programs[i])
  string<-as.list(strsplit(program, ' ')[[1]])
  hola<-grep('\\((.*?)\\)', string, value = T)
  listp<-c(listp,hola)
}

listp<-unique(listp)
listp<-sort(listp)
<<<<<<< HEAD

#Adding the number of universities in the area
hschools<-datafeat$schoolname

datafeat$Compet_Colleges<-NA

for (i in 1:10){
  hschool<-hschools[i]
  hschool<-gsub(' ','+',hschool)
  schooldist<-c()
  for (j in 1:length(newdata$Schools)){
    
    #Getting each college address
    collegeadd<-newdata$Street_Address[i]
    collegecity<-newdata$School_City[i]
    college<-paste(collegeadd,collegecity)
    college<-gsub(' ','+',college)
    
    results<-gmapsdistance(origin = hschool, destination = college, mode = 'driving')
    dist_kms<-results$Distance/1000
    dist_hs<-results$Time/3600
    schooldist<-c(schooldist,dist_hs)
  }
}

##SAVING RESULTS##
write.csv(datafeat, file = "DataFeatures_U.csv")
=======
>>>>>>> 01efeb49f4debac3247bedbcf37c496888bc0656
