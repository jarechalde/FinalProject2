#Loading libraries
library(e1071)
library(caret)

#Setting the working directory
#setwd('C:/Users/Javier/Documents/GitHub/FinalProject')
setwd('/home/javier/Work/FinalProject')
getwd()

#We read the data
oldata = read.csv('DataFeatures.csv', header = TRUE, sep = ",")
#data<-data[data$Admit_Term==2015,]

data = data.frame("Admit_Term" = oldata$Admit_Term, "Gender" = oldata$Gender,"schoolname" = oldata$schoolname,
                  "Location" = oldata$Location, "Honors" = oldata$Honors,"School_rank" = oldata$School_rank, 
                  "SATMA" = oldata$SATMA, "SATVB" = oldata$SATVB,"Total_SAT" = oldata$Total_SAT,"Honors" = oldata$Honors,
                  "hsgpa" = oldata$hsgpa,"Completion" = oldata$Completion,"School_GPA" = oldata$School_GPA,
                  "ACAD_GROUP_A" = oldata$ACAD_GROUP_A, "schoolstate" = oldata$schoolstate, "School_SAT" = oldata$School_SAT,
                  "Admitted_Major" = oldata$Admitted_Major,"Above_GPA" = oldata$Above_GPA, "Above_SAT" = oldata$Above_SAT,
                  "MajorType" = oldata$MajorType)

#Fixing data
data$Admit_Term<-as.character(data$Admit_Term)
data$Location<-as.character(data$Location)
data$Honors<-as.character(data$Honors)
data$School_rank<-as.character(data$School_rank)

#Factoring the data
data$Location<-factor(data$Location)
data$School_rank<-factor(data$School_rank)
data$Completion<-factor(data$Completion)
data$ACAD_GROUP_A<-factor(data$ACAD_GROUP_A)

#Try this out
maxrank<-max(as.integer(data$School_rank))
for (i in 1:length(data$School_rank)){
  rank<-as.integer(data$School_rank[i])
  if(rank>=maxrank*3/4){
    data$School_rank[i]<-'1'
  }
  
  else if(rank>maxrank*2/4){
    data$School_rank[i]<-'2'
  }
  
  else if(rank>maxrank*1/4){
    data$School_rank[i]<-'3'
  }
  
  else{
    data$School_rank[i]<-'4'
  }
  
}

#Partitioning into training and test set
partition<-sample(2,nrow(data), replace = T, prob=c(0.7,0.3))
train<-data[partition==1,]
test<-data[partition==2,]

#Training the model
mysvm<-svm(Completion~Gender+hsgpa+SATMA+SATVB+Total_SAT+Location+Above_GPA+Above_SAT+
             ACAD_GROUP_A+School_GPA+School_SAT+Honors+MajorType, data = train, kernel = 'sigmoid')

#Prediction
predsvm<-predict(mysvm,test)
# 
# mynames<-names(predsvm)
# 
# pred<-c()
# 
# for (i in 1:length(mynames)){
#   index<-mynames[i]
#   predvalue<-predsvm[index]
#   if (predvalue>=0.5){
#     predvalue = 1
#     pred<-c(pred,predvalue)
#   }
#   
#   else{
#     predvalue = 0
#     pred<-c(pred,predvalue)
#   }
#   
# }

#Confusion Matrix
cmat<-confusionMatrix(predsvm,test$Completion)

#Printing Confusion Matrix
print(cmat)
