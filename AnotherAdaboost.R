#Loading libraries
library(adabag)
library(caret)

#Setting the working directory
#setwd("/home/javier/Work/FinalProject")
#setwd('C:/Users/jarec/Documents/GitHub/FinalProject')

setwd('/home/javier/Work/FinalProject')
getwd()

#We read the data
data = read.csv('DataFeatures.csv', header = TRUE, sep = ",")

#Correct some issues
data$Admit_Term<-as.character(data$Admit_Term)
data$Location<-as.character(data$Location)
data$School_rank<-as.character(data$School_rank)

#Try this out
for (i in 1:length(data$School_rank)){
  rank<-as.integer(data$School_rank[i])
  if(rank>=rank/10){
    data$School_rank[i]<-"1"
  }
  
  if(rank<rank/10){
    data$School_rank[i]<-"2"
  }
  
}

#Partitioning into training and test set
partition<-sample(2,nrow(data), replace = T, prob=c(0.7,0.3))
train<-data[partition==1,]
test<-data[partition==2,]

data$Completion<-as.factor(data$Completion)
train$Completion<-as.factor(train$Completion)

#Training the model
myada<-boosting(Completion~Gender+Admit_Term+Admitted_Semester+schoolname+SATMA+SATVB+
                  Location+Honors+ACAD_GROUP_A+Admitted_Major+Degree_Major+
                  School_rank+D_Adm, data = train,
                boos = TRUE, mfinal = 10)

#Cross validation
myadacv<-boosting.cv(Completion~Gender+Admit_Term+Admitted_Semester+schoolname+
                  Location+Honors+ACAD_GROUP_A+Admitted_Major+Degree_Major+
                  Changed_Major+School_rank+D_Adm, data, v = 4, boos = TRUE, mfinal = 10,
                  coeflearn = 'Breiman')

#Prediction
pred<-predict(myada,test)

#Confusion Matrix
cmat<-confusionMatrix(pred$class,test$Completion)

#Printing Confusion Matrix
print(cmat)
