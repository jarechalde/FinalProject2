#Loading libraries
library(fastAdaboost)
library(caret)
library(adabag)

#Setting the working directory
#setwd('C:/Users/Javier/Documents/GitHub/FinalProject')
setwd('/home/javier/Work/FinalProject')
getwd()

#We read the data
data = read.csv('DataFeatures.csv', header = TRUE, sep = ",")

#Adding column that says if the student is from Bristol county or not
data$Bristol<-ifelse(data$Schoolcounty == 'Bristol', 'Yes', 'No')

#Try this out
maxrank<-max(as.integer(data$School_rank))
for (i in 1:length(data$School_rank)){
  #print('----------')
  rank<-as.integer(data$School_rank[i])
  #print(rank)
  rank<-(rank-1)/(maxrank-1)
  #print(rank)
  if(rank<=3/10){
    data$School_rank[i]<-'1'
  }
  
  else if(rank>3/10 && rank<7/10){
    data$School_rank[i]<-'2'
  }
  
  else{
    data$School_rank[i]<-'3'
  }
  
}

#Fixing data
data<-data[data$Admit_Term<=2011,]

data$Admit_Term<-factor(as.character(data$Admit_Term))
data$Location<-factor(as.character(data$Location))
data$Honors<-factor(as.character(data$Honors))
data$School_rank<-factor(as.character(data$School_rank))

data$Above_GPA<-factor(as.character(data$Above_GPA))
data$Above_SAT<-factor(as.character(data$Above_SAT))
data$Above_HSGPA<-factor(as.character(data$Above_HSGPA))
data$Above_HSSAT<-factor(as.character(data$Above_HSSAT))

data$Perf_Eng_GPA<-factor(as.character(data$Perf_Eng_GPA))
data$Perf_Eng_SATMA<-factor(as.character(data$Perf_Eng_SATMA))
data$Perf_Eng_SATVB<-factor(as.character(data$Perf_Eng_SATVB))

data$Perf_Art_GPA<-factor(as.character(data$Perf_Art_GPA))
data$Perf_Art_SATMA<-factor(as.character(data$Perf_Art_SATMA))
data$Perf_Art_SATVB<-factor(as.character(data$Perf_Art_SATVB))

data$Perf_Nur_GPA<-factor(as.character(data$Perf_Nur_GPA))
data$Perf_Nur_SATMA<-factor(as.character(data$Perf_Nur_SATMA))
data$Perf_Nur_SATVB<-factor(as.character(data$Perf_Nur_SATVB))

data$Perf_Bus_GPA<-factor(as.character(data$Perf_Bus_GPA))
data$Perf_Bus_SATMA<-factor(as.character(data$Perf_Bus_SATMA))
data$Perf_Bus_SATVB<-factor(as.character(data$Perf_Bus_SATVB))

data$Perf_Other_GPA<-factor(as.character(data$Perf_Other_GPA))
data$Perf_Other_SATMA<-factor(as.character(data$Perf_Other_SATMA))
data$Perf_Other_SATVB<-factor(as.character(data$Perf_Other_SATVB))

#Partitioning into training and test set
partition<-sample(2,nrow(data), replace = T, prob=c(0.7,0.3))
train<-data[partition==1,]
test<-data[partition==2,]

#Training the model
myada<-adaboost(Completion~
                  schoolname+Schoolcounty+schoolstate+ #Data from the school
                  Gender+hsgpa+SATMA+SATVB+Total_SAT+ #Data from the student
                  ACAD_GROUP_A+Admitted_Major+MajorType+Changed_Major+ #Major information
                  Above_GPA+Above_SAT+Above_HSGPA+Above_HSSAT+
                  Honors+Location+D_Adm+Bristol+
                  School_rank+School_SAT+School_GPA+
                  Income+
                  Distance+Distance_T#+#Commuter#+ #Distance in time and kms
                  #P_GPA_Region+P_GPA_State+P_GPA_School+
                  #P_SATMA_Region+P_SATMA_State+P_SATMA_School+
                  #P_SATVB_Region+P_SATVB_State+P_SATVB_School+
                  #Perf_Eng_GPA+Perf_Art_GPA+Perf_Nur_GPA+Perf_Bus_GPA+Perf_Other_GPA+
                  #Perf_Eng_SATMA+Perf_Art_SATMA+Perf_Nur_SATMA+Perf_Bus_SATMA+Perf_Other_SATMA+
                  #Perf_Eng_SATVB+Perf_Art_SATVB+Perf_Nur_SATVB+Perf_Bus_SATVB+Perf_Other_SATVB
                  , data = train, 10)

#Prediction
pred<-predict(myada,test)

#Confusion Matrix
cmat<-confusionMatrix(pred$class,test$Completion)

#Printing Confusion Matrix
print(cmat)
