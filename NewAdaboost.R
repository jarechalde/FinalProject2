#Loading libraries
library(fastAdaboost)
library(caret)
library(adabag)

#Setting the working directory
#setwd("/home/javier/Work/FinalProject")
setwd('/home/javier/Work/FinalProject')
getwd()

#We read the data
data = read.csv('NewData.csv', header = TRUE, sep = ",")

#Correcting some issues
data$field<-factor(as.character(data$field))

#Adding a column that classifies students depending on
#their origin
loc <- c()
adjstates<-c('NH','VT','NY','CT','RI') #States next to MA

for (i in 1:length(data$pstate)){
  cstate = data$pstate[i]
  ccounty = data$County_US[i]
  if(cstate=='MA'){
    if(ccounty == 'Bristol'){
      loc<-c(loc,'0')
    }
    else{
      loc<-c(loc,'1')
    }
  }
  else if(cstate==''){ 
    #If we don't have any state data, we suppose the 
    #student came from out of the US
    loc<-c(loc,'4')
  }
  else if(cstate!='MA'){
    if(cstate %in% adjstates){
      loc<-c(loc,'2')
    }
    else{
      loc<-c(loc,'3')
    }
  }
}

data$Location<-loc
data$Location<-factor(data$Location)


data$Completion<-NA

data$Completion<-ifelse(data$grad17 == 1,1,0)

data$Housing<-ifelse(data$Housing == 1,1,0)

#Honors high school
data$HS_Honors<-ifelse(data$hsgpa>=4,1,0)

#We create a column that indicates if the student is progressing or not
data$Progress<-ifelse(data$class12 == 'SO',1,0)

#Adding a column to see if the student is from Bristol or not
data$Bristol<-ifelse(data$County_US == 'Bristol', 1,0)

#Partitioning into training and test set
partition<-sample(2,nrow(data), replace = T, prob=c(0.6,0.4))
train<-data[partition==1,]
test<-data[partition==2,]

#Training the model
myada<-adaboost(Completion~military+MILITARYstatus+
                  pcntry+pcity+County_US+pstate+Location+ #Location information
                  sex+AGE+
                  fte+ #Full time equivalence
                  #ETHNIC_STATE+ETHNIC_FED+ #Ethnicity
                  college+major1+field+ # Studies information
                  FIRSTGEN_HS+FIRSTGEN_FED+#Bristol+
                  indorms+#dormcd+ #Dorm Information
                  satma+satvb+satwr+totsat+HS_Honors+visa
                , data = train, 500)

#dormcd s lowering the performance

#Prediction
pred<-predict(myada,test)

#Confusion Matrix
cmat<-confusionMatrix(pred$class,test$Completion)

#Printing Confusion Matrix
print(cmat)
