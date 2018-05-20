#Setting the working directory
setwd("/home/javier/Work/FinalProject")
getwd()

#We read the data
require(gdata)
library(Hmisc)
library(stringi)
mydata = read.xls('IPE01.xls', sheet = 2, header = TRUE)

statecount = mydata$Areaname
statecount<-as.character(statecount)

avginc = mydata$IPE010209D

#COunties data
states<-c()
counties<-c()
incomes<-c()

#States data
ststates<-c()
stateinc<-c()

for (i in 1:length(statecount)){
  data = statecount[i]
  income = avginc[i]
  sepdata = strsplit(data,',')
  len = length(sepdata[[1]])
  if (len==2){
    sepdata<-unlist(sepdata)
    state = sepdata[2]
    state = gsub("[[:space:]]", "", state) 
    county = sepdata[1]
    county = gsub("[[:space:]]", "", county) 
    
    states<-c(states,state)
    counties<-c(counties,county)
    incomes<-c(incomes,income)
  }
  
  else if (len==1){
    sepdata<-unlist(sepdata)
    state<-sepdata
    if(state!="UNITED STATES" && state !="District of Columbia"){
      stateinc<-c(stateinc,income)
      ststates<-c(ststates,state) 
    }
  }
}

ststates<-tolower(ststates)
ststates<-stri_trans_totitle(ststates)
ststates<-state.abb[match(ststates,state.name)]

#District of columbia was giving us some problems
ststates[is.na(ststates)]<-"DC"

dfstates = data.frame(ststates,stateinc)

finaldf = data.frame(states,counties,incomes)


##SAVING RESULTS##
write.csv(dfstates, file = "IncomeStates.csv")
write.csv(finaldf, file = "Income.csv")