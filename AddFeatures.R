#Setting the working directory
#setwd("/home/javier/Work/FinalProject")
setwd('C:/Users/Javier/Documents/GitHub/FinalProject')

getwd()

##INITIALIZATION##

#Loading some libraries that we will need
library(gmapsdistance)

#We read the data
mydata = read.csv('DataClean.csv', header = TRUE, sep = ";")

#Income data by county
incdata = read.csv('Income.csv', header = TRUE)

#Income data by state
incstate = read.csv('IncomeStates.csv', header = TRUE)

#We get rid of the ID column, as it is not going to be
#useful for our classification task
mydata = subset(mydata, select = -c(ID))

#We convert the Admit term column from int to string
mydata$Admit_Term<-as.character(mydata$Admit_Term)

##WORKING WITH MISSING DATA##

#Replacing missig values of GPA with the mean gpa
mydata$hsgpa[is.na(mydata$hsgpa)]<-0

#Calculating the mean GPA (we exclude GPA values equal to 0)
gpa<-mydata$hsgpa[mydata$hsgpa!=0]
gpa<-gpa[gpa<=4]
m_gpa<-mean(gpa)

#Now we replace missing values of GPA with the mean GPA
mydata$hsgpa[mydata$hsgpa==0]<-m_gpa

#Replacing missing values of SATVB and SATMA with the mean values

#Calculate mean SATMA,SATVB
satma<-mydata$SATMA[mydata$SATMA!=0]
m_satma<-mean(satma)

satvb<-mydata$SATVB[mydata$SATVB!=0]
m_satvb<-mean(satvb)

#Now we replace missing values with the mean
mydata$SATMA[mydata$SATMA==0]<-m_satma
mydata$SATVB[mydata$SATVB==0]<-m_satvb

##ADDING FEATURES##

#Adding the column with distance in Km and distance driving from the
#students high school to UMass Dartmouth

cities<-levels(mydata$Schoolcity)

mydata$Distance<-NA
mydata$Distance_T<-NA

#cities<-cities[2:length(cities)]
#cities<-c(gsub(' ','+',cities))
#results<-gmapsdistance(origin = cities, destination = "Umass+Dartmouth", mode = 'driving')

#The first value is empty city so we skip it
for (i in 2:length(cities)){
  print(i)
  city<-cities[i]
  city_origin<-gsub(' ','+',city)
  print(city_origin)
  results<-gmapsdistance(origin = city_origin, destination = 'UMass+Dartmouth', mode = 'driving')
  dist_kms<-results$Distance/1000
  dist_hs<-results$Time/3600
  
  mydata$Distance[mydata$Schoolcity == city]<-dist_kms
  mydata$Distance_T[mydata$Schoolcity == city]<-dist_hs
  
}

#Commuter or not
mydata$Commuter<-ifelse(mydata$Distance_T>2/3, '1', '0')

#Adding the total SAT to the dataset
mydata$Total_SAT<-mydata$SATMA+mydata$SATVB

#Adding a column that tells us if a student is above
#or below the mean GPA and SAT
mydata$Above_GPA<-ifelse(mydata$hsgpa>m_gpa,'1','0')
mydata$Above_SAT<-ifelse(mydata$Total_SAT>(m_satma+m_satvb),'1','0')

#Adding a column that tell us if the student graduated
#with honors from high school or not
mydata$Honors<-ifelse(mydata$hsgpa>4,'1','0')

#Adding a column that classifies students depending on
#their origin
loc <- c()
adjstates<-c('NH','VT','NY','CT','RI') #States next to MA

for (i in 1:length(mydata$schoolstate)){
  cstate = mydata$schoolstate[i]
  ccounty = mydata$Schoolcounty[i]
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

mydata$Location<-loc

#Adding a column that tells us if a student was a direct admission or not
dadm<-c()

for (i in 1:length(mydata$ACAD_GROUP_A)){
  admprog<-mydata$ACAD_GROUP_A[i]
  if(admprog == 'NOW' | admprog == 'DCE' | admprog == 'PCE'){
    dadm<-c(dadm,'No')
  }
  else{
    dadm<-c(dadm,'Yes')
  }
}

mydata$D_Adm<-dadm

#Adding a column that tells us if a student changed his major or not
changed_major<-c()

for (i in 1:length(mydata$Admitted_Major)){
  adm<-as.character(mydata$Admitted_Major[i])
  curr<-as.character(mydata$Current_Major[i])
  
  #If we have missing data from the current major
  #We will supose the student is in the same major
  if(curr == ''){
    curr = adm
  }
  
  if(adm==curr){
    changed_major<-c(changed_major,'No')
  }
  else{
    changed_major<-c(changed_major,'Yes')
  }
}

mydata$Changed_Major<-changed_major

#Adding the percentage GPA for school, region, and state
mydata$P_GPA_School<-c()
mydata$P_GPA_Region<-c()
mydata$P_GPA_State<-c()

for (i in 1:length(mydata$hsgpa)){
  gpa<-mydata$hsgpa[i]
  
  school<-mydata$schoolname[i]
  state<-mydata$schoolstate[i]
  county<-mydata$Schoolcounty[i]
  
  #School
  maxschgpa<-max(mydata$hsgpa[mydata$schoolname==school])
  minschgpa<-min(mydata$hsgpa[mydata$schoolname==school])
  pgpasch<-(gpa-minschgpa)/(maxschgpa-minschgpa)
  if (minschgpa == maxschgpa){
    mydata$P_GPA_School[i]<-0.5
  }
  else{
    mydata$P_GPA_School[i]<-pgpasch
  }
  
  #Region
  maxreggpa<-max(mydata$hsgpa[mydata$Schoolcounty==county])
  minreggpa<-min(mydata$hsgpa[mydata$Schoolcounty==county])
  pgpareg<-(gpa-minreggpa)/(maxreggpa-minreggpa)
  if (minreggpa == maxreggpa){
    mydata$P_GPA_Region[i]<-0.5
  }
  else{
    mydata$P_GPA_Region[i]<-pgpareg
  }
  
  #State
  maxstgpa<-max(mydata$hsgpa[mydata$schoolstate==state])
  minstgpa<-min(mydata$hsgpa[mydata$schoolstate==state])
  pgpast<-(gpa-minstgpa)/(maxstgpa-minstgpa)
  if (minstgpa == maxstgpa){
    mydata$P_GPA_State[i]<-0.5
  }
  else{
    mydata$P_GPA_State[i]<-pgpast
  }
}

#Adding the percentage SATMA for school, region, and state
mydata$P_SATMA_School<-c()
mydata$P_SATMA_Region<-c()
mydata$P_SATMA_State<-c()

for (i in 1:length(mydata$SATMA)){
  satma<-mydata$SATMA[i]
  
  school<-mydata$schoolname[i]
  state<-mydata$schoolstate[i]
  county<-mydata$Schoolcounty[i]
  
  #School
  maxschsatma<-max(mydata$SATMA[mydata$schoolname==school])
  minschsatma<-min(mydata$SATMA[mydata$schoolname==school])
  psatmasch<-(satma-minschsatma)/(maxschsatma-minschsatma)
  if (minschsatma == maxschsatma){
    mydata$P_SATMA_School[i]<-0.5
  }
  else{
    mydata$P_SATMA_School[i]<-psatmasch
  }
  
  #Region
  maxregsatma<-max(mydata$SATMA[mydata$Schoolcounty==county])
  minregsatma<-min(mydata$SATMA[mydata$Schoolcounty==county])
  psatmareg<-(satma-minregsatma)/(maxregsatma-minregsatma)
  if (minregsatma == maxregsatma){
    mydata$P_SATMA_Region[i]<-0.5
  }
  else{
    mydata$P_SATMA_Region[i]<-psatmareg
  }
  
  #State
  maxstsatma<-max(mydata$SATMA[mydata$schoolstate==state])
  minstsatma<-min(mydata$SATMA[mydata$schoolstate==state])
  psatmast<-(satma-minstsatma)/(maxstsatma-minstsatma)
  if (minstsatma == maxstsatma){
    mydata$P_SATMA_State[i]<-0.5
  }
  else{
    mydata$P_SATMA_State[i]<-psatmast
  }
}

#Adding the percentage SATVB for school, region, and state
mydata$P_SATVB_School<-c()
mydata$P_SATVB_Region<-c()
mydata$P_SATVB_State<-c()

for (i in 1:length(mydata$SATVB)){
  satvb<-mydata$SATVB[i]
  
  school<-mydata$schoolname[i]
  state<-mydata$schoolstate[i]
  county<-mydata$Schoolcounty[i]
  
  #School
  maxschsatvb<-max(mydata$SATVB[mydata$schoolname==school])
  minschsatvb<-min(mydata$SATVB[mydata$schoolname==school])
  psatvbsch<-(satma-minschsatvb)/(maxschsatvb-minschsatvb)
  if (minschsatvb == maxschsatvb){
    mydata$P_SATVB_School[i]<-0.5
  }
  else{
    mydata$P_SATVB_School[i]<-psatvbsch
  }
  
  #Region
  maxregsatvb<-max(mydata$SATVB[mydata$Schoolcounty==county])
  minregsatvb<-min(mydata$SATVB[mydata$Schoolcounty==county])
  psatvbreg<-(satvb-minregsatvb)/(maxregsatvb-minregsatvb)
  if (minregsatvb == maxregsatvb){
    mydata$P_SATVB_Region[i]<-0.5
  }
  else{
    mydata$P_SATVB_Region[i]<-psatvbreg
  }
  
  #State
  maxstsatvb<-max(mydata$SATVB[mydata$schoolstate==state])
  minstsatvb<-min(mydata$SATVB[mydata$schoolstate==state])
  psatvbst<-(satvb-minstsatvb)/(maxstsatvb-minstsatvb)
  if (minstsatvb == maxstsatvb){
    mydata$P_SATVB_State[i]<-0.5
  }
  else{
    mydata$P_SATVB_State[i]<-psatvbst
  }
}

#Adding a column with each school's GPA and SAT
hschools<-levels(mydata$schoolname)
hschsat<-c()

#Adding the column to the data frame
mydata$School_GPA<-NA
mydata$School_SAT<-NA

for (i in 1:length(hschools)){
  schname<-hschools[i]

  avgGPA<-sum(mydata$hsgpa[mydata$schoolname==schname])/length(mydata$hsgpa[mydata$schoolname==schname])
  avgSAT<-sum(mydata$Total_SAT[mydata$schoolname==schname])/length(mydata$Total_SAT[mydata$schoolname==schname])

  hschsat<-c(hschsat, avgSAT)

  mydata$School_GPA[mydata$schoolname == schname]<-avgGPA
  mydata$School_SAT[mydata$schoolname == schname]<-avgSAT
 
}

#Adding a column with the ranking of the schools by SAT
hschools<-hschools[order(hschsat, decreasing = TRUE)]
hschsat<-sort(hschsat, decreasing = TRUE)

mydata$School_rank<-NA

for (i in 1:length(hschools)){
  school<-hschools[i]
  mydata$School_rank[mydata$schoolname==school]<-i
}

#Adding a Column that tells us if the student was above his 
#schools GPA or not
mydata$Above_HSGPA<-ifelse(mydata$hsgpa>mydata$School_GPA,'1','0')

#Adding a Column that tells us if the student was above his 
#schools SAT or not
mydata$Above_HSSAT<-ifelse(mydata$Total_SAT>mydata$School_SAT,'1','0')

#Adding the type of major
majors<-mydata$Admitted_Major
typelist<-c()

for (i in 1:length(majors)){
  major<-as.character(majors[i])
  type<-strsplit(major,"-")
  if (type=='COLNW' | type == 'START'){
    typelist<-c(typelist,type[[1]])
  }
  else{
    type<-type[[1:2]]
    typelist<-c(typelist,type)
  }
}

mydata$MajorType<-factor(typelist)

#Adding the performance group depending on the college

#Performance in different fields and different colleges
mydata$Perf_Eng_GPA<-rep(0,length(mydata$hsgpa))
mydata$Perf_Eng_SATMA<-rep(0,length(mydata$hsgpa))
mydata$Perf_Eng_SATVB<-rep(0,length(mydata$hsgpa))

mydata$Perf_Art_GPA<-rep(0,length(mydata$hsgpa))
mydata$Perf_Art_SATMA<-rep(0,length(mydata$hsgpa))
mydata$Perf_Art_SATVB<-rep(0,length(mydata$hsgpa))

mydata$Perf_Nur_GPA<-rep(0,length(mydata$hsgpa))
mydata$Perf_Nur_SATMA<-rep(0,length(mydata$hsgpa))
mydata$Perf_Nur_SATVB<-rep(0,length(mydata$hsgpa))

mydata$Perf_Bus_GPA<-rep(0,length(mydata$hsgpa))
mydata$Perf_Bus_SATMA<-rep(0,length(mydata$hsgpa))
mydata$Perf_Bus_SATVB<-rep(0,length(mydata$hsgpa))

mydata$Perf_Other_GPA<-rep(0,length(mydata$hsgpa))
mydata$Perf_Other_SATMA<-rep(0,length(mydata$hsgpa))
mydata$Perf_Other_SATVB<-rep(0,length(mydata$hsgpa))

#GPA
for (i in 1:length(mydata$P_GPA_School)){
  percgpa<-mydata$P_GPA_Region[i]
  college<-mydata$ACAD_GROUP_A[i]
  
  #Engineering
  if (college == 'CEGR'){
   if (percgpa>=0.7){
     mydata$Perf_Eng_GPA[i]<-"1"
   }
  
   else if (percgpa<0.7 && percgpa>0.3){
     mydata$Perf_Eng_GPA[i]<-"2"
   }
  
   else{
     mydata$Perf_Eng_GPA[i]<-"3"
   }
  }
  
  #Business
  else if (college == 'CCB'){
    if (percgpa>=0.7){
      mydata$Perf_Bus_GPA[i]<-"1"
    }
    
    else if (percgpa<0.7 && percgpa>0.3){
      mydata$Perf_Bus_GPA[i]<-"2"
    }
    
    else{
      mydata$Perf_Bus_GPA[i]<-"3"
    }
  }
  
  #Arts
  else if (college == 'CAS' | college == 'CVPA'){
    if (percgpa>=0.7){
      mydata$Perf_Art_GPA[i]<-"1"
    }
    
    else if (percgpa<0.7 && percgpa>0.3){
      mydata$Perf_Art_GPA[i]<-"2"
    }
    
    else{
      mydata$Perf_Art_GPA[i]<-"3"
    }
  }
  
  #Nursing
  else if (college == 'CNUR'){
    if (percgpa>=0.7){
      mydata$Perf_Nur_GPA[i]<-"1"
    }
    
    else if (percgpa<0.7 && percgpa>0.3){
      mydata$Perf_Nur_GPA[i]<-"2"
    }
    
    else{
      mydata$Perf_Nur_GPA[i]<-"3"
    }
  }
  
  #Other
  else{
    if (percgpa>=0.7){
      mydata$Perf_Other_GPA[i]<-"1"
    }
    
    else if (percgpa<0.7 && percgpa>0.3){
      mydata$Perf_Other_GPA[i]<-"2"
    }
    
    else{
      mydata$Perf_Other_GPA[i]<-"3"
    }
  }
}

#SATMA
for (i in 1:length(mydata$P_SATMA_School)){
  percsatma<-mydata$P_SATMA_Region[i]
  college<-mydata$ACAD_GROUP_A[i]
  #Engineering
  if (college == 'CEGR'){
    if (percsatma>=0.7){
      mydata$Perf_Eng_SATMA[i]<-"1"
    }
    
    else if (percsatma<0.7 && percsatma>0.3){
      mydata$Perf_Eng_SATMA[i]<-"2"
    }
    
    else{
      mydata$Perf_Eng_SATMA[i]<-"3"
    }
  }
  
  #Business
  else if (college == 'CCB'){
    if (percsatma>=0.7){
      mydata$Perf_Bus_SATMA[i]<-"1"
    }
    
    else if (percsatma<0.7 && percsatma>0.3){
      mydata$Perf_Bus_SATMA[i]<-"2"
    }
    
    else{
      mydata$Perf_Bus_SATMA[i]<-"3"
    }
  }
  
  #Arts
  else if (college == 'CAS' | college == 'CVPA'){
    if (percsatma>=0.7){
      mydata$Perf_Art_SATMA[i]<-"1"
    }
    
    else if (percsatma<0.7 && percsatma>0.3){
      mydata$Perf_Art_SATMA[i]<-"2"
    }
    
    else{
      mydata$Perf_Art_SATMA[i]<-"3"
    }
  }
  
  #Nursing
  else if (college == 'CNUR'){
    if (percsatma>=0.7){
      mydata$Perf_Nur_SATMA[i]<-"1"
    }
    
    else if (percsatma<0.7 && percsatma>0.3){
      mydata$Perf_Nur_SATMA[i]<-"2"
    }
    
    else{
      mydata$Perf_Nur_SATMA[i]<-"3"
    }
  }
  
  #Other
  else{
    if (percsatma>=0.7){
      mydata$Perf_Other_SATMA[i]<-"1"
    }
    
    else if (percsatma<0.7 && percsatma>0.3){
      mydata$Perf_Other_SATMA[i]<-"2"
    }
    else{
      mydata$Perf_Other_SATMA[i]<-"3"
    }
  }
}

#SATVB
for (i in 1:length(mydata$P_SATVB_School)){
  percsatvb<-mydata$P_SATVB_Region[i]
  college<-mydata$ACAD_GROUP_A[i]
  
  #Engineering
  if (college == 'CEGR'){
    if (percsatvb>=0.7){
      mydata$Perf_Eng_SATVB[i]<-"1"
    }
    
    else if (percsatvb<0.7 && percsatvb>0.3){
      mydata$Perf_Eng_SATVB[i]<-"2"
    }
    
    else{
      mydata$Perf_Eng_SATVB[i]<-"3"
    }
  }
  
  #Business
  else if (college == 'CCB'){
    if (percsatvb>=0.7){
      mydata$Perf_Bus_SATVB[i]<-"1"
    }
    
    else if (percsatvb<0.7 && percsatvb>0.3){
      mydata$Perf_Bus_SATVB[i]<-"2"
    }
    
    else{
      mydata$Perf_Bus_SATVB[i]<-"3"
    }
  }
  
  #Arts
  else if (college == 'CAS' | college == 'CVPA'){
    if (percsatvb>=0.7){
      mydata$Perf_Art_SATVB[i]<-"1"
    }
    
    else if (percsatvb<0.7 && percsatvb>0.3){
      mydata$Perf_Art_SATVB[i]<-"2"
    }
    
    else{
      mydata$Perf_Art_SATVB[i]<-"3"
    }
  }
  
  #Nursing
  else if (college == 'CNUR'){
    if (percsatvb>=0.7){
      mydata$Perf_Nur_SATVB[i]<-"1"
    }
    
    else if (percsatvb<0.7 && percsatvb>0.3){
      mydata$Perf_Nur_SATVB[i]<-"2"
    }
    
    else{
      mydata$Perf_Nur_SATVB[i]<-"3"
    }
  }
  
  #Other
  else{
    if (percsatvb>=0.7){
      mydata$Perf_Other_SATVB[i]<-"1"
    }
    
    else if (percsatvb<0.7 && percsatvb>0.3){
      mydata$Perf_Other_SATVB[i]<-"2"
    }
    
    else{
      mydata$Perf_Other_SATVB[i]<-"3"
    }
  }
}

#Adding income column
incdata$states<-as.character(incdata$states)
incdata$counties<-as.character(incdata$counties)

incstate$ststates<-as.character(incstate$ststates)

incarray<-c()

for (i in 1:length(mydata$Schoolcounty)){
  sstate = as.character(mydata$schoolstate[i])
  scounty = as.character(mydata$Schoolcounty[i])
  scounty = gsub("[[:space:]]", "", scounty) 

  if (sstate=="" && scounty==""){
    inc = 0
  }
  
  else if (sstate!="" && scounty==""){
    data = incstate[incstate$ststates == sstate,]
    inc = data$stateinc[1]
  }
  
  else{
    data = incdata[incdata$states == sstate,]
    data = data[data$counties == scounty,]
    inc = data$incomes[1]
    
    if (is.na(inc)){
      data = incstate[incstate$ststates == sstate,]
      inc = data$stateinc[1]
    }
  }
  
  if (is.na(inc)){
    inc = 0
  }
  
  incarray<-c(incarray,inc)
}


mydata$Income<-incarray
incno0<-mydata$Income[mydata$Income!=0]
mydata$Income[mydata$Income==0]<-mean(incno0)

##NORMALIZATION##

#HSGPA
mingpa<-min(mydata$hsgpa)
maxgpa<-max(mydata$hsgpa)

mydata$hsgpa<-(mydata$hsgpa-mingpa)/(maxgpa-mingpa)

#SATMA
minsma<-200
maxsma<-800

mydata$SATMA<-(mydata$SATMA-minsma)/(maxsma-minsma)

#SATVB
minsvb<-200
maxsvb<-800

mydata$SATVB<-(mydata$SATVB-minsvb)/(maxsma-minsvb)

#TOTALSAT
mints<-minsma+minsvb
maxts<-maxsma+maxsvb

mydata$Total_SAT<-(mydata$Total_SAT-mints)/(maxts-mints)
mydata$School_SAT<-(mydata$School_SAT-mints)/(maxts-mints)

#SCHOOL_GPA
minsch<-min(mydata$School_GPA)
maxsch<-max(mydata$School_GPA)

mydata$School_GPA<-(mydata$School_GPA-minsch)/(maxsch-minsch)

#DISTANCE
mindist<-min(mydata$Distance[!is.na(mydata$Distance)]) #We find the maximum and minimum of the values which are not NA
maxdist<-max(mydata$Distance[!is.na(mydata$Distance)])

mydata$Distance<-(mydata$Distance-mindist)/(maxdist-mindist)

#DISTANCE_T
mindistt<-min(mydata$Distance_T[!is.na(mydata$Distance_T)]) #Maximum and minimum of the values which are not NA
maxdistt<-max(mydata$Distance_T[!is.na(mydata$Distance_T)])

mydata$Distance_T<-(mydata$Distance_T-mindistt)/(maxdistt-mindistt)

#INCOME
maxinc<-min(mydata$Income)
mininc<-max(mydata$Income)
  
mydata$Income<-(mydata$Income-mininc)/(maxinc-mininc)

##SAVING RESULTS##
write.csv(mydata, file = "DataFeatures.csv")
