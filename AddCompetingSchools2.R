#Loading libraries
library(stringr)
library(gmapsdistance)
library(RJSONIO)
library(geosphere)
#devtools::install_github("dkahle/ggmap")
library(ggmap)

#Setting the working directory
#setwd('C:/Users/Javier/Documents/GitHub/FinalProject')
setwd('/home/javier/Work/FinalProject')
getwd()
options(digits = 12)

#We read the data
datau = read.csv('UniversitiesMA_Rank.csv', header = TRUE, sep = ",")
datau<-datau[datau$Schools!='University of Massachusetts-Dartmouth',]
data = read.csv('DataFeatures.csv', header = TRUE, sep = ",")

hschools<-levels(data$schoolname)
hschools<-hschools[hschools!="#REF!"]

datau$Rank<-ifelse(datau$Rank=='>15',16,as.numeric(as.character(datau$Rank)))

data$Longitude<-NA
data$Latitude<-NA

#Adding ggmaps api key
register_google(key = 'AIzaSyD-mC6_G2sFoghC3l4vTiiN9-SBKi8150Q')

for (i in 2:length(hschools)){
  hschool<-hschools[i]
  hschoolmap<-gsub(' ','+',hschool)
  
  print(hschool)
  
  coord<-geocode(hschoolmap)
  
  print(coord)
  
  data$Longitude[data$schoolname==hschool]<-coord[1]
  data$Latitude[data$schoolname==hschool]<-coord[2]
  
}

data$Longitude<-as.numeric(as.character(data$Longitude))
data$Latitude<-as.numeric(as.character(data$Latitude))

PCIP05<-c('WGS','WMS')
PCIP11<-c('CIS','CPE','DSC')
PCIP13<-c('AED')
PCIP1415<-c('CON','EET','EGR','ELE','ELEC','MET','MNE','BNG','CEN','BMB')
PCIP16<-c('FRN','GER','POR','SPA')
PCIP23<-c('ENL','ENLD','ENLL','ENLW')
PCIP26<-c('NUR','NURACC','NURN','BIO')
PCIP27<-c('MTH')
PCIP30<-c('MDS')
PCIP38<-c('PHL')
PCIP40<-c('PHY','CHM')
PCIP42<-c('PSY')
PCIP45<-c('HRM','HSS' ,'SHU','SHUP','SOASOC','PSC','CJS','SOC')
PCIP50<-c('LAP','LAR','LAU','VID','VIDC','VIDD','VIDE','VIDF','VIDG','VIDI',
          'VIDM','VIDP','VIDT','VIDV','VIDW','OPS','OPSC','OPSG','OPSO','TEC','TES','TET',
          'ARH','ART','ARTC','ARTF','ARTJ','ARTW','ASU','ASUN','ASUP','ATR','ATRC','ATRF',
          'ATRJ','ATRM','MTX','MTXB','MTXT','MUS','PAN','PRN','FIA','FOU','FOUD','FOUP','TDE','TDF','SCL')
PCIP51<-c('MLSB','MLSC','MLSH','MLSL','MDT','MLS')
PCIP52<-c('ACT','BSA','BSAN','BSP','GBA','MGT','MGTE','MGTL','MIS','MISE','MIST','MKT','BIS','MMT','BAD','FIN','ECO')
PCIP54<-c('HST')

### NOT CLASSIFIED YET ###
#COLNW 	College Now 
#START 	START Program 


majors<-data$Admitted_Major

#Adding number of competing schools and the choice to the dataset
data$Competing_Schools<-NA
data$Choice<-NA

for (i in 1:length(majors)){
  
  #So we can know in which iteration are we
  print(i)
  
  hschoolcoord<-c(data$Latitude[i],data$Longitude[i])
  
  #Getting the students high school
  hschool<-data$schoolname[i]
  
  #If we don't have the school name, we skip this loop iteration
  if (hschool==''){
    next
  }
  
  #Getting the students major
  major<-as.character(majors[i])
  type<-strsplit(major,"-")
  type<-as.list(type[[1]])
  type<-type[1]
  
  if (type %in% PCIP05){
    schools<-datau[datau$PCIP05!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP11){
    schools<-datau[datau$PCIP11!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP13){
    schools<-datau[datau$PCIP05!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP1415){
    schools<-datau[which(datau$PCIP15!=0 | datau$PCIP14!=0),]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP16){
    schools<-datau[datau$PCIP16!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP23){
    schools<-datau[datau$PCIP23!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP26){
    schools<-datau[datau$PCIP26!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP27){
    schools<-datau[datau$PCIP27!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP30){
    schools<-datau[datau$PCIP30!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP38){
    schools<-datau[datau$PCIP38!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP40){
    schools<-datau[datau$PCIP40!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP42){
    schools<-datau[datau$PCIP42!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP45){
    schools<-datau[datau$PCIP45!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP50){
    schools<-datau[datau$PCIP50!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP51){
    schools<-datau[datau$PCIP51!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP52){
    schools<-datau[datau$PCIP52!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else if (type %in% PCIP54){
    schools<-datau[datau$PCIP54!=0,]
    listch<-schools$Schools
    listrank<-schools$Rank
  } else {
    listch<-datau$Schools
    listrank<-datau$Rank
  }
  
  dist_kms<-c()
  
  #We need to get a key for the API
  for (j in 1:length(listch)){
    collegelat<-datau$Latitude[j]
    collegelat<-as.numeric(as.character(collegelat))
    collegelong<-datau$Longitude[j]
    collegelong<-as.numeric(as.character(collegelong))
    
    dist<-distm(c(as.numeric(as.character(hschoolcoord[2])),as.numeric(as.character(hschoolcoord[1]))), c(collegelong,collegelat), fun = distHaversine)
    #print(dist/1000)
    dist_kms<-c(dist_kms,dist/1000)
    
  }
  
  cond<-sapply(dist_kms, function(x) x<30)
  listrank<-listrank[cond]
  rank<-listrank[listrank<15]
  data$Competing_Schools[i]<-length(dist_kms[dist_kms<30])
  data$Choice[i]<-length(rank)
}



##SAVING RESULTS##
write.csv(data, file = "DataFeatures_U.csv")