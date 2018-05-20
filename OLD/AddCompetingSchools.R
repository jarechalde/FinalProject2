#Loading libraries
library(stringr)
library(gmapsdistance)
library(RJSONIO)
library(geosphere)

#Setting the working directory
#setwd('C:/Users/Javier/Documents/GitHub/FinalProject')
setwd('/home/javier/Work/FinalProject')
getwd()

#We read the data
datau = read.csv('UniversitiesMA.csv', header = TRUE, sep = ",")
data = read.csv('DataFeatures.csv', header = TRUE, sep = ",")

#Function for getting coordinates
geocodeAdddress <- function(address) {
  require(RJSONIO)
  url <- "http://maps.google.com/maps/api/geocode/json?address="
  url <- URLencode(paste(url, address, "&sensor=false", sep = ""))
  x <- fromJSON(url, simplify = FALSE)
  if (x$status == "OK") {
    out <- c(x$results[[1]]$geometry$location$lng,
             x$results[[1]]$geometry$location$lat)
  } else {
    out <- NA
  }
  Sys.sleep(0.2)  # API only allows 5 requests per second
  out
}

hschools<-levels(data$schoolname)
hschools<-hschools[hschools!="#REF!"]

data$Longitude<-NA
data$Latitude<-NA

for (i in 2:length(hschools)){
  hschool<-hschools[i]
  hschoolmap<-gsub(' ','+',hschool)
  
  print(hschool)
  
  coord<-geocodeAdddress(hschoolmap)
  
  print(coord)
  
  data$Longitude[data$schoolname==hschool]<-coord[1]
  data$Latitude[data$schoolname==hschool]<-coord[2]
  
}

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

data$Competing_Schools<-NA

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
  } else if (type %in% PCIP11){
    schools<-datau[datau$PCIP11!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP13){
    schools<-datau[datau$PCIP05!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP1415){
    schools<-datau[which(datau$PCIP15!=0 | datau$PCIP14!=0),]
    listch<-schools$Schools
  } else if (type %in% PCIP16){
    schools<-datau[datau$PCIP16!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP23){
    schools<-datau[datau$PCIP23!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP26){
    schools<-datau[datau$PCIP26!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP27){
    schools<-datau[datau$PCIP27!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP30){
    schools<-datau[datau$PCIP30!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP38){
    schools<-datau[datau$PCIP38!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP40){
    schools<-datau[datau$PCIP40!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP42){
    schools<-datau[datau$PCIP42!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP45){
    schools<-datau[datau$PCIP45!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP50){
    schools<-datau[datau$PCIP50!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP51){
    schools<-datau[datau$PCIP51!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP52){
    schools<-datau[datau$PCIP52!=0,]
    listch<-schools$Schools
  } else if (type %in% PCIP54){
    schools<-datau[datau$PCIP54!=0,]
    listch<-schools$Schools
  } else {
    listch<-datau$Schools
  }
  
  dist_kms<-c()
  dist_hs<-c()
  
  
  
  #We need to get a key for the API
  for (j in 1:length(listch)){
    college<-listch[j]
    college<-gsub(' ','+',college)
    
    
    hschool<-gsub(' ','+',hschool)
    
    
    dist<-distm(hschoolcoord, c(40.6895, -74.1745), fun = distHaversine)
    
    dist_kms<-c(dist_kms,results$Distance/1000)
    
  }
  
  data$Competing_Schools[i]<-length(dist_kms[dist_kms<30])
  
}



##SAVING RESULTS##
write.csv(newdata, file = "DataFeatures_U.csv")