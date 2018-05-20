#setwd('C:/Users/Javier/Documents/GitHub/FinalProject')

#Setting the working directory
setwd("/home/javier/Work/FinalProject")
getwd()

#We read the data
mydata = read.csv('DataClean.csv', header = TRUE, sep = ";")

#I had to change the format of the data, because some school
#names had commas within, which was giving us problems

#We create a data frame with the  data
df = data.frame(mydata)


#Names of the columns in the data frame
columns<-colnames(df)

#Array to store the percentage of nonempty cells
nemptyarr<-c()

for (i in 1:length(df)){
  empty = sum(df[,i]=='')
  filled = sum(df[,i]!='')
  
  if (is.na(empty)){
    empty = sum(is.na(df[,i]))
    filled = sum(!is.na(df[,i]))
  }
  
  #Now we calculate the percentage that is not empty
  nempty<-filled*100/(empty+filled)
  nempty<-as.integer(nempty)
  
  nemptyarr[i]<-nempty
  
  print(columns[i])
  print(nempty)
}

#We plot the results
names(nemptyarr)<-columns #To add the names to the bars
barplot(nemptyarr, col = 'blue', ylim=c(0,100), las = 2)

#Extracting the completion data from the dataframe
comp = df$Completion
tcomp<-table(comp)

#lbls <- c('Transfered or dropped','Completed')
lbls <- c('Incomplete','Completed')
pct <- round(tcomp/sum(tcomp)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels 

pie(tcomp, labels = lbls, col = c('Red','Green'), main = 'Student Completion')