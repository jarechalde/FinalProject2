from csv import reader

file = open('/home/javier/Work/FinalProject/2000-2015.csv','r')

filel = file.readlines()

clean = open('/home/javier/Work/FinalProject/DataClean.csv','w')

#Initialize the header
header = 0

for line in reader(filel):
 #This is the data that will be useful to us
 data = line[0:20]
 outline = '' 

 
 #If we are writing the header, we wont need to clean the data
 if header == 0:
  header = 1 
  for i in range(0,20):
   if i==0:
    outline = outline + data[i]
   else:
    outline = outline + ';' + data[i]
  #Once we have the header ready, we will write into the file
  clean.write(outline+'\n')

 else:
  #Extracting variables from the data
  yearad = data[0]
  termad = data[1]
  stid = data[2]
  stdgender = data[3]
  hschool = data[4]
  schcity = data[5]
  schcounty = data[6]
  schstate = data[7]
  hsgpa = data[8]
  satma = data[9]
  satvb = data[10]
  acada= data[11]
  admajor = data[12]
  acadc = data[13]
  currmajor = data[14]
  acadd = data[15]
  degmajor = data[16]
  compterm = data[17]
  compsem = data[18]
  comp = data[19]  
  
  #Converting the gpa to a float
  gpa = float(hsgpa)

  #Cleaning GPA
  #if gpa>4:
   #hsgpa = '4.0'
  if gpa == 0:
   hsgpa = ''
 
  #Checking is gender is available in the samples
  if stdgender!='Male' and stdgender!='Female':
   print(stdgender)

  #Cleaning completion
  if comp!='0' and comp!='1':
   print(line)
   print(comp)
   #If there is a 0 in the string, we will set it to 0
   if '0' in comp:
    comp = '0'
   #Else if there is a 1 we set it to 1
   if '1' in comp:
    comp = '1'
   #If we cant find it we will skip this row
   else:
    continue

  cleandata  = [yearad,termad,stid,stdgender,hschool,schcity,schcounty,schstate,hsgpa,satma,satvb,acada,admajor,acadc,currmajor,acadd,degmajor,compterm,compsem,comp]
 
  #We only going to use 20 first attributes
  for i in range(0,20):
   if i == 0:
    outline = outline+cleandata[i]
   else:
    outline = outline +';'+ cleandata[i]

  #Write the data to the file
  clean.write(outline+'\n')

#Maybe will be a good idea to create columns like if the student is above and below average GPA

#Close the files
file.close()
clean.close() 
