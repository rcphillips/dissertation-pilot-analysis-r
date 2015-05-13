#QA on pilot data (timing).R
#R.Phillips
#151305
#The goal here is to extract the total time per block from a single subject in the course of the TASK.
###Initializing Variables/Startup:
#from IRC I think:
#setwd("C:/Users/rphillips/Box Sync/SRP_AXCPT_pilot_data")
#from home comp:
setwd("C:/Users/ryphil/Box Sync/SRP_AXCPT_pilot_data")
###Pseudocode:
#load csv
#Find the column that contains time
#group it by block
#subtract beginning time from end time
###Actual Code Begins:
#loadcsv
i=23
subjdata_name<- paste("subj", i, sep="","_task.csv")
subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
#Find the column that contains time
head(subjdata$BlockNum)
which(subjdata$BlockNum==1)
#final row which contains blocknum == 1 is 51
subjdata[51,1:5]
subjdata[51,35:40]
final_cue_in_ms<-subjdata$Cue.OnsetTime[103]#this is the cue onset of the last trial of the first block
final_cue_in_s<-final_cue_in_ms/1000
final_cue_in_min<-final_cue_in_s/60
final_cue_in_min
#9.91 --> like ten minutes. Not bad, really. Could just go with this. Need to get trialnum info, see how this compares across people.
#ah, but block 2 is 21 mins....
#also need to make sure the optseq magic works out
table(subjdata$Subject, subjdata$BlockNum)



#group it by block

#subtract beginning time from end time
