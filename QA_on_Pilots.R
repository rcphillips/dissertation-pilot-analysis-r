#QA on pilot data (timing).R
#R.Phillips
#151305
#The goal here is to extract the total time per block from a single subject in the course of the TASK.
###Initializing Variables/Startup:
#from IRC I think:
setwd("C:/Users/rphillips/Box Sync/SRP_AXCPT_pilot_data")
#from home comp:
#setwd("C:/Users/ryphil/Box Sync/SRP_AXCPT_pilot_data")
last_trial_in_block<-matrix(nrow=5,ncol=1)
final_cues_in_min_minus_sum<-matrix(nrow=5,ncol=1)
allsubj_final_cues_in_min_minus_sum<-data.frame(blankcol=c(1:5))
###Pseudocode:
#load csv
#Find the column that contains time
#group it by block
#subtract beginning time from end time
###Actual Code Begins:
#loadcsv
i=23
for  (i in c(6,7,9,10,11,13,15,16,17,18,19,20,22,23,24,25,26,27,28,29,30,31,32)){
subjdata_name<- paste("subj", i, sep="","_task.csv")
subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
#find the final item in each block
for (j in 1:5){
last_trial_in_block[j]<-as.numeric(which(subjdata$BlockNum==j)[length(which(subjdata$BlockNum==j))])
}

#Find the column that contains time
final_cues_in_ms<-subjdata$Cue.OnsetTime[last_trial_in_block]
final_cues_in_s<-final_cues_in_ms/1000
final_cues_in_min<-final_cues_in_s/60
final_cues_in_min#this gives us the endpoint of each block, but we need to subtract
                 #the previous blocks, so we can see when the block begins
for (j in 1:5){
final_cues_in_min_minus_sum[j]<-final_cues_in_min[j]-sum(final_cues_in_min[j-1])
}
final_cues_in_min_minus_sum
allsubj_final_cues_in_min_minus_sum<-cbind(allsubj_final_cues_in_min_minus_sum,final_cues_in_min_minus_sum) #join with data from previous loops
}
which(allsubj_final_cues_in_min_minus_sum=="")
#9.91 --> like ten minutes. Not bad, really. Could just go with this. Need to get trialnum info, see how this compares across people.
#ah, but block 2 is 21 mins.... OHH I'm a doof. the blocks are added
#also need to make sure the optseq magic works out
table(subjdata$Subject, subjdata$BlockNum)



#group it by block

#subtract beginning time from end time
