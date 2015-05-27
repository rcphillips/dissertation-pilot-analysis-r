#Getting Full Lapse numbers
#R.Phillips
#As simple as it sounds, just pulling out how many full lapses each subject has
###housekeeping- setting working directory, initializing variables
#setwd:
#for IRC comp
#setwd("C:/Users/rphillips/Box Sync/SRP_AXCPT_pilot_data")
#for CNS comp
setwd("~/Box Sync/SRP_AXCPT_pilot_data")
#for home comp
#setwd("E:/Box Sync/Box Sync/SRP_AXCPT_pilot_data")
trial_type_of_interest <- "AX"
result<-data.frame(NULL)
subjno<-c(6,7,10,11,15, 16,17,19,20,22,24,25,26,27,28,30,31,32)
all_inc<-data.frame(NULL)
###actual code begins
#extract the data for all subjects
i=25
for  (i in c(6,7,10,11,15, 16,17,19,20,22,24,25,26,27,28,30,31,32)){
  #load and name the csv for RTs
  subjdata_name<- paste("subj", i, sep="","_task.csv")
  subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
  subjdata<-subset(subjdata, subjdata$Cue.ACC=="1" & subjdata$Probe.ACC=="0" & subjdata$TrialType==trial_type_of_interest)
  inc<-length(subjdata$ExperimentName)
  
  all_inc<-rbind(all_inc,inc)
}
result<-cbind(subjno,all_inc)
rownames(result)<-subjno
colnames(result)<-c("subjno","incorrect")
#result<-subset(result, select = incorrect)
result

library(ggplot2)
p<-ggplot(result, aes(x=subjno,y=incorrect)) + geom_point() +
  geom_hline(yintercept=5)

p
