#SRP-AXCPT
#IndivSubjectAnalysis
#R.Phillips
#150512
#Trying something different this time. Rather than adding things onto an
#increasingly cumbersome script, I'm just going to bring in the pieces from the
#other script (PSS_to_task_plotter (merged).R) which are important for the output, and
#keep it in the same working directory.
###pseudocode:
#for a single subject:
#Read csv
#plot RT on x axis
#plot count on y axis
#add grouping variable
#
#repeat for all subjects on the same plot
###housekeeping- setting working directory, initializing variables
#setwd:
#for IRC comp
setwd("C:/Users/rphillips/Box Sync/SRP_AXCPT_pilot_data")
#for CNS comp
#setwd("~/Box Sync/SRP_AXCPT_pilot_data")
#Initializing variables
allsubjdata<-data.frame(NULL)
###actual code begins
#extract the data for all subjects
for  (i in c(6,7,9,10,11,13,15,16,17,18,19,20,22)){
#Read csv
subjpss_name<- paste("subj", i, sep="","_pss.csv")
subjpss<-read.csv(subjpss_name,stringsAsFactors=FALSE)
#load and name the csv for RTs
subjdata_name<- paste("subj", i, sep="","_task.csv")
subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
#Remove inaccurate trials from the subjdata. This part of the task is just concerned with
#correct trials/partial lapses. Incorrect trials are dealt with elsewhere.
subjdata<-subset(subjdata, subjdata$Cue.ACC=="1" & subjdata$Probe.ACC=="1")
#bring all subjects into same data frame
subjdata[length(subjdata)+1]<-i # give a group variable
allsubjdata<-rbind(allsubjdata,subjdata) #join with data from previous loops
}#end group processing
#plot RT on x axis
#plot count on y axis
hist(subjdata$Probe.RT, breaks = 100, ylim=c(0,5))
#plot divided by sig

