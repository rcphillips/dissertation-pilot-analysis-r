#SRP-AXCPT
#IndivSubjectAnalysis
#R.Phillips
#150512
#Trying something different this time. Rather than adding things onto an
#increasingly cumbersome script, I'm just going to bring in the pieces from the
#other script (PSS_to_task_plotter (merged).R) which are important for the output, and
#keep it in the same working directory.
###
#for a single subject:
#Read csv
#plot RT on x axis
#plot count on y axis
#join with SRP info
#plot divided by sig
#repeat for all subjects on the same plot
###
setwd("~/Box Sync/SRP_AXCPT_pilot_data")
#subjno goes here:
i = 8
#Read csv
subjpss_name<- paste("subj", i, sep="","_pss.csv")
subjpss<-read.csv(subjpss_name,stringsAsFactors=FALSE)
#load and name the csv for RTs
subjdata_name<- paste("subj", i, sep="","_task.csv")
subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
#Remove inaccurate trials from the subjdata. This part of the task is just concerned with
#correct trials/partial lapses. Incorrect trials are dealt with elsewhere.
subjdata<-subset(subjdata, subjdata$Cue.ACC=="1" & subjdata$Probe.ACC=="1")
#plot RT on x axis
hist(subjdata$Probe.RT, breaks = 100, ylim=c(0,5))
#plot count on y axis
#join with SRP info
#plot divided by sig
#repeat for all subjects on the same plot