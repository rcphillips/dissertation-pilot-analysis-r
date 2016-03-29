#VectorQA
#R.Phillips
#Goal here is to get the cue onsets by type and SRP level to compare against
#vectors
setwd("C:/Users/rphillips/Desktop/complete_csvs/complete_csvs")
subjdata<-read.csv('srp_01_all.csv')
times<-data.frame('SRP_Rating' = subjdata$SRP_Rating)
times$type<-subjdata$TrialType
times$Cue.OnsetTime<-subjdata$Cue.OnsetTime
times$CueTimeinSec<-((subjdata$Cue.OnsetTime-subjdata$Cue.OnsetTime[1])/1000)
highAX<-subset(times, times$type=="AX" & times$SRP_Rating==2)
highAX$CueTimeinSec
