#ex-gaussian modelling for SRP-AXCPT
#R. Phillips
#150716
#The goal here is to classify partial lapses using the Heathcote method as put forth by Fassbender,
#and as implemented by the retimes package
###Steps:
#Bring in a single subject's RT distribution

#Bring in a single subject's PSR info

#split the data by SRP

#apply the timefit method to each class of SRP

#save the tau values from each of those timefits

#apply tau cutoff for each class of SRP

#quantify  partial lapses for each class of SRP

#repeat for all subjects

###
###Housekeeping:
#IRC
#setwd("C:/Users/rphillips/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
setwd("C:/Users/rphillips/Desktop/complete_csvs/complete_csvs")
#CNS
#setwd("~/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
#home
#setwd("E:/Box Sync/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
library(ggplot2)
library(retimes)
allsubjno<-data.frame(NULL)
allsubj_high_plapse<-data.frame(NULL)
allsubj_low_plapse<-data.frame(NULL)
allsubj_high_tau<-data.frame(NULL)
allsubj_low_tau<-data.frame(NULL)
###
#test case:
subjno=10
#The function:
partial_lapses<-function(subjno){
#Bring in a single subject's RT distribution
subjtask_name<- paste("srp_", subjno, sep="","_task.csv")
subjtask<-read.csv(subjtask_name,stringsAsFactors=FALSE)
#remove incorrect trials
subjtask<-subset(subjtask, subjtask$Cue.ACC=="1" & subjtask$Probe.ACC=="1")
#Bring in a single subject's PSR info
subjpsr_name<- paste("srp_", subjno, sep="","_psr.csv")
subjpsr<-read.csv(subjpsr_name,stringsAsFactors=FALSE)

#remove null columns
subjpsr<-subset(subjpsr[
  which(subjpsr$Word!=""),])
subjpsr<-subset(subjpsr[
  which(subjpsr$Word!="NULL"),])

subj_psr_score<-matrix(nrow=length(subjtask$Word), ncol=1)
for (k in 1:length(subjtask$DisplayStr)){
  #this tries to match each psr word with a task trial, and then saves the RT, trial type, and psr score.
  subj_psr_score[k]<-subjpsr$WordPresentation.RESP[match(as.character(
    subjtask$DisplayStr[k]), subjpsr$Word)]
  #as well as the word being matched with (subjtask[k]), this is maintained IN subjtask order and
  #can be cbound directly with the subjtask file.
}

subjtask$srp_rating<-subj_psr_score
subjtask$srp_class<-subj_psr_score
subjtask$srp_class[which(subjtask$srp_rating<5)] = "low"
subjtask$srp_class[which(subjtask$srp_rating>=5)] = "high"

#subset out BX and AY trials
subjtask<-subset(subjtask, subjtask$TrialType=="AX")
#break taskpsr out into high and low SRP
subj_highSRP<-subset(subjtask, subjtask$srp_class=="high")
subj_lowSRP<-subset(subjtask, subjtask$srp_class=="low")
#apply the timefit method to each class of SRP
high_exg<-timefit(x=as.numeric(subj_highSRP$Cue.RT), iter = 0, size = length(subj_highSRP$Cue.RT),
                  replace = TRUE, plot = TRUE, start = NULL)
high_tau<-high_exg@par[3]
low_exg<-timefit(x=as.numeric(subj_lowSRP$Cue.RT), iter = 0, size = length(subj_lowSRP$Cue.RT),
                 replace = TRUE, plot = TRUE, start = NULL)
low_tau<-low_exg@par[3]
#apply tau cutoff for each class of SRP
high_plapse_trials<-subset(subj_highSRP, subj_highSRP$subj_RT>(high_tau*4.5))
low_plapse_trials<-subset(subj_lowSRP, subj_lowSRP$subj_RT>(low_tau*4.5))
#quantify  partial lapses for each class of SRP
high_plapse<-dim(high_plapse_trials)[1]
low_plapse<-dim(low_plapse_trials)[1]
high_plapse_rate<-(dim(high_plapse_trials)[1]/dim(subj_highSRP)[1])
low_plapse_rate<-(dim(low_plapse_trials)[1]/dim(subj_lowSRP)[1])

result<-cbind(high_plapse,low_plapse,high_tau,low_tau)
return(result)}

#Looping through all subjects
allsubjno<-data.frame(NULL)
allsubj_high_plapse<-data.frame(NULL)
allsubj_low_plapse<-data.frame(NULL)
allsubj_high_tau<-data.frame(NULL)
allsubj_low_tau<-data.frame(NULL)
for (i in c('01','02','03','04','05','06','07','08','09',10:15,17:22,24:26,29:33)){
#extract
subjno <- as.numeric(i)
  subj_high_plapse<-partial_lapses(i)[1]
  subj_low_plapse<-partial_lapses(i)[2]
  subj_high_tau<-partial_lapses(i)[3]
  subj_low_tau<-partial_lapses(i)[4]
  #link
  allsubjno<-rbind(allsubjno,subjno)
  allsubj_high_plapse<-rbind(allsubj_high_plapse,subj_high_plapse)
  allsubj_low_plapse<-rbind(allsubj_low_plapse,subj_low_plapse)
  allsubj_high_tau<-rbind(allsubj_high_tau,subj_high_tau)
  allsubj_low_tau<-rbind(allsubj_low_tau,subj_low_tau)
}
#plotting partial lapses
plapses<-cbind(allsubjno,allsubj_high_plapse,allsubj_low_plapse)
colnames(plapses)<-c('allsubjno','allsubj_high_plapse','allsubj_low_plapse')
#Plotting group data

plot(plapses$allsubjno,plapses$allsubj_high_plapse, pch= 19, col="red", ylim=c(0,150))
points(plapses$allsubjno,plapses$allsubj_low_plapse, pch= 19, col="blue")
segments(x0=c(plapses$allsubjno),y0=c(plapses$allsubj_low_plapse),x1=c(plapses$allsubjno),y1=c(plapses$allsubj_high_plapse))
#plotting Tau
ptau<-cbind(allsubjno,allsubj_high_tau,allsubj_low_tau)
colnames(ptau)<-c('allsubjno','allsubj_high_tau','allsubj_low_tau')
plot(ptau$allsubjno,(ptau$allsubj_high_tau-ptau$allsubj_low_tau), pch= 19, col="red")
abline(h=0)
points(ptau$allsubjno,ptau$allsubj_low_tau, pch= 19, col="blue")
segments(x0=c(ptau$allsubjno),y0=c(ptau$allsubj_low_tau),x1=c(ptau$allsubjno),y1=c(ptau$allsubj_high_tau))

