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
setwd("C:/Users/rphillips/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
#CNS
setwd("~/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
#home
setwd("E:/Box Sync/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
library(ggplot2)
install.packages('retimes')
library(retimes)
allsubjno<-data.frame(NULL)
allsubj_high_plapse<-data.frame(NULL)
allsubj_low_plapse<-data.frame(NULL)
allsubj_high_tau<-data.frame(NULL)
allsubj_low_tau<-data.frame(NULL)
###
#test case:
subjno=33
#The function:
partial_lapses<-function(subjno){
#Bring in a single subject's RT distribution
subjdata_name<- paste("subj", subjno, sep="","_task.csv")
subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
#remove incorrect trials
subjdata<-subset(subjdata, subjdata$Cue.ACC=="1" & subjdata$Probe.ACC=="1")
#Bring in a single subject's PSR info
subjpss_name<- paste("subj", subjno, sep="","_pss.csv")
subjpss<-read.csv(subjpss_name,stringsAsFactors=FALSE)
#split the data by SRP
if (subjno %in% c(6,7,9,10,11,13,15,16,17,18,19,20,22)==TRUE){
  #9, 13, 16, 18 #removed for low AX accuracy, low BX accuracy
  #This group is for an early version of the pss script.
  #removed unused columns
  subjpss<-subset(subjpss, select = c(Subject, Block, Word.Trial., WordPresentation.RESP, WordPresentation.RT, WordPresentation1.RESP, WordPresentation1.RT, WordPresentation2.RESP, WordPresentation2.RT, WordPresentation3.RESP, WordPresentation3.RT, WordPresentation4.RESP, WordPresentation4.RT, WordPresentation5.RESP, WordPresentation5.RT))
  #match each PSSword with its PSSscore
  j=1
  reaction_time<-matrix(nrow = 306, ncol=1)
  score<-matrix(nrow = 306, ncol=1)
  word<-matrix(nrow = 306, ncol=1)
  for (j in 1:306) {
    reaction_time[j]<-max(subjpss[j,4:15], na.rm=TRUE) #a somewhat strange way of getting RTs 
    score[j]<-min(subjpss[j,4:15], na.rm=TRUE)
    word[j]<-as.character(subjpss$Word.Trial.[j])
  }
  clean_subj_pss<-data.frame(word,score,reaction_time)
}
else
{
  subjpss<-subset(subjpss, select = c(Subject, Block, Word, WordPresentation.RESP,WordPresentation.RT))
  #match each PSSword with its PSSscore
  j=10
  reaction_time<-matrix(nrow = 306, ncol=1)
  score<-matrix(nrow = 306, ncol=1)
  word<-matrix(nrow = 306, ncol=1)
  for (j in 1:306) {
    reaction_time[j]<-subjpss$WordPresentation.RT[j]
    score[j]<-subjpss$WordPresentation.RESP[j]
    word[j]<-as.character(subjpss$Word[j])
    
  }
  clean_subj_pss<-data.frame(word,score,reaction_time)
}
sorted_pss<-clean_subj_pss[order(word),]
sorted_task<-subjdata[order(subjdata$DisplayStr),]
subj_RT<-matrix(nrow=length(sorted_pss$word), ncol=1)
subj_trialtype<-matrix(nrow=length(sorted_pss$word), ncol=1)
subj_pss_score<-matrix(nrow=length(sorted_pss$word), ncol=1)
subj_word<-matrix(nrow=length(sorted_pss$word), ncol=1)
for (k in 1:length(clean_subj_pss$word)){
  #TASK SEGMENT SELECTION (AX CUE OR PROBE)
  subj_RT[k]<-subjdata$Probe.RT[match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)]
  subj_trialtype[k]<-as.character(subjdata$TrialType[match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)])
  subj_word[k]<-as.character(subjdata$DisplayStr[match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)])
  #this tries to match each PSS word with a task trial, and then saves the RT, trial type, and pss score.
  subj_pss_score[k]<-sorted_pss$score[k]
}

subj_taskpss<-data.frame(subj_RT,subj_pss_score,subj_trialtype,subj_word)
#subset out BX and AY trials
subj_taskpss<-subset(subj_taskpss, subj_trialtype=="AX")
#break taskpss out into high and low SRP
subj_highSRP<-subset(subj_taskpss, subj_taskpss$subj_pss_score>=5)
subj_lowSRP<-subset(subj_taskpss, subj_taskpss$subj_pss_score<3)
#apply the timefit method to each class of SRP
high_exg<-timefit(x=subj_highSRP$subj_RT, iter = 0, size = length(subj_highSRP$subj_RT),
                  replace = TRUE, plot = TRUE, start = NULL)
high_tau<-high_exg@par[3]
low_exg<-timefit(x=subj_lowSRP$subj_RT, iter = 0, size = length(subj_lowSRP$subj_RT),
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
for (i in c(6,7,9,10,11,15,16,17,19,20,22,24,25,26,28,31,32,34,35,37,38,39,40,41,42,43,44,45)){
=======
#looping through all subjects
allsubjno<-data.frame(NULL)
allsubj_high_plapse<-data.frame(NULL)
allsubj_low_plapse<-data.frame(NULL)
allsubj_high_tau<-data.frame(NULL)
allsubj_low_tau<-data.frame(NULL)

for (i in c(33,34,35,36,37,38,39,41)){
  #extract
  subjno <- i
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
=======
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

