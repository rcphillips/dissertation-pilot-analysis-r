#full lapses
#R. Phillips
#150728
#The goal here is to classify trials into high and low, and then check the lapse rate.
###
###Housekeeping:
#IRC
setwd("C:/Users/rphillips/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
library(ggplot2)
install.packages('retimes')
library(retimes)
###
subjno=37
full_lapses<-function(subjno){
  #Bring in a single subject's RT distribution
  subjdata_name<- paste("subj", subjno, sep="","_task.csv")
  subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
  #remove nonlapse trials
  subjdata<-subset(subjdata, subjdata$Cue.ACC=="1" & subjdata$Probe.ACC=="0")
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
  high_full<-dim(subj_highSRP)[1]
  low_full<-dim(subj_lowSRP)[1]
  result<-data.frame(high_full,low_full)
  return(result)}
