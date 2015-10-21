#BX_error_extractor
#the goal of this script is to sort by high and low SRP
#and then get a table of trial type by error.
###Housekeeping:
#IRC
setwd("C:/Users/rphillips/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
###
#rip off SRP extractor from "ex_gaussian.R"
#produce group result table
###
#test case:
subjno=33
#The function:
errors_by_AXtype<-function(subjno){
  
#Bring in a single subject's RT distribution
  subjdata_name<- paste("subj", subjno, sep="","_task.csv")
  subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
  #remove incorrect CUES. We want them to have the cue correct
  subjdata<-subset(subjdata, subjdata$Cue.ACC=="1")
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
  subj_acc<-matrix(nrow=length(sorted_pss$word), ncol=1)
  subj_trialtype<-matrix(nrow=length(sorted_pss$word), ncol=1)
  subj_pss_score<-matrix(nrow=length(sorted_pss$word), ncol=1)
  subj_word<-matrix(nrow=length(sorted_pss$word), ncol=1)
  for (k in 1:length(clean_subj_pss$word)){
    #TASK SEGMENT SELECTION (AX CUE OR PROBE)
    subj_RT[k]<-subjdata$Probe.RT[match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)]
    subj_acc[k]<-subjdata$Probe.ACC[match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)]
    subj_trialtype[k]<-as.character(subjdata$TrialType[match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)])
    subj_word[k]<-as.character(subjdata$DisplayStr[match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)])
    #this tries to match each PSS word with a task trial, and then saves the RT, trial type, and pss score.
    subj_pss_score[k]<-sorted_pss$score[k]
  }
  
  subj_taskpss<-data.frame(subj_RT, subj_acc,subj_pss_score,subj_trialtype,subj_word)
  ##subset out BX and AY trials
  #subj_taskpss<-subset(subj_taskpss, subj_trialtype=="AX")
  #break taskpss out into high and low SRP
  subj_highSRP<-subset(subj_taskpss, subj_taskpss$subj_pss_score>=5)
  subj_lowSRP<-subset(subj_taskpss, subj_taskpss$subj_pss_score<3)

  highSRP_BXtable<-table(subj_highSRP$subj_trialtype,subj_highSRP$subj_acc)[3,]
  lowSRP_BXtable<-table(subj_lowSRP$subj_trialtype,subj_lowSRP$subj_acc)[3,]
  highSRP_RT<-c(mean(subj_highSRP$subj_RT,na.rm=TRUE),0)
  lowSRP_RT<-c(mean(subj_lowSRP$subj_RT,na.rm=TRUE),0)
  result<-rbind(highSRP_BXtable,lowSRP_BXtable,highSRP_RT,lowSRP_RT)
  return(result)}
##########
##########
#looping through all subjs

allsubjno<-data.frame(NULL)
allsubj_high_incorr<-data.frame(NULL)
allsubj_low_incorr<-data.frame(NULL)
allsubj_high_corr<-data.frame(NULL)
allsubj_low_corr<-data.frame(NULL)
allsubj_high_RT<-data.frame(NULL)
allsubj_low_RT<-data.frame(NULL)
for (i in c(33,34,35,36,37,38,39,41,42,43,44,45)){
  #extract
  subjno <- i
  subj_high_incorr<-errors_by_AXtype(i)[1]
  subj_low_incorr<-errors_by_AXtype(i)[2]
  subj_high_corr<-errors_by_AXtype(i)[5]
  subj_low_corr<-errors_by_AXtype(i)[6]
  subj_high_RT<-errors_by_AXtype(i)[3]
  subj_low_RT<-errors_by_AXtype(i)[4]
  #link
  allsubjno<-rbind(allsubjno,subjno)
  allsubj_high_incorr<-rbind(allsubj_high_incorr,subj_high_incorr)
  allsubj_low_incorr<-rbind(allsubj_low_incorr,subj_low_incorr)
  allsubj_high_corr<-rbind(allsubj_high_corr,subj_high_corr)
  allsubj_low_corr<-rbind(allsubj_low_corr,subj_low_corr)
  allsubj_high_RT<-rbind(allsubj_high_RT,subj_high_RT)
  allsubj_low_RT<-rbind(allsubj_low_RT,subj_low_RT)
}
allsubj_err_by_type<-cbind(allsubjno,allsubj_high_incorr,allsubj_high_corr,allsubj_low_incorr,allsubj_low_corr,allsubj_high_RT,allsubj_low_RT)
colnames(allsubj_err_by_type)<-c('allsubjno','allsubj_high_incorr','allsubj_high_corr','allsubj_low_incorr','allsubj_low_corr','allsubj_high_RT','allsubj_low_RT')

#Just getting Rates
allsubj_bxerr_high_rate<-allsubj_err_by_type$allsubj_high_incorr/(allsubj_err_by_type$allsubj_high_incorr+all_subj_err_by_type$allsubj_high_corr)

allsubj_bxerr_low_rate<-allsubj_err_by_type$allsubj_low_incorr/(allsubj_err_by_type$allsubj_low_incorr+all_subj_err_by_type$allsubj_low_corr)

#Plotting group data
=======
plot(allsubj_err_by_type$allsubjno,allsubj_err_by_type$allsubj_high_RT, pch= 19, col="red",ylim=c(0,1500))
points(allsubj_err_by_type$allsubjno,allsubj_err_by_type$allsubj_low_RT, pch= 19, col="blue")
segments(x0=c(allsubj_err_by_type$allsubjno),y0=c(allsubj_err_by_type$allsubj_high_RT),x1=c(allsubj_err_by_type$allsubjno),y1=c(allsubj_err_by_type$allsubj_low_RT))
#plotting Tau
ptau<-cbind(allsubjno,allsubj_high_tau,allsubj_low_tau)
colnames(ptau)<-c('allsubjno','allsubj_high_tau','allsubj_low_tau')
plot(ptau$allsubjno,(ptau$allsubj_high_tau-ptau$allsubj_low_tau), pch= 19, col="red")
abline(h=0)
points(ptau$allsubjno,ptau$allsubj_low_tau, pch= 19, col="blue")
segments(x0=c(ptau$allsubjno),y0=c(ptau$allsubj_low_tau),x1=c(ptau$allsubjno),y1=c(ptau$allsubj_high_tau))



