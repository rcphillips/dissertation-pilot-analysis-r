#PSR categorization for fMRI analysis
#R.Phillips
#151007
#The goal of this script is to add a column to the edats produced by eprime which
#indicates whether that particular trial was rated "high significance" (highSRP) or 
#"low significance" (low SRP) by the subject in the second part of the task.
#TODO
#currently, the "don't know" responses of the PSR become NA. That might mess things up? Or maybe they just won't be marked as high or low SRP.
#maybe they should go into unmodelled trials??
###
#import task csv
#import psr csv
#generate dummy variable column
#join this column to task csv
#save out as task-psr csv
###
#Housekeeping
#CNS 
setwd("~/Desktop/test_csvforMRI_withPSR")
###
#import task csv
#import psr csv
#generate dummy variable column
#join this column to task csv
#save out as task-psr csv
###
#import task csv
subjtask<-read.csv('srp_10_task.csv',stringsAsFactors=FALSE)
#remove null columns (check that this does not disrupt vectors)
subjtask<-subset(subjtask[which(subjtask$Probe.ACC!="NULL"),])
#import psr csv
subjpsr<-read.csv('srp_10_psr.csv',stringsAsFactors=FALSE)
subjpsr<-subset(subjpsr, select = c(Subject, Block, Word, WordPresentation.RESP,WordPresentation.RT))
#remove null columns (check that this does not disrupt vectors)
subjpsr<-subset(subjpsr[
  which(subjpsr$Word!=""),])
#generate psr column
#match each PSSword with its PSSscore
subj_psr_score<-matrix(nrow=length(subjpsr$word), ncol=1)
for (k in 1:length(subjtask$DisplayStr)){
#this tries to match each psr word with a task trial, and then saves the RT, trial type, and psr score.
  subj_psr_score[k]<-as.character(subjpsr$WordPresentation.RESP[match(as.character(subjtask$DisplayStr[k]), subjpsr$Word)])
}
#join this column to task csv
subjtask<-cbind(subjtask,subj_psr_score)
#binarize psr scores
srplabel<-matrix(nrow=length(subjtask$subj_psr_score), ncol=1)
srplabel[which(as.numeric(subjtask$subj_psr_score)>=5)]<-"high"
srplabel[which(as.numeric(subjtask$subj_psr_score)<5)]<-"low"
table(srplabel)
#join this column to task csv
subjtask<-cbind(subjtask,srplabel)
subjtask$srplabel
#save out csv
write.csv(subjtask, file = "srp_10_taskpsr.csv")
