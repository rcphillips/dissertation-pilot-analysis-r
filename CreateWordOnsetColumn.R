#CreateWordOnsetColumn.R
#R.Phillips
#160218
###
#this is going to create and append a new column to each subject's taskcsv.
###
#define function:
##load csv
##create new column
##save out csv
#loop for all subjects
###
#update 160219 Adding in rating as well, and adding it all into the task.csv. No seperate file.
###
setwd("C:/Users/rphillips/Desktop/complete_csvs/complete_csvs")
#define function:
subjno<-'07'
#setwd("C:/Users/rphillips/PycharmProjects/convert_eprime") #now works directly with pycharm
#define function:
create_word_onset<-function(subjno){
  ##load csv
  subjtask_name<-c(paste('srp_',subjno,'_task.csv', sep=''))
  subjtask<-read.csv(subjtask_name,stringsAsFactors=FALSE)
  subjpsr_name<-c(paste('srp_',subjno,'_psr.csv', sep=''))
  subjpsr<-read.csv(subjpsr_name,stringsAsFactors=FALSE)
  subjpsr<-subset(subjpsr, select = c(Subject, Word, WordPresentation.RESP,WordPresentation.RT))
  #remove null columns (check that this does not disrupt vectors)
  subjpsr<-subset(subjpsr[
    which(subjpsr$Word!=""),])
  subjpsr<-subset(subjpsr[
    which(subjpsr$Word!="NULL"),])
  ##remove null columns (check that this does not disrupt vectors)
  subjtask<-subset(subjtask[which(subjtask$Probe.ACC!="NULL"),])
  ##create new column for word onsets
  subjtask$WordOnset<-as.numeric(subjtask$Cue.OnsetTime)-4515
  ##create new column for SRP rating
  subj_psr_score<-matrix(nrow=length(subjpsr$word), ncol=1)
  for (k in 1:length(subjtask$DisplayStr)){
    #this tries to match each psr word with a task trial, and then saves the RT, trial type, and psr score.
    subj_psr_score[k]<-as.character(subjpsr$WordPresentation.RESP[match(as.character(
      subjtask$DisplayStr[k]), subjpsr$Word)])
    #as well as the word being matched with (subjtask[k]), this is maintained IN subjtask order and
    #can be cbound directly with the subjtask file.
  }
  #binarize the SRP scores
  srplabel<-matrix(nrow=length(subj_psr_score), ncol=1)
  srplabel[which(as.numeric(subj_psr_score)>=5)]<-2
  srplabel[which(as.numeric(subj_psr_score)<5)]<-1
  #bind the new columns to the subjtask
  subjtask$SRP_Rating<-srplabel
  ##save out csv
  write.csv(subjtask, file = c(paste('srp_',subjno,'_all.csv', sep='')), row.names=FALSE, quote=FALSE)
}

for(i in c(11:26,29:33)){create_word_onset(i)}
