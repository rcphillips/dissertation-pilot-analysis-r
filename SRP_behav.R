#SRP_AXCPT Behavior
#R.Phillips
#160505
#goal is to make a function which extracts AXCPT cue and probe data across subjects
###
#access data
#make table by trial type and accuracy
#label appropriately
#repeat for all subjects and make into larger table
###
###housekeeping
#at home comp
#setwd("C:/Users/ryphil/Desktop/complete_csvs")
#at IRC comp
#setwd("C:/Users/rphillips/Desktop/complete_csvs/complete_csvs")
#at CNS comp
setwd("/Users/rcphillips/Box Sync/Proj_SRPAX/complete_csvs")
###
allsubjaccresult<-data.frame(NULL)
#access data
subjno='02'
for (i in c('01','02','03','04','05','06','07','08','09',10:15,17:24,26,29:33)){
subjno=i
subjdata_name<- paste("srp_", subjno, sep="","_all.csv")
subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
#remove null columns
subjdata<-subset(subjdata[which(subjdata$Probe.ACC!="NULL"),])
#make table by trial type and accuracy
acc_table<-table(subjdata$TrialType,subjdata$Probe.ACC)
#label appropriately
AX_corr<-acc_table[1,2]
AX_incorr<-acc_table[1,1]
AY_corr<-acc_table[2,2]
AY_incorr<-acc_table[2,1]
BX_corr<-acc_table[3,2]
BX_incorr<-acc_table[3,1]
#save result
subjresult<-c(as.numeric(subjno),as.numeric(AX_corr),as.numeric(AX_incorr),as.numeric(BX_corr),as.numeric(BX_incorr),as.numeric(AY_corr),as.numeric(AY_incorr))
allsubjaccresult<-rbind(allsubjaccresult,subjresult)
}
#repeat for all subjects and make into larger table
###
colnames(allsubjaccresult)<-c("subjno","AX_corr","AX_incorr","BX_corr","BX_incorr","AY_corr","AY_incorr")
