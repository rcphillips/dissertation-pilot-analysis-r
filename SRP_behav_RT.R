#SRP_AXCPT Behavior -- RTs
#R.Phillips
#160505
#goal is to make a function which extracts AXCPT cue and probe data across subjects
###
#access data
#make table by trial type and RTs
#label appropriately
#repeat for all subjects and make into larger table
###
###housekeeping
#at home comp
#setwd("C:/Users/ryphil/Desktop/complete_csvs")
#at IRC comp
setwd("C:/Users/rphillips/Desktop/complete_csvs/complete_csvs")
###
allsubjresult<-data.frame(NULL,stringsAsFactors=FALSE)
#access data
subjno='11'
for (i in c('01','02','03','04','05','06','07','08','09',10:15,17:24,26,29:33)){
  subjno=i
  subjdata_name<- paste("srp_", subjno, sep="","_all.csv")
  subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
  #remove null columns
  subjdata<-subset(subjdata[which(subjdata$Probe.ACC!="NULL"),])
  #remove incorrect trials
  subjdata<-subset(subjdata[which(subjdata$Probe.ACC==1),])
  AX_meanRT<-mean(subjdata$Probe.RT[which(subjdata$TrialType=="AX")])
  BX_meanRT<-mean(subjdata$Probe.RT[which(subjdata$TrialType=="BX")])
  AY_meanRT<-mean(subjdata$Probe.RT[which(subjdata$TrialType=="AY")])
  #make table by trial type and RT
  subjresult<-cbind(as.numeric(subjno),as.numeric(AX_meanRT),as.numeric(BX_meanRT),as.numeric(AY_meanRT))
  #label appropriately  
  #save result
  allsubjresult<-rbind(allsubjresult,subjresult)
}


#repeat for all subjects and make into larger table
###
colnames(allsubjresult)<-c("subjno","AX_meanRT","BX_meanRT","AY_meanRT")

write.csv(allsubjresult,file="Rphillips_SRP_Behavior_RT.csv")
