#SRP_AXCPT Behavior -- RTs broken up by SRP level
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
#setwd("C:/Users/rphillips/Desktop/complete_csvs/complete_csvs")
#at CNS comp
setwd("/Users/rcphillips/Box Sync/Proj_SRPAX/complete_csvs")
###
allsubjRTresult<-data.frame(NULL,stringsAsFactors=FALSE)
#access data
subjno='11'
for (i in c('01','02','03','04','05','06','07','08','09',10:15,17:24,26,29:33)){
  subjno=i
  subjdata_name<- paste("srp_", subjno, sep="","_all.csv")
  subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
  #remove null columns
  subjdata<-subset(subjdata[which(subjdata$Probe.ACC!="NULL"),])
  #Just the high SRP trials
  subjdata<-subset(subjdata[which(subjdata$SRP_Rating==1),])
  #remove incorrect trials
  subjdata<-subset(subjdata[which(subjdata$Probe.ACC==1 & subjdata$Cue.ACC==1),])
  AX_meanRT<-mean(subjdata$Probe.RT[which(subjdata$TrialType=="AX")])
  BX_meanRT<-mean(subjdata$Probe.RT[which(subjdata$TrialType=="BX")])
  AY_meanRT<-mean(subjdata$Probe.RT[which(subjdata$TrialType=="AY")])
  #make table by trial type and RT
  subjresult<-cbind(as.numeric(subjno),as.numeric(AX_meanRT),as.numeric(BX_meanRT),as.numeric(AY_meanRT))
  #label appropriately  
  #save result
  allsubjRTresult<-rbind(allsubjRTresult,subjresult)
}


#repeat for all subjects and make into larger table
###
colnames(allsubjRTresult)<-c("subjno","AX_meanRT","BX_meanRT","AY_meanRT")

write.csv(allsubjRTresult,file="Rphillips_SRP_Behavior_RT.csv")

#plotting:

plot(x=c(rep(1,length(allsubjRTresult$AX_meanRT))),y=allsubjRTresult$AX_meanRT,col="black",
     #type='n',
     xlim=c(0,4),
     ylim=c(0,1000))
title(main="lowSRP_ProbeRTs")
points(x=c(rep(2,length(allsubjRTresult$BX_meanRT))),
       y=allsubjRTresult$BX_meanRT,
       #type='n',
       col = "red")
#text(x=c(rep(1,length(allsubjRTresult$AX_meanRT))),y=allsubjRTresult$AX_meanRT,
     #labels = allsubjRTresult$subjno,col="blue",cex=1)
#text(x=c(rep(2,length(allsubjRTresult$BX_meanRT))),y=allsubjRTresult$BX_meanRT,
     #labels = allsubjRTresult$subjno,col="blue",cex=1)
segments(x0=c(rep(1,length(allsubjRTresult$AX_meanRT))),
         y0=allsubjRTresult$AX_meanRT,
         x1=c(rep(2,length(allsubjRTresult$BX_meanRT))),
         y1=c(allsubjRTresult$BX_meanRT))
points(x=c(rep(3,length(allsubjRTresult$AY_meanRT))),y=allsubjRTresult$AY_meanRT,col="green")
segments(x0=c(rep(2,length(allsubjRTresult$BX_meanRT))),
        y0=allsubjRTresult$BX_meanRT,
        x1=c(rep(3,length(allsubjRTresult$AY_meanRT))),
        y1=c(allsubjRTresult$AY_meanRT))