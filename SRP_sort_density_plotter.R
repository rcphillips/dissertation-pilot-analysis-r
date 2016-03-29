#generating density plots, split up by psr
#160328
####
#Bring in a single subject's RT distribution

#Bring in a single subject's PSR info

#split the data by SRP

#apply the timefit method to each class of SRP

#save the tau values from each of those timefits

#apply tau cutoff for each class of SRP

#quantify  partial lapses for each class of SRP

#repeat for all subjects
###
###housekeeping
#at home comp
#setwd("C:/Users/ryphil/Desktop/complete_csvs")
#at IRC comp
setwd("C:/Users/rphillips/Desktop/complete_csvs/complete_csvs")
###
#subjlist
#(i in c('01','02','03','04','05','06','07','08','09',10:15,17:24,26,29:33))
#testcase
subjno='01'
split_density_plot<-function(subjno){
  #load SRP info
  subjpsr_name<- paste("srp_", subjno, sep="","_psr.csv")
  subjpsr<-read.csv(subjpsr_name,stringsAsFactors=FALSE)
  #remove null columns
  subjpsr<-subset(subjpsr[
    which(subjpsr$Word!=""),])
  subjpsr<-subset(subjpsr[
    which(subjpsr$Word!="NULL"),])
  #load and name the csv for RTs
  subjtask_name<- paste("srp_", subjno, sep="","_task.csv")
  subjtask<-read.csv(subjtask_name,stringsAsFactors=FALSE)
  #remove null columns
  subjtask<-subset(subjtask[which(subjtask$Probe.ACC!="NULL"),])
  #Remove inaccurate trials from the subjdata. 
  subjtask<-subset(subjtask, subjtask$Cue.ACC=="1" & subjtask$Probe.ACC=="1")

  #match and merge the srp information into the task information
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
  subjtask$srp_class[which(subjtask$srp_rating<3)] = "low"
  subjtask$srp_class[which(subjtask$srp_rating>=6)] = "high"
  
  highCueRTs<-subjtask$Cue.RT[which(subjtask$srp_class=="high")]
  lowCueRTs<-subjtask$Cue.RT[which(subjtask$srp_class=="low")]  
  result<-cbind(highCueRTs,lowCueRTs)
return(result)
}

allsubjno<-data.frame(NULL)
allsubj_highCueRT<-data.frame(NULL)
allsubj_lowCueRT<-data.frame(NULL)
#for (i in c('01','02','03','04','05','06','07','08','09',10:15,17:24,26,29:33)){
 for (i in c('02','04','07','09',10:13,18,19,22,24,26,29:33)){

#extract
  subjno <- i
  subj_highCueRT<-split_density_plot(subjno)[,1]
  subj_lowCueRT<-split_density_plot(subjno)[,2]
  
  #link
  allsubjno<-c(allsubjno,subjno)
  allsubj_highCueRT<-c(allsubj_highCueRT,subj_highCueRT)
  allsubj_lowCueRT<-c(allsubj_lowCueRT,subj_lowCueRT)
  
  #fix these ugly plots, but you can see the tail
  #plot(density(as.numeric(allsubj_highCueRT)))
  #plot(density(as.numeric(allsubj_lowCueRT)))
  
}

plotdata<-data.frame("times"<-c(as.numeric(allsubj_highCueRT),
                              as.numeric(allsubj_lowCueRT)),
                     "label"<-c(rep("high",length(as.numeric(allsubj_highCueRT))),rep("low",length(as.numeric(allsubj_highCueRT))))
                     )
colnames(plotdata)<-c("times","label")


allsubj_highCueRT<-data.frame(as.numeric(allsubj_highCueRT))
colnames(allsubj_highCueRT)<-"times"
allsubj_lowCueRT<-data.frame(as.numeric(allsubj_lowCueRT))
colnames(allsubj_lowCueRT)<-"times"


library(ggplot2)
plot_title<-paste("AXCPT Reaction Times")
plot<-ggplot() + 
  xlim(-1,2000) +
  xlab("AX Probe RT")+
  ylim(-.001,.0035) +
  ylab("Probability Density")+
  ggtitle(plot_title) +
  theme(strip.text.y=element_text(size=20), 
        axis.title=element_text(size=20), 
        plot.title=element_text(size=30),
        axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        legend.key = element_rect(color=NA))

plot+
  geom_density(data=allsubj_lowCueRT,aes(x=times,fill="Low"))+
  geom_density(alpha=.8,data=allsubj_highCueRT,aes(x=times,fill="High"))+
  scale_fill_manual(values=c("#F8766D", "#00BCD8"))+
  guides(color="none",fill=guide_legend(title="SRP Level"))