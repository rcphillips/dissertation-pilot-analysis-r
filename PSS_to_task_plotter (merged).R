###Personal Significance -> task plotter
#R.Phillips
#12/4/14

#Bring in the PSS data for one subject
#generate new groups based on quartile split.
#plot the result
#Set working directory to flash drive (on home PC))
#setwd("F:/r/analyses (in r)/AX_RT_analysis")
#Set working directory to flash drive (on CNS Mac))
#setwd("/Volumes/NEW VOLUME/r/analyses (in r)/AX_RT_analysis")
#Set working directory to flash drive (on IRC PC)
#setwd("F:/r/analyses (in r)/AX_RT_analysis")
#Set working directory to flash drive (on laptop)
#setwd("H:/r/analyses (in r)/AX_RT_analysis")
#Set working directory to box sync! Does this work? (from CNS)
setwd("~/Box Sync/SRP_AXCPT_pilot_data")
###
#TRIAL TYPE SELECTION
trial_type_of_interest <- "AX"

###
i=6
#subject counter start (for JUST pss, not cbind with task)
allsubj_personal<-data.frame(NULL)
allsubj_high_tau<-data.frame(NULL)
allsubj_low_tau<-data.frame(NULL)
allsubj_num<-data.frame(subjnum=c(23:32))
allsubj_full_lapse<-data.frame(NULL)
allsubj_partial_lapse<-data.frame(NULL)
#extract the dta for all subjects
for  (i in c(6,7,9,10,11,13,15,16,17,18,19,20,22:32)){
  subjpss_name<- paste("subj", i, sep="","_pss.csv")
  subjpss<-read.csv(subjpss_name,stringsAsFactors=FALSE)
  #load and name the csv for RTs
  subjdata_name<- paste("subj", i, sep="","_task.csv")
  subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
  #Remove inaccurate trials from the subjdata. This part of the task is just concerned with
  #correct trials/partial lapses. Incorrect trials are dealt with elsewhere.
  subjdata<-subset(subjdata, subjdata$Cue.ACC=="1" & subjdata$Probe.ACC=="1")
#####Side analyses
  #fun way of looking at which trials they got right and wrong!
  table(subjdata$TrialType[which(subjdata$Probe.ACC=="0")])
  #note, have to do this before removing incorr trials
  ###
  #colors(1:20)
  #when are incorr AX occuring?#this doesn't account for BLOCKS
  #plot(subjdata$Probe.ACC,col=i)
  ###
  #subj_full_lapse<-length(subset(subjdata$Probe.ACC, subjdata$TrialType=="AX"))
  #allsubj_full_lapse<-c(allsubj_full_lapse, subj_full_lapse)
#####
  
  if (i %in% c(6,7,9,10,11,13,15,16,17,18,19,20,22)==TRUE){
    #9, 13, 16, 18 #removed for low AX accuracy, low BX accuracy
    #NOTE: these removed subjects have to be added back in in order
    #remove pointless columns. Sorry this line is so gross. You can't break the c()...
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
      #pilot three version also ^
    }
    clean_subj_pss<-data.frame(word,score,reaction_time)
  }
  #save the output
  subjdigit<-subjpss$Subject[1] #subjno
  savefilename<-paste("subj", subjdigit, sep="", "_personal_ratings")
  assign(savefilename, clean_subj_pss)
  ###
  #Sort the two lists alphabetically, and then just apply the PSS scores to the task scores.
  sorted_pss<-clean_subj_pss[order(word),]
  sorted_task<-subjdata[order(subjdata$DisplayStr),]
  #Turns out the above method had problems too. But! I have a solution!
  #And it's called match()!
  subj_RT<-matrix(nrow=length(sorted_pss$word), ncol=1)
  subj_trialtype<-matrix(nrow=length(sorted_pss$word), ncol=1)
  subj_pss_score<-matrix(nrow=length(sorted_pss$word), ncol=1)
  
  for (k in 1:length(clean_subj_pss$word)){
    #determine which sorted_task$DisplayStr[] is a PSS word.
    match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)
    #and extract the related RT and PSS score
    #TASK SEGMENT SELECTION (AX CUE OR PROBE)
    subj_RT[k]<-subjdata$Probe.RT[match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)]
    subj_trialtype[k]<-as.character(subjdata$TrialType[match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)])
    #the above is the crucial line. It is going into the (needlessly) sorted PSS words one by one (loop using k), and seeing if there are any matches in the subject's task data DisplayStr. If there are, it
    #assigns that value to that row in the subject's RT. This is then matched to the subject's pss score below, to construct subj_sail.
    subj_pss_score[k]<-sorted_pss$score[k]
  }
  
  #getting it plotted:
  subj_sail<-data.frame(subj_RT,subj_pss_score,subj_trialtype)
  
  subj_sail<-subset(subj_sail, subj_trialtype==trial_type_of_interest)
  #subj_sail thus contains a paired list of RTs and PSS scores. What I would like to do now is have this also contain AX trial type info.
  summary(subj_pss_score)[5]
  subj_personal_high<-subset(subj_sail, subj_sail$subj_pss_score>=5)
  #assign categorical variable for plotting
  subj_personal_high$group<-rep("high",nrow(subj_personal_high))
  subj_personal_low<-subset(subj_sail, subj_sail$subj_pss_score<4)
  subj_personal_low$group<-rep("low",nrow(subj_personal_low))
  subj_personal<-rbind(subj_personal_high,subj_personal_low)
  
  #save the output
  subjdigit<-subjdata$Subject[1] #subjno
  savefilename<-paste("subj", subjdigit, sep="", "_personal_salience")
  assign(savefilename, subj_personal)
  
  subj_partial_lapse<-length(which(subj_personal$subj_RT>550))
  allsubj_partial_lapse<-c(allsubj_partial_lapse, subj_partial_lapse)
  #knit the data into one large table
allsubj_personal<-rbind(allsubj_personal,subj_personal)


}#END OF MAIN LOOP
#Plot the group results
library(ggplot2)
plot_title<-paste(trial_type_of_interest,"probe RT with median PSR split", ' ')
ggplot(allsubj_personal, aes(x=subj_RT)) + 
  geom_density(fill="white", colour="black", binwidth=30) +
  xlim(-1,2000) +
  xlab(paste(trial_type_of_interest,"Probe RT", ' '))+
  ylim(-.001,.005) +
  ylab("Probability Density")+
  ggtitle(plot_title) +
  theme(strip.text.y=element_text(size=30), axis.title=element_text(size=30), 
        plot.title=element_text(size=30)) +
  facet_grid(group ~ .)

#word extractor!
#subj24_personal_ratings$word[which(subj24_personal_salience$subj_RT>300)]

