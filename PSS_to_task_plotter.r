###Personal Significance -> task plotter
#R.Phillips
#12/3/14

#Bring in the PSS data for one subject
#generate new groups based on quartile split.
#plot the results
###
#subject counter start (for JUST pss, not cbind with task)
i=6
allsubj_personal<-data.frame(NULL)
#extract the dta for all subjects
for  (i in c(6,7,9,10,11,13,15,16,17,18,19,20,22)){
  #load and name the csv for PSS
  subjpss_name<- paste("subj", i, sep="","_pss.csv")
  subjpss<-read.csv(subjpss_name)
  #load and name the csv for RTs
  subjdata_name<- paste("subj", i, sep="","_task.csv")
  subjdata<-read.csv(subjdata_name)
  #Format the PSS data
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
  subj_pss_score<-matrix(nrow=length(sorted_pss$word), ncol=1)
  
  for (k in 1:length(clean_subj_pss$word)){
    #determine which sorted_task$DisplayStr[] is a PSS word.
    match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)
    #and extract the related RT and PSS score
    subj_RT[k]<-subjdata$Cue.RT[match(as.character(sorted_pss$word[k]), subjdata$DisplayStr)]
    subj_pss_score[k]<-sorted_pss$score[k]
  }
  
  #getting it plotted:
  subj_sail<-data.frame(subj_RT,subj_pss_score)
  summary(subj_pss_score)[5]
  subj_personal_high<-subset(subj_sail, subj_sail$subj_pss_score>=5)
  #assign categorical variable for plotting
  subj_personal_high$group<-rep("high",nrow(subj_personal_high))
  subj_personal_low<-subset(subj_sail, subj_sail$subj_pss_score<=4)
  subj_personal_low$group<-rep("low",nrow(subj_personal_low))
  subj_personal<-rbind(subj_personal_high,subj_personal_low)
  #plot the group RT by the wordtype using ggplot
  #library(ggplot2)
  #ggplot(subj_personal, aes(x=subj_RT)) + geom_histogram(fill="white", colour="black")+
  #  facet_grid(group ~ .)
  #Good! that works well for a single subject, how about multiple subjects
  
  #save the output
  subjdigit<-subjdata$Subject[1] #subjno
  savefilename<-paste("subj", subjdigit, sep="", "_personal_salience")
  assign(savefilename, subj_personal)
  #knit the data into one large table
  allsubj_personal<-rbind(allsubj_personal,subj_personal)
}
#Plot the personalized results
library(ggplot2)
ggplot(allsubj_personal, aes(x=subj_RT)) + 
  geom_density(fill="white", colour="black", binwidth=30) +
  ggtitle("high/lowSRP with quartile-based split") +
  
  theme(strip.text.y=element_text(size=20)) +
  facet_grid(group ~ .)
