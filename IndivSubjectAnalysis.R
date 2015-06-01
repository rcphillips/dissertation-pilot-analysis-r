#SRP-AXCPT
#IndivSubjectAnalysis
#R.Phillips
#150512
#Trying something different this time. Rather than adding things onto an
#increasingly cumbersome script, I'm just going to bring in the pieces from the
#other script (PSS_to_task_plotter (merged).R) which are important for the output, and
#keep it in the same working directory.
###pseudocode:
#for a single subject:
#Read csv
#plot RT on x axis
#plot count on y axis
#add grouping variable
#
#repeat for all subjects on the same plot
###housekeeping- setting working directory, initializing variables
#setwd:
#for IRC comp
#setwd("C:/Users/rphillips/Box Sync/SRP_AXCPT_pilot_data")
#for CNS comp
#setwd("~/Box Sync/SRP_AXCPT_pilot_data")
#for home comp
setwd("E:/Box Sync/Box Sync/SRP_AXCPT_pilot_data")
install.packages("ggplot2")
install.packages("RColorBrewer")
library(ggplot2)
library(RColorBrewer)

#Initializing variables
allsubjdata<-data.frame(NULL)
trial_type_of_interest <- "AX"
###actual code begins
#extract the data for all subjects
i=33
for  (i in c(6,7,10,11,15, 16,17,19,20,22,24,25,26,27,28,30,31,32)){
  #29 needs to be dropped because not all blocks were correctly encoded
#Read csv
  subjpss_name<- paste("subj", i, sep="","_pss.csv")
  subjpss<-read.csv(subjpss_name,stringsAsFactors=FALSE)
#load and name the csv for RTs
  subjdata_name<- paste("subj", i, sep="","_task.csv")
  subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
#Remove inaccurate trials from the subjdata. This part of the task is just concerned with
#correct trials/partial lapses. Incorrect trials are dealt with elsewhere.
  subjdata<-subset(subjdata, subjdata$Cue.ACC=="1" & subjdata$Probe.ACC=="1")
  subjdata<-subset(subjdata, select = c(Probe.RT, DisplayStr, TrialType))# can add more later
#bring all subjects into same data frame
  subjdata[length(subjdata)+1]<-i # give a group variable
  allsubjdata<-rbind(allsubjdata,subjdata) #join with data from previous loops

###SRP info:
#This first part deals with the poor file structure I generated in my initial pilot:
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
  subjpss<-data.frame(word,score,reaction_time)
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
  subjpss<-data.frame(word,score,reaction_time)
}
#at this point we have subjpss on the individual level, what we need to do now is take the
#TASK rt and link it to the PSS score.
#unfortunately, we have to initialize some variables down here, because we don't know how long
#this matrix is going to be until we have the indiv. subject's data
subj_RT<-matrix(nrow=length(subjpss$word), ncol=1)
subj_trialtype<-matrix(nrow=length(subjpss$word), ncol=1)
subj_pss_score<-matrix(nrow=length(subjpss$word), ncol=1)
subj_pss_word<-matrix(nrow=length(subjpss$word), ncol=1)
###
k=100
for (k in 1:length(subjpss$word)){
  #TASK SEGMENT SELECTION (AX CUE OR PROBE)
  subj_RT[k]<-subjdata$Probe.RT[match(as.character(subjpss$word[k]), subjdata$DisplayStr)]
  subj_trialtype[k]<-as.character(subjdata$TrialType[match(as.character(subjpss$word[k]), subjdata$DisplayStr)])
  subj_pss_word[k]<-subjdata$DisplayStr[match(as.character(subjpss$word[k]), subjdata$DisplayStr)]
  #the above is the crucial line. It is going into the (needlessly) sorted PSS words one by one (loop using k), and seeing if there are any matches in the subject's task data DisplayStr. If there are, it
  #assigns that value to that row in the subject's RT. This is then matched to the subject's pss score below, to construct subj_sail.
  subj_pss_score[k]<-subjpss$score[k]
}

subjtask_and_pss<-data.frame(subj_pss_word,subj_RT,subj_trialtype,subj_pss_score)
subjtask_and_pss<-subset(subjtask_and_pss, subj_trialtype==trial_type_of_interest)
#so now we've got the task RT and pss score together
#the next thing is to classify the scores as high or low
subjtask_and_pss$SRPclass<-c(1:length(subjtask_and_pss$subj_pss_word)) #initializing
for (k in 1:length(subjtask_and_pss$subj_pss_word)){ 
  if ((subjtask_and_pss$subj_pss_score[k]>=6)=="TRUE"){subjtask_and_pss$SRPclass[k]='high'}
  if ((subjtask_and_pss$subj_pss_score[k]<5)=="TRUE") {subjtask_and_pss$SRPclass[k]='low'}
}
indiv_subj_plot <- ggplot(subjtask_and_pss, aes(x=subj_RT)) + 
  geom_histogram(binwidth=10) + 
  ggtitle(paste("subj", i, sep="","_plot")) +
  scale_x_continuous(limits=c(0,2000))+
  scale_y_continuous(limits=c(0,5))
indiv_subj_plot
assign(paste("subj", i, sep="","_plot"),indiv_subj_plot)
}#end group processing
colnames(allsubjdata)[length(allsubjdata)]<-"subjno"
#plot RT on x axis
#plot count on y axis
hist(subjdata$Probe.RT, breaks = 100, ylim=c(0,5))

###PSR plotting
#The goal here is to produce a histogram simply indicating where their responses were
table(subjpss$score)

indiv_subj_plot_psr<-ggplot(subjpss, aes(x=score)) +
  geom_histogram(binwidth=1) + 
  ggtitle(paste("subj", i, sep="","_plot")) +
  scale_x_continuous(limits=c(-1,8))
indiv_subj_plot_psr

p<-ggplot(allsubjdata, aes(x=Probe.RT, color=as.factor(subjno)))+
         geom_density(position="identity",alpha=0.6)
p

###too clustered, need to spread out

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

multiplot(
subj6_plot,
subj7_plot,
subj10_plot, #this one actually shows an effect
subj11_plot, #maybe...
subj15_plot, #very strange distribution, but, technically could see an effect
#subj16_plot, # this one looks good
subj17_plot, #hard to interpret, but I think you could say there's the effect 
subj19_plot, #effect observed
subj20_plot, #nope...
subj22_plot, # definitely
subj24_plot, #no
subj25_plot, #no
subj26_plot, #definitely
subj27_plot, #plot is broken somehow
subj28_plot, #definite effect
subj30_plot, #maybe, though the SRP isn't catching it
subj31_plot, #maybe
subj32_plot, #maybe... pretty unclear
cols = 3)

