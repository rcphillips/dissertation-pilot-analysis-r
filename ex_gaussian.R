#ex-gaussian modelling for SRP-AXCPT
#R. Phillips
#150716
#The goal here is to model subjects RTs, and then fit that to a split linear model,
#starting with a gaussian compenent and then shifting to a negative exponential component.
###Steps:
#Bring in a single subject's RT distribution
#visualize
#Model a gaussian component
#visuzalize
#Model a gaussian component and a negative exponential component
#iteratively pick the best split point for the data
#split the data by SRP, and repeat this process for both parts
#repeat for all subjects
###
###Housekeeping:
#IRC
setwd("C:/Users/rphillips/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
library(ggplot2)
###
#Bring in a single subject's RT distribution
subjdata<-read.csv('subj39_task.csv')
#removing incorrect trials
subjdata<-subset(subjdata, subjdata$Cue.ACC=="1" & subjdata$Probe.ACC=="1" & subjdata$TrialType=="AX") 
##visualize
#first, produce a histogram
plot_title<-c("Probe RT")
plot<-ggplot(subjdata, aes(x=Probe.RT)) + 
  geom_histogram(fill="white", colour="black", binwidth=30) +
  #xlim(-1,2000) +
  xlab("Probe RT")+
  #ylim(-.001,100) +
  ylab("Count")+
  ggtitle(plot_title) +
  theme(strip.text.y=element_text(size=30), axis.title=element_text(size=30), 
        plot.title=element_text(size=30))
#next, extract the histogram counts from it
hist_counts<-ggplot_build(plot)
xvar<-as.numeric(unlist(hist_counts$data[[1]][11]))
xvar<-(xvar-mean(xvar))#centering data
yvar<-as.numeric(unlist(hist_counts$data[[1]][2]))
yvar<-(yvar-mean(yvar))#centering data
#then, remodel it:
plot(x=xvar,y=yvar)
#Model a gaussian component
###So this is from that stats class, needs to be modified
#Create segments of x1 for model
subjdata$seg_marker1a <- subjdata$Probe.RT
subjdata$seg_marker1b <- subjdata$Probe.RT
###^_^ So this is how you set up a spline
###^_^ You define your inflection point
###^_^ And you split it up
subjdata$seg_marker1a[which(subjdata$seg_marker1a > mean(subjdata$seg_marker))] <- 0
subjdata$seg_marker1b[which(subjdata$seg_marker1a <= mean(subjdata$seg_marker))] <- 0


m2s1 <- lm(Probe.RT ~ seg_marker1a + seg_marker1b, data = subjdata)
summary(m2s1)

#visuzalize
#Model a gaussian component and a negative exponential component
#iteratively pick the best split point for the data
#split the data by SRP, and repeat this process for both parts
#repeat for all subjects
###