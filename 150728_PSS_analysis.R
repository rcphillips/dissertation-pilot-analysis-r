#This script is design to generate a series of plots describing subject's personal significance
#ratings.
#It takes csvs, saved out of excel as .csvs in the format:
#subj#_pss.csv
#refer to the lucid chart if I've gotten around to making it yet for more specific conversion details
#Ryan C. Phillips, 141028
########################################################################
########################################################################
#first, for a single subject
#load the csv
#trim it so that it looks nice
#get the wayward data all into the same column
#plot the data in a histogram
#conduct a paired, one tailed t-test on low_AY_errs versus high_AY_errs
########################################################################
########################################################################
###Housekeeping:
#IRC
#setwd("C:/Users/rphillips/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
#CNS
setwd("~/Box Sync/Proj_SRPAX/Data_SRPAX_pilotsubjs_behavonly")
###
#load and name the csv
subjno=33
subjpss_name<- paste("subj", subjno, sep="","_pss.csv")
subjpss<-read.csv(subjpss_name,stringsAsFactors=FALSE)
#trim it so that it looks nice 
#(for mri subjs)
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
}
clean_subj_pss<-data.frame(word,score,reaction_time)
#remove missing values
clean_subj_pss<-na.omit(clean_subj_pss)
#plot the data in a barplot
subj_name<- paste("subj", subjno, sep="","PersonalSailienceRatings")
barplot(table(clean_subj_pss$score), main=subj_name, xlim=c(0,9), ylim = c(0,110))

