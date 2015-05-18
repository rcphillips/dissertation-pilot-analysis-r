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
setwd("~/Box Sync/SRP_AXCPT_pilot_data")
install.packages("ggplot2")
install.packages("RColorBrewer")
library(ggplot2)
library(RColorBrewer)

#Initializing variables
allsubjdata<-data.frame(NULL)
###actual code begins
#extract the data for all subjects
i=20
for  (i in c(6,7,9,10,11,13,15,16,17,18,19,20,22,24,25,26,27,28,30,31,32)){
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
  subjdata<-subset(subjdata, select = c(Probe.RT))# can add more later
#bring all subjects into same data frame
  subjdata[length(subjdata)+1]<-i # give a group variable
  allsubjdata<-rbind(allsubjdata,subjdata) #join with data from previous loops

indiv_subj_plot <- ggplot(subjdata, aes(x=Probe.RT,)) + 
  geom_density() + 
  ggtitle(paste("subj", i, sep="","_plot")) +
  scale_x_continuous(limits=c(0,2000)) +
  scale_y_continuous(limits=c(0,.008))
indiv_subj_plot
assign(paste("subj", i, sep="","_plot"),indiv_subj_plot)
}#end group processing
colnames(allsubjdata)[length(allsubjdata)]<-"subjno"
#plot RT on x axis
#plot count on y axis
hist(subjdata$Probe.RT, breaks = 100, ylim=c(0,5))



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
subj9_plot,
subj10_plot,
subj11_plot,
subj13_plot,
subj15_plot,
subj16_plot,
subj17_plot,
subj18_plot,
subj19_plot,
subj20_plot,
subj22_plot,
subj24_plot,
subj25_plot,
subj26_plot,
subj27_plot,
subj28_plot,
subj30_plot,
subj31_plot,
subj32_plot,
cols = 3)

