#DBarch_Data_Exploration
#R.Phillips
#151015
#Goal: Just find out what these columns are.
###
#change to working dir
#load csv
###
#change to working dir
#laptop
setwd("C:/Users/rphillips/Desktop/DBarch_data/csvs")
#flashdriveonCNS
setwd("F:/DBarch_data/csvs")
#libraries
library(ggplot2)
install.packages('retimes')
library(retimes)
###
#load csvs
cd_shapes<-read.csv("CD_Shapes_061815_small.csv",stringsAsFactors=FALSE)
cd_squares<-read.csv("CD_Squares_061815_small.csv",stringsAsFactors=FALSE)
cl_shapes<-read.csv("CL_Shapes_061815_small.csv",stringsAsFactors=FALSE)
cl_squares<-read.csv("CL_Squares_061815_small.csv",stringsAsFactors=FALSE)
#list column names
colnames(cd_shapes)
#one of em looks like IDs
table(cd_shapes$CNTRACSID)
length(table(cd_shapes$CNTRACSID))
#Looks like 227 subjects of 240 trials each
length(table(cd_squares$CNTRACSID))
#226 of 240 trials each
length(table(cl_shapes$CNTRACSID))
#222 of 60 trials each
length(table(cl_squares$CNTRACSID))
#225 of 60 trials each

#so! Looks like 900 subjects. That's cool.

#let's focus on the RTs of just one subject from one dataset.
#cd_shapes is the dataset
#get one subject

subjdata<-cd_shapes[which(cd_shapes$CNTRACSID==(unique(cd_shapes$CNTRACSID)[1])),]
cl_subjdata<-cl_shapes[which(cl_shapes$CNTRACSID==(unique(cl_shapes$CNTRACSID)[1])),]
table(cl_subjdata$Resp.ACC)
#get accuracies by trialtype
subj5<-table(subjdata$ChangeCondition,subjdata$Resp.ACC)[1,]
subj1<-table(subjdata$ChangeCondition,subjdata$Resp.ACC)[3,]
subj2<-table(subjdata$ChangeCondition,subjdata$Resp.ACC)[4,]
subj0<-table(subjdata$ChangeCondition,subjdata$Resp.ACC)[2,]
subjresults<-rbind(subj0,subj1,subj2,subj5)
subjresults[,1]
plot(x=c(0,1,2,5),y=subjresults[,2],ylim=c(0,70),xlim=c(-1,6), main="subject accuracy by condition",ylab="Correct Trials", xlab="Number of Objects Changed")
lines(x=c(0,1,2,5),y=subjresults[,2])
#filter by corr trials
subjdata<-subset(subjdata, subjdata$Resp.ACC==1)
#get subj exgauss
subjexgauss<-timefit(x=subjdata$Resp.RT, iter = 0, size = length(subjdata$Resp.RT),
                               replace = TRUE, plot = TRUE, start = NULL)
all_tau<-subjexgauss@par[3]
#get subj exgauss by trialtype
subjzero<-subset(subjdata,subjdata$ChangeCondition=="NoChange")
subjzeroexgauss<-timefit(x=subjzero$Resp.RT, iter = 0, size = length(subjzero$Resp.RT),
                         replace = TRUE, plot = TRUE, start = NULL)
zero_tau<-subjzeroexgauss@par[3]
subjone<-subset(subjdata,subjdata$ChangeCondition=="One")
subjoneexgauss<-timefit(x=subjone$Resp.RT, iter = 0, size = length(subjone$Resp.RT),
                         replace = TRUE, plot = TRUE, start = NULL)
one_tau<-subjoneexgauss@par[3]
subjtwo<-subset(subjdata,subjdata$ChangeCondition=="Two")
subjtwoexgauss<-timefit(x=subjtwo$Resp.RT, iter = 0, size = length(subjtwo$Resp.RT),
                         replace = TRUE, plot = TRUE, start = NULL)
two_tau<-subjtwoexgauss@par[3]
subjfive<-subset(subjdata,subjdata$ChangeCondition=="Five")
subjfiveexgauss<-timefit(x=subjfive$Resp.RT, iter = 0, size = length(subjfive$Resp.RT),
                         replace = TRUE, plot = TRUE, start = NULL)
five_tau<-subjfiveexgauss@par[3]

results<-cbind(all_tau,zero_tau,one_tau,two_tau,five_tau)

plot(x=c(-1, 0,1,2,5),y=results[1,],ylim=c(0,300),xlim=c(-2,6), main="tau by condition",ylab="estimated tau value", xlab="Number of Objects Changed")
lines(x=c(-1, 0,1,2,5),y=results[1,])

#K is a means of quantifying the shape of the distribution.
#it's tau divided by sigma

K <-(subjoneexgauss@par[3]/subjoneexgauss@par[2])
K
