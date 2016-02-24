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
allsubjresults<-data.frame(NULL, stringsAsFactors = FALSE)
#######################################
#group processing for cd_
for (i in 1:length(unique(cd_squares$CNTRACSID))){
  subjdata<-cd_squares[which(cd_squares$CNTRACSID==(unique(cd_squares$CNTRACSID)[i])),]
  #filter by corr trials
  subjdata<-subset(subjdata, subjdata$Resp.ACC==1)
  #get subj exgauss
  subjexgauss<-timefit(x=subjdata$Resp.RT, iter = 0, size = length(subjdata$Resp.RT),
                       replace = TRUE, plot = FALSE, start = NULL)
  #get all of the relevant parameters
  all_tau<-subjexgauss@par[3]
  all_sigma<-subjexgauss@par[2]
  all_mu<-subjexgauss@par[1]
  #get subj exgauss by trialtype
  #zerochange trials
  subjzero<-subset(subjdata,subjdata$ChangeCondition=="NoChange")
  subjzeroexgauss<-timefit(x=subjzero$Resp.RT, iter = 0, size = length(subjzero$Resp.RT),
                           replace = TRUE, plot = FALSE, start = NULL)
  zero_tau<-subjzeroexgauss@par[3]
  zero_sigma<-subjzeroexgauss@par[2]
  zero_mu<-subjzeroexgauss@par[1]
  #onechange trials
  subjone<-subset(subjdata,subjdata$ChangeCondition=="One")
  subjoneexgauss<-timefit(x=subjone$Resp.RT, iter = 0, size = length(subjone$Resp.RT),                   replace = TRUE, plot = FALSE, start = NULL)
  one_tau<-subjoneexgauss@par[3]
  one_sigma<-subjoneexgauss@par[2]
  one_mu<-subjoneexgauss@par[1]
  #twochange trials
  subjtwo<-subset(subjdata,subjdata$ChangeCondition=="Two")
  subjtwoexgauss<-timefit(x=subjtwo$Resp.RT, iter = 0, size = length(subjtwo$Resp.RT),
                          replace = TRUE, plot = FALSE, start = NULL)
  two_tau<-subjtwoexgauss@par[3]
  two_sigma<-subjtwoexgauss@par[2]
  two_mu<-subjtwoexgauss@par[1]
  #fivechange trials
  subjfive<-subset(subjdata,subjdata$ChangeCondition=="Five")
  subjfiveexgauss<-timefit(x=subjfive$Resp.RT, iter = 0, size = length(subjfive$Resp.RT),
                           replace = TRUE, plot = FALSE, start = NULL)
  five_tau<-subjfiveexgauss@par[3]
  five_sigma<-subjfiveexgauss@par[2]
  five_mu<-subjfiveexgauss@par[1]
  
  
  #get parameters in table
  subjname<-unique(cd_squares$CNTRACSID)[i]
  results<-cbind(subjname,all_mu,all_sigma,all_tau, 
                 zero_mu, zero_sigma, zero_tau,
                 one_mu, one_sigma, one_tau,
                 two_mu, two_sigma,two_tau,
                 five_mu, five_sigma,five_tau)
  allsubjresults<-rbind(allsubjresults,results)
}
#######################################
#group processing for cl_
for (i in 1:length(unique(cl_squares$CNTRACSID))){
  subjdata<-cl_squares[which(cl_squares$CNTRACSID==(unique(cl_squares$CNTRACSID)[i])),]
  #filter by corr trials
  subjdata<-subset(subjdata, subjdata$Resp.ACC==1)
  #get subj exgauss
  subjexgauss<-timefit(x=subjdata$Resp.RT, iter = 0, size = length(subjdata$Resp.RT),
                       replace = TRUE, plot = FALSE, start = NULL)
  #get all of the relevant parameters
  all_tau<-subjexgauss@par[3]
  all_sigma<-subjexgauss@par[2]
  all_mu<-subjexgauss@par[1]
  #get parameters in table
  subjname<-unique(cl_squares$CNTRACSID)[i]
  results<-cbind(subjname,all_mu,all_sigma,all_tau)
  allsubjresults<-rbind(allsubjresults,results)
}
#basic quality check
plot(x=c(1:dim(allsubjresults)[1]),
     y=as.numeric(as.character(allsubjresults$all_tau)),
     xlim=c(-1,dim(allsubjresults)[1]), 
     main="tau by subject, all corr trials",
     ylab="estimated tau value",
     xlab="subjno")

#saveout csv
write.csv(x=allsubjresults, "cl_squares_exg.csv", quote=FALSE,row.names=FALSE)
#################################
#checking out outliers, data quality
#this shows the max tau for the dataset
allsubjresults[which(allsubjresults$all_tau==max(as.numeric(as.character(allsubjresults$all_tau)))
),]
outlier_subj<-allsubjresults$subjname[which(allsubjresults$all_tau==max(as.numeric(as.character(allsubjresults$all_tau)))
)]

outlier_data<-cl_squares[which(cl_squares$CNTRACSID==as.character(outlier_subj)),]
#this shows you the plot of that subject
outlier_exgauss<-timefit(x=outlier_data$Resp.RT, iter = 100, size = length(outlier_data$Resp.RT),
                     replace = TRUE, plot = TRUE, start = NULL)

length(outlier_data$Resp.RT)

#loading back in the output
cd_shapes_exg<-read.csv("cd_shapes_exg.csv",stringsAsFactors=FALSE)
cd_squares_exg<-read.csv("cd_squares_exg.csv",stringsAsFactors=FALSE)
cl_shapes_exg<-read.csv("cl_shapes_exg.csv",stringsAsFactors=FALSE)
cl_squares_exg<-read.csv("cd_squares_exg.csv",stringsAsFactors=FALSE)
data<-cl_squares_exg
name<-"cl_squares_exg"
plot(x=c(1:dim(data)[1]),
     y=as.numeric(as.character(data$all_tau)),
     xlim=c(-1,dim(data)[1]), 
     main=name,
     ylab="tau by subject, all corr trials",
     xlab="subjno")
