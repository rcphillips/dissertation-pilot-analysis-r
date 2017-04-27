#getting the subjno in each complete_csv
setwd("/Users/rcphillips/Desktop/Dissertation/data/MRI_subj/complete_csvs")

subjno = '25'
allsubj_no_label<-data.frame('label',stringsAsFactors=FALSE)
allsubj_no_baked_in<-data.frame('baked-in',stringsAsFactors=FALSE)
allsubj_complete_date<-data.frame('date',stringsAsFactors=FALSE)
allsubj_task_date<-data.frame('date',stringsAsFactors=FALSE)
allsubj_psr_date<-data.frame('date',stringsAsFactors=FALSE)

for (i in c('01','02','03','04','05','06','07','08','09',10:24,26,29:33)){

subjno=i
subjdata_name<- paste("srp_", subjno, sep="","_all.csv")
subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
subj_complete_date <-subjdata$SessionStartDateTimeUtc[1]
allsubj_complete_date<-rbind(allsubj_complete_date,subj_complete_date)
#for task file
subjdata_name<- paste("srp_", subjno, sep="","_task.csv")
subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
subj_task_date <-subjdata$SessionStartDateTimeUtc[1]
allsubj_task_date<-rbind(allsubj_task_date,subj_task_date)
#for_psr_file
subjdata_name<- paste("srp_", subjno, sep="","_task.csv")
subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)
subj_psr_date <-subjdata$SessionStartDateTimeUtc[1]
allsubj_psr_date<-rbind(allsubj_psr_date,subj_psr_date)




subj_no_baked_in <-as.numeric(subjdata$Subject[1])
allsubj_no_baked_in<-rbind(allsubj_no_baked_in,subj_no_baked_in)

allsubj_no_label<-rbind(allsubj_no_label,as.numeric(i))
}
results<-cbind(allsubj_no_label,allsubj_no_baked_in,allsubj_complete_date,allsubj_task_date,allsubj_psr_date)
results

