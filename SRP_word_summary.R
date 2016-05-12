#SRP_word_summary.R
#160511
#R.Phillips
#Goal: Get the measure of how often subjects are affirming that the word presented 
#describes them
setwd("/Users/rcphillips/Box Sync/Proj_SRPAX/complete_csvs")
allsubjresult<-data.frame(NULL)
i='02'
for (i in c('01','02','03','04','05','06','07','08','09',10:15,17:24,26,29:33)){
subjno=i
subjdata_name<- paste("srp_", subjno, sep="","_all.csv")
subjdata<-read.csv(subjdata_name,stringsAsFactors=FALSE)

word_endorsement<-subjdata$WordResponse.RESP
#remove the '5' t-pulses from the scanner
word_endorsement<-as.numeric(gsub(5,'',word_endorsement))
word_affirmed<-table(word_endorsement,useNA='ifany')[[1]]
word_denied<-table(word_endorsement,useNA='ifany')[[2]]
indeterminate<-length(which(word_endorsement!=1 & word_endorsement!=2,))
no_resp<-sum(is.na(word_endorsement))
subjresult<-c(as.numeric(subjno),as.numeric(word_affirmed),as.numeric(word_denied),as.numeric(indeterminate),as.numeric(no_resp))
allsubjresult<-rbind(allsubjresult,subjresult)
}
colnames(allsubjresult)<-c('subjno','word_affirmed','word_denied','indeterminate','no_resp')

setwd('/Users/rcphillips/Desktop')
write.csv(allsubjresult, file='words_affirmed.csv',row.names=FALSE)
