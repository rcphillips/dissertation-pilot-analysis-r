subjdata<-read.csv("SRP_33_eyedata.csv")
head(subjdata)
colnames(subjdata)<-c("sampleno","notsure1","notsure2pupil","notsure3","constant")
colnames(subjdata)
plot(as.numeric(subjdata$notsure1), xlim=c(1000,13000))
plot(as.numeric(subjdata$notsure2), xlim=c(1,29000))
plot(as.numeric(subjdata$notsure3))

plot(x=c(100000:200000),y=subjdata$X1241[100000:200000])
