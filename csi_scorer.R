#Couple's Satisfaction Index Scorer
#R.Phillips
#160413

#Goal here is to have a single score which represents couple's satisfaction.
###
#access the CSV
#extract the CSI columns for a single subject
#sum them based on the document Katherine sent me
#produce an appropriate score
###
#to do: follow up with Katherine on it. Something about Cohen's D?
###
#housekeeping:
library("sqldf")
#at CNS
setwd("~/Desktop/Eharm")
###
#access the CSV
data<-read.csv("all_baseline_husband.csv")
#extract the CSI columns
data2<-data.frame(
        data$CUSTOMID,
        data$COUPLEID,
        data$csifr_01_bl_h,data$csifr_02_bl_h,data$csifr_03_bl_h,
        data$csifr_04_bl_h,data$csifr_05_bl_h,data$csifr_06_bl_h,
        data$csifr_07_bl_h,data$csifr_08_bl_h,data$csifr_09_bl_h,
        data$csifr_10_bl_h,data$csifr_11_bl_h, data$csifr_12_bl_h,
        data$csifr_13_bl_h,data$csifr_14_bl_h, data$csifr_15_bl_h,
        data$csifr_16_bl_h,data$csifr_17_bl_h,data$csifr_18_bl_h,data$csifr_18b_bl_h,
        data$csifr_19_bl_h,data$csifr_20_bl_h,data$csifr_21_bl_h,
        data$csifr_22_bl_h,data$csifr_23_bl_h,data$csifr_24_bl_h,
        data$csifr_25_bl_h)
#check them out for a single subject
data2[which(data2$data.CUSTOMID==1283),]
#score them appropriately based on the document Katherine sent to slack
#dropping one column which was added in by eharmony people
drop<-c("data.csifr_18b_bl_h")
data2<-data2[, !(names(data2) %in% drop)]
#looks like we can just average their scores 
#produce an appropriate score
data2$csi_avg<-rowMeans(data2[3:27], na.rm=TRUE)
#merge that back into the main data
data$csi_avg<-data2$csi_avg
#visualize results, using
#"agree disagree, how much is 'feeling in love' a problem in your relationship?"
#1 = not a problem, 7 = major problem
#how's it correlate?
cor(as.numeric(data$csi_avg),as.numeric(data$imp21_bl_h), use="pairwise.complete.obs")
#Looks bad. Is it a significant predictor of csi_avg score?
model1<-(lm(data$csi_avg ~ data$imp21_bl_h))
summary(model1)
#it is.
plot(data$csi_avg,data$imp21_bl_h, 
     xlab="Avg Satisfaction",
     ylab="How much is feeling in love a problem?",
     yaxt = 'n',
     xlim=c(0,6),
     ylim=c(0,8))
axis(2, at=c(1,4,7), labels=c("not at all","medium problem","major problem"))
abline(model1)
#note:
#no one says it's more than a little more than a major problem! That's nice.
#Seems fair to say the effect is being driven by that large "medium problem" group being
#just a bit lower than the "not at all" group
table(data$imp21_bl_h)

