#optseq2eprime
#Ryan Phillips
#150419
###
#This is a simple script which takes the .csvs which result from
#MANUALLY converting the .pars which come out of optseq to .csvs
#using default settings with Excel. This just shuffles the columns and 
#converts the seconds to milliseconds.
###
#load csv
#make column 5 column 1
#make column 2 column 3
#multiply column 2 and 3 by 1000
#save as a csv
#whoops, it's weirder than I thought.
#for col2 = 0, populate col 3. Current col3 doesn't exist. it's a dummy varible

###
i=1
for (i in c(1:6)){
filename_orig<- paste("rphillips_optseq_", i, sep="",".csv")
file<-read.csv(filename_orig,stringsAsFactors=FALSE,header=FALSE)
colnames(file)<-c(1,2,3,4,5)
newfile<-data.frame(file[,5],file[,3],file[,2]) #col3 here must be changed:
colnames(newfile)<-c("col1","col2","col3")
for (j in c(1:nrow(newfile))){
    if(newfile[j,3]==0){newfile[j-1,3]<-newfile[j,2]}
}
newfile[,2:3]<-newfile[,2:3]*1000

endfile<-
  newfile[seq(from=1, to=nrow(newfile),by=2),1:3]
filename_final<- paste("file", i, sep="",".csv")
assign(filename_final,endfile)
write.table(endfile, file = filename_final,row.names=FALSE,col.names=FALSE, sep=",", quote=FALSE)
}