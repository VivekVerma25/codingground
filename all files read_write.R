# This is the code for creating return, given stock list and weights , every rebalancing date

##################################################### Libraries

library("xts")
library("zoo")
library("matrixStats")

############################ Inputs to the code
setwd("D:/LFT/Template/tester/R test data/full simulation")
path_input<-"D:/LFT/Template/tester/R test data/full simulation/data files/"
path_output<-"D:/LFT/Template/tester/R test data/full simulation/output/"


dir_list = list.dirs(path_input)
names = c("dir","files")
ff<-list.files(dir_list[6])
qw<-data.frame(dir_list[6],ff)
colnames(qw)<-names
final<-qw
i=3
for(i in 3:(length(dir_list))){
  ff<-list.files(dir_list[i]) 
  qw<-data.frame(dir_list[i],ff)
  colnames(qw)<-names
  final<-rbind(final,qw)
}

final[,1]=as.character(final[,1])
final[,2]=as.character(final[,2])
final[,2]=(substr(final[,2],1,nchar(final[,2])-4))

write.csv(final,paste(path_input,"ANALYST ZSCORE.csv"),row.names=FALSE)
