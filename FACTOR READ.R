
library("xts")
library("zoo")

############################ Inputs to the code
setwd("D:/LFT/Template/tester/R test data/Factor List/")
path_input<-"D:/LFT/Template/tester/R test data/Factor List/"
path_output<-"D:/LFT/Template/tester/R test data/Factor List/output/" 

f= data.frame(read.csv("FACTOR LIST.csv",))
f1=data.frame(read.csv(paste(path_input,f[1,1],".csv",sep="")))


for( i in 1:dim(f)[1])
{
  f1=data.frame(read.csv(paste(path_input,f[i,1],".csv",sep="")))
  write.csv(f1,file=paste(path_output,f[i,1],".csv",sep=""))
}