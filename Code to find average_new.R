##################################################### Libraries

library("xts")
library("zoo")
library("matrixStats")

############################ Inputs to the code


path_input<-"D:/LFT/Template/tester/R test data/full simulation/data files/BASIS/"
path_output<-"D:/LFT/Template/tester/R test data/full simulation/data files/BASIS AVG/"
ff<-list.files(path_input)

lookback_period=c(20,40,60)
for(l in 1:length(lookback_period)){
for(i in 1:length(ff)){
  wq1<-read.csv(paste(path_input,ff[i],sep = ""),header = TRUE)
  dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],lookback_period[l],mean,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
   
  rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- 0
  rollfin<-data.frame(DATE=dt,rollfin)
  write.csv(rollfin,paste(path_output,"average of ",lookback_period[l],ff[i],sep = ""),row.names=FALSE)

  }
}

