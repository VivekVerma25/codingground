##################################################### Libraries

library("xts")
library("zoo")
library("matrixStats")

############################ Inputs to the code


path_input<-"D:/LFT/Template/tester/R test data/full simulation/data files/OPTIONS DATA/"
path_output<-"D:/LFT/Template/tester/R test data/full simulation/data files/OPTIONS DATA AVG/"
ff<-list.files(path_input)

lookback_period=c(10,20)
for(l in 1:length(lookback_period)){
  
  wq1<-read.csv(paste(path_input,ff[i],sep = ""),header = TRUE)
  dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],lookback_period[l],mean,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  # for(j in 3:ncol(wq1)){
  #   roll<-rollapply(wq1[j],lookback_period[l],mean,fill=NA,align = "right")
  #   rollfin<-cbind(rollfin,roll)
  # }
  
  rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- 0
  # rollfin<-data.frame(DATE=dt,rollfin)
  
  wq1<-read.csv(paste(path_input,ff[i+1],sep = ""),header = TRUE)
  dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],lookback_period[l],mean,fill=NA,align = "right",by.column=TRUE)
  rollfin1 <- data.frame(roll)
  # for(j in 3:ncol(wq1)){
  #   roll<-rollapply(wq1[j],lookback_period[l],mean,fill=NA,align = "right")
  #   rollfin1<-cbind(rollfin1,roll)
  # }
  
  rollfin1[is.na(rollfin1)] =0
  rollfin1<-as.matrix(rollfin1)
  rollfin1[which(!is.finite(rollfin1))] <- 0
  
  rollfin1<-rollfin/rollfin1
  rollfin1[is.na(rollfin1)] =0
  rollfin1<-as.matrix(rollfin1)
  rollfin1[which(!is.finite(rollfin1))] <- 0
  
  
  rollfin1<-data.frame(DATE=dt,rollfin1)
  
  
  
  
  write.csv(rollfin1,paste(path_output,"ratio of average ",lookback_period[l],substr(ff[i],1,nchar(ff[i])-4)," to average ",ff[i+1],sep = ""),row.names=FALSE)
  
}

