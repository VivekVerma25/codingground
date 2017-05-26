##################################################### Libraries

library("xts")
library("zoo")
library("matrixStats")

############################ Inputs to the code


path_input<-"D:/LFT/Template/tester/R test data/full simulation/data files/OPTIONS DATA/"
path_output<-"D:/LFT/Template/tester/R test data/full simulation/data files/OPTIONS DATA AVG/"
ff<-list.files(path_input)

checkdim =0
lookback_period=c(5,20,40,60)
for(l in 1:length(lookback_period)){
  
  for(i in 1:length(ff)){
  wq1<-read.csv(paste(path_input,ff[i],sep = ""),header = TRUE)
  dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],lookback_period[l],mean,fill=NA,align = "right")
  rollfin <- data.frame(roll)
  # for(j in 3:ncol(wq1)){
  #   roll<-rollapply(wq1[,j],lookback_period[l],mean,fill=NA,align = "right")
  #   rollfin<-cbind(rollfin,roll)
  # }
  rollfin<-rollfin[1:(nrow(rollfin)-1),]
  wq1<-wq1[2:(nrow(wq1)),2:ncol(wq1)]
  fin<-wq1/rollfin
  fin[is.na(fin)] =0
  fin<-as.matrix(fin)
  fin[which(!is.finite(fin))] <- 0
  temp<-t(data.frame(rep(0,ncol(fin))))
  colnames(temp)<-colnames(temp)
  temp<-rbind(temp,fin)
  fin<-temp
  fin<-data.frame(DATE=dt,fin)
  checkdim[i*l] = paste("lookback ",lookback_period[l],ff[i],sep = "",dim(fin)[1],dim(fin)[2])
  write.csv(fin,paste(path_output,"lookback ",lookback_period[l],ff[i],sep = ""),row.names=FALSE)
}
}