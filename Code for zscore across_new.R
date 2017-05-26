path_input<-"D:/LFT/Template/tester/R test data/full simulation/data files/ANALYST DATA/"
path_output<-"D:/LFT/Template/tester/R test data/full simulation/data files/ANALYST DATA ZSCORE/"
ff<-list.files(path_input)


lookback_period=c(10,20,60)

  
for(i in 1:length(lookback_period)){
  for(k in 1:length(ff)){
    pric<-read.csv(paste(path_input,ff[k],sep = ""),header = TRUE)
    test<-pric[,-1]
    m<-rollapply(test,lookback_period[i],mean,na.rm=TRUE,align="right",by.column = FALSE)
    s<-rollapply(test,lookback_period[i],sd,na.rm=TRUE,align="right",by.column = FALSE)
     
s[is.na(s)] =0
s[which(!is.finite(as.matrix(s)))] <- 0

m[is.na(m)] =0
m[which(!is.finite(as.matrix(m)))] <- 0


zpcr<-(pric[,-1]-m)/s
zpcr<-as.matrix(zpcr)
zpcr[is.na(zpcr)] =0
zpcr[which(!is.finite((zpcr)))]=0
zpcr<-data.frame(DATE= pric[,1],zpcr)
write.csv(zpcr,paste(path_output,"zscore of ",lookback_period[i],ff[k],sep = ""),row.names=FALSE)
}
}