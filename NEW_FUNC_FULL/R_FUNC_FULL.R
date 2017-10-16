

STOCK_CAP = function(fac2_rank_top2,fac2_rank_bot2){
  
  
  fac2_rank_top_old = fac2_rank_top2
  fac2_rank_bot_old = fac2_rank_bot2
  
  
  fac2_rank_top2[fac2_rank_top2 > stock_cap] = stock_cap  
  fac2_rank_bot2[fac2_rank_bot2 > stock_cap] = stock_cap
  
  top_weight_diff = fac2_rank_top2
  top_weight_diff[,]  =NA
  bot_weight_diff = top_weight_diff
  
  top_weight_diff1 = (rowSums(abs(fac2_rank_top_old),na.rm=TRUE) - rowSums(abs(fac2_rank_top2),na.rm=TRUE))
  bot_weight_diff1 = (rowSums(abs(fac2_rank_bot_old),na.rm=TRUE) - rowSums(abs(fac2_rank_bot2),na.rm=TRUE))
  
  top_weight_diff[,1:ncol(top_weight_diff)] = top_weight_diff1  
  bot_weight_diff[,1:ncol(bot_weight_diff)] = bot_weight_diff1
  
  #top_weight_sum             
  fac2_rank_top2_temp = fac2_rank_top2 
  fac2_rank_bot2_temp = fac2_rank_bot2          
  
  fac2_rank_top2_temp[fac2_rank_top2_temp >= stock_cap] = 0
  fac2_rank_top2_temp[is.na(fac2_rank_top2_temp)] = 0          
  
  fac2_rank_bot2_temp[fac2_rank_bot2_temp >= stock_cap] = 0
  fac2_rank_bot2_temp[is.na(fac2_rank_bot2_temp)] = 0          
  
  
  fac2_rank_top2_temp = fac2_rank_top2_temp + (fac2_rank_top2_temp)*(top_weight_diff)/rowSums(abs(fac2_rank_top2_temp),na.rm=TRUE)
  fac2_rank_bot2_temp = fac2_rank_bot2_temp + (fac2_rank_bot2_temp)*(bot_weight_diff)/rowSums(abs(fac2_rank_bot2_temp),na.rm=TRUE)
  fac2_rank_top2_temp[which(!is.finite(fac2_rank_top2_temp))] = 0        
  fac2_rank_bot2_temp[which(!is.finite(fac2_rank_bot2_temp))] = 0
  fac2_rank_top2[is.na(fac2_rank_top2)] = 0
  fac2_rank_bot2[is.na(fac2_rank_bot2)] = 0
  
  fac2_rank_top2[fac2_rank_top2 < stock_cap] = fac2_rank_top2_temp[fac2_rank_top2 < stock_cap]
  fac2_rank_bot2[fac2_rank_bot2 < stock_cap] = fac2_rank_bot2_temp[fac2_rank_bot2 < stock_cap]
  
  final_weight = list((fac2_rank_top2),(fac2_rank_bot2)) 
  return(final_weight)
  
}












###################################################################################################################################################################################################

library("xts")
library("zoo")
library("matrixStats")
library("TTR")
#library("roll")



###################################################################################################################################################################################################


ROLL_RSI = function(XX,N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")

  roll = data.frame(apply(wq1[,-1],n=N, 2, RSI))

  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}



ROLL_MACD = function(XX,NF,NS,NSIG,C){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  if(C==1)
  {
  
  roll = data.frame(apply(wq1[,-1],nFast = NF, nSlow = NS, nSig = NSIG, 2, MACD,percent = TRUE))[1:nrow(XX),]
  }
  
  if(C==2)
  {
    
    roll = data.frame(apply(wq1[,-1],nFast = NF, nSlow = NS, nSig = NSIG, 2, MACD,percent = TRUE))[(nrow(XX)+1):(2*nrow(XX)),]
  }
  
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}




EXP_MA = function(XX,N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  roll = (apply(wq1[,-1], 2, EMA,n=N,wilder = FALSE))
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}

EXP_WILDER_MA = function(XX,N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  roll = (apply(wq1[,-1], 2, EMA,n=N,wilder = TRUE))
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}

DOUBLE_EXP_MA = function(XX,N, WW) {
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  roll = (apply(wq1[,-1], 2, DEMA,n=N,wilder = WW,v=1))
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}

ZLE_MA = function(XX,N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  roll = (apply(wq1[,-1], 2, ZLEMA,n=N))
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}

H_MA = function(XX,N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  roll = (apply(wq1[,-1], 2, HMA,n=N))
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}

VWAP_MA = function(XX,YY,N){
  
  wq1<-(XX)
  wq2<-(YY)
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  roll = mapply(price=wq1[,-1],volume = wq2[,-1], VWAP,n=N)
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}

NEGATIVE = function(XX){
  
  return(data.frame(stck_lvl$Date,(-XX[,-1])))
  
} # NEGATIVE of XX, XX: FACTOR


DELAY = function(XX, N){
  
  DELAY_XX = XX[,-1]
  DELAY_XX[c(-1:-N),] =DELAY_XX[c((-dim(DELAY_XX)[1]):(-dim(DELAY_XX)[1]+N-1)),]
  return(data.frame(stck_lvl$Date,DELAY_XX))
  
}


DELTA = function(XX, N){
  
  
  DELAY_XX = XX[,-1]
  DELAY_XX[c(-1:-N),] =DELAY_XX[c((-dim(DELAY_XX)[1]):(-dim(DELAY_XX)[1]+N-1)),]
  
  DELTA_XX = (XX[,-1]-DELAY_XX)
  
  rollfin<-as.matrix(DELTA_XX)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


PER_CHANGE = function(XX, N){
  
  
  DELAY_XX = XX[,-1]
  DELAY_XX[c(-1:-N),] =DELAY_XX[c((-dim(DELAY_XX)[1]):(-dim(DELAY_XX)[1]+N-1)),]
  
  DELTA_XX = (XX[,-1]-DELAY_XX)/DELAY_XX
  
  rollfin<-as.matrix(DELTA_XX)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}

PER_CHANGE_NEW = function(XX, N){
  
  
  DELAY_XX = XX[,-1]
  DELAY_XX[c(-1:-N),] =DELAY_XX[c((-dim(DELAY_XX)[1]):(-dim(DELAY_XX)[1]+N-1)),]
  
  DELTA_XX = (XX[,-1]-DELAY_XX)/abs(XX[,-1]+DELAY_XX)
  
  rollfin<-as.matrix(DELTA_XX)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ZSCORE = function(XX,N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  m<-rollapply(wq1[,-1],N,mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  roll= (wq1[,-1]-m)/s
  
  rollfin <- data.frame(roll)
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
  
}


ZSCORE_NEW = function(XX,N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  m<-rollapply(wq1[,-1],N,mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  roll= (wq1[,-1]-m)/(1+s)
  
  rollfin <- data.frame(roll)
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
  
}


TS_RANK = function(XX,N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  roll = (apply(wq1[,-1], 2, runPercentRank,n=N,cumulative = FALSE, exact.multiplier=0.5))
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
  
}

ROLL_MAX =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],N,max,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ROLL_MIN =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],N,min,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}

ROLL_SUM =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],N,sum,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ROLL_MEAN =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],N,mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ROLL_SD =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ROLL_VAR =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],N,var,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ROLL_MEDIAN =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],N,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ROLL_ARGMAX =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-(N+1 - rollapply(wq1[,-1],N,which.max,fill=NA,align = "right",by.column=TRUE))
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}

ROLL_ARGMIN =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-(N+1 - rollapply(wq1[,-1],N,which.min,fill=NA,align = "right",by.column=TRUE))
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ROLL_ARG =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll1 <- (N+1 - rollapply(wq1[,-1],N,which.min,fill=NA,align = "right",by.column=TRUE))
  
  roll2 <- (N+1 - rollapply(wq1[,-1],N,which.max,fill=NA,align = "right",by.column=TRUE))
  
  roll  = (roll1-roll2)/(roll1+roll2)
  
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ROLL_UPCOUNT =  function(XX, N){
 
  DELTA_XX = DELTA(XX,1)[,-1]
  
  temp = DELTA_XX
  
  temp[] = 0
  
  temp[DELTA_XX>0] = 1
  
  roll <- rollapply(temp,N,sum,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ROLL_DNCOUNT =  function(XX, N){
  
  DELTA_XX = DELTA(XX,1)[,-1]
  
  temp = DELTA_XX
  
  temp[] = 0
  
  temp[DELTA_XX<0] = 1
  
  roll <- rollapply(temp,N,sum,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}

ROLL_UPDN =  function(XX, N){
  
  DELTA_XX = DELTA(XX,1)[,-1]
  
  temp = DELTA_XX
  
  temp[] = 0
  
  temp[DELTA_XX>0] = 1
  
  roll1 <- rollapply(temp,N,sum,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  
  temp[] = 0
  
  temp[DELTA_XX<0] = 1
  
  roll2 <- rollapply(temp,N,sum,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  
  roll = (roll1-roll2)/(roll1+roll2)
  
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}



TODAY_BY_ROLLMEAN =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],N,mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  rollfin<- wq1[,-1]/rollfin
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


TODAY_BY_ROLLMEDIAN =  function(XX, N){
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],N,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  rollfin<- wq1[,-1]/rollfin
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


RATIO_AVG =  function(XX,YY, N){
  
  
  wq1<-XX
  #dt<-wq1[1:nrow(wq1),1]
  roll1<-rollapply(wq1[,-1],N,mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin1 <- data.frame(roll1)
  
  wq2<-YY
  
  roll2<-rollapply(wq2[,-1],N,mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin2 <- data.frame(roll2)
  
  
  rollfin <- rollfin1/rollfin2
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


RANK =  function(XX,N){
  
  
  rollfin = (t(apply(XX[,-1], 1, rank,na.last="keep",ties.method="random")))
  
  rollfin = data.frame(rollfin/(apply(rollfin,1,max,na.rm=TRUE)))
  
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))
  
}


MAX =  function(XX,YY){
  
  WQ1 = XX[,-1]
  WQ2 = YY[,-1]
  rollfin =  WQ1[WQ1 < WQ2] = WQ2[WQ1 < WQ2]
  
  return(data.frame(stck_lvl$Date,rollfin))
  
}


MIN =  function(XX,YY){
  
  WQ1 = XX[,-1]
  WQ2 = YY[,-1]
  rollfin =  WQ1[WQ1 > WQ2] = WQ2[WQ1 > WQ2]
  
  return(data.frame(stck_lvl$Date,rollfin))
  
}


ROLL_CORRELATION = function(XX,YY,N){
  
  wq1<-(XX)
  wq2<-(YY)
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  roll = mapply(x=wq1[,-1],y = wq2[,-1], runCor,n=N,use = "all.obs", sample = TRUE, cumulative = FALSE)
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}

ROLL_COVARIANCE = function(XX,YY,N){
  
  wq1<-(XX)
  wq2<-(YY)
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  roll = mapply(x=wq1[,-1],y = wq2[,-1], runCov,n=N,use = "all.obs", sample = TRUE, cumulative = FALSE)
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}

ROLL_REGRESSION = function(YY,XX,N,RR){
  
  wq1<-as.matrix(XX[,-1])
  wq2<-as.matrix(YY[,-1])
  #dt<-wq1[1:nrow(wq1),1]
  #roll<-rollapply(wq1[,-1],N,rank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(wq1[,-1],N,runPercentRank,na.last="keep",fill=NA,align = "right",by.column=TRUE,ties.method="average")
  #roll<-rollapply(N,lm(wq2[,-1]~wq1[,-1])$coefficients,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  roll = data.frame(wq1)
  
  if(RR==0){
    
    for(jjj in 1:ncol(wq1)){
      
      roll[,jjj] = roll_lm(as.matrix(wq1[,jjj]),as.matrix(wq2[,jjj]),N)$r.squared
      
      #roll = mapply(x=as.matrix(wq1),y = as.matrix(wq2), roll_lm ,width=N)$r.squared
      
    }
    
  }
  
  if(RR==1){
    
    for(jjj in 1:ncol(wq1)){
      
      roll[,jjj] = data.frame(roll_lm(as.matrix(wq1[,jjj]),as.matrix(wq2[,jjj]),N))[,1]
      
      
      #roll = mapply(x=as.matrix(wq1),y = as.matrix(wq2), roll_lm ,width=N)$r.squared
      
    }
    
  }
  
  if(RR==2){
    
    for(jjj in 1:ncol(wq1)){
      
      roll[,jjj] = (data.frame(roll_lm(as.matrix(wq1[,jjj]),as.matrix(wq2[,jjj]),N))[,2])
      
      
      #roll = mapply(x=as.matrix(wq1),y = as.matrix(wq2), roll_lm ,width=N)$r.squared
      
    }
    
  }
  
  if(RR==3){
    
    for(jjj in 1:ncol(wq1)){
      
      roll[,jjj] = ((data.frame(roll_lm(as.matrix(wq1[,jjj]),as.matrix(wq2[,jjj]),N))[,2])*wq1[,jjj]) + data.frame(roll_lm(as.matrix(wq1[,jjj]),as.matrix(wq2[,jjj]),N))[,1]
      
      
      #roll = mapply(x=as.matrix(wq1),y = as.matrix(wq2), roll_lm ,width=N)$r.squared
      
    }
    
  }
  
  
  if(RR==4){
    
    for(jjj in 1:ncol(wq1)){
      
      roll[,jjj] = (((data.frame(roll_lm(as.matrix(wq1[,jjj]),as.matrix(wq2[,jjj]),N))[,2])*wq1[,jjj]) + data.frame(roll_lm(as.matrix(wq1[,jjj]),as.matrix(wq2[,jjj]),N))[,1]) - wq2[,jjj]
      
      
      #roll = mapply(x=as.matrix(wq1),y = as.matrix(wq2), roll_lm ,width=N)$r.squared
      
    }
    
  }
  
  
  
  
  
  
  #roll<-rollapply(wq1[,1],wq2[,1],N,lm,na.last="keep",fill=NA,align = "right",by.column=TRUE)
  #s<-rollapply(wq1[,-1],N,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  return(data.frame(stck_lvl$Date,rollfin))  
}

#regress = 






###################################################################################################################################################################################################




ann_rslt<-function(dtrettv){
  
  
  colnames(dtrettv)<-c("Date","retn","tv")
  
  dtrettv<-data.frame(cbind(dtrettv,cumsum(dtrettv$retn),substr(dtrettv$Date,1,4),substr(dtrettv$Date,6,7)))
  
  colnames(dtrettv)[4:6]<-c("cumsum","year","month")
  
  dtrettv<-data.frame(cbind(dtrettv,(cummax(dtrettv$cumsum)-dtrettv$cumsum)))
  colnames(dtrettv)[7]<-"dd"
  
  
  analysis<-data.frame(matrix(NA,8,length(unique(dtrettv$year))+1))
  
  colnames(analysis)[1:length(unique(dtrettv$year))]<-c(paste((unique(dtrettv$year))))
  colnames(analysis)[1+length(unique(dtrettv$year))]<-"Total"
  
  row.names(analysis)<-c("Return","Vol","Maxdd","Sharpe","Mtm/tv","Pos Months","Neg Months","trades")
  
  for(ij in 1:length(unique(dtrettv$year))){
    yr_int<-(unique(dtrettv$year))[ij]
    analysis[1,ij]<-sum(dtrettv$retn[dtrettv$year==yr_int])                   # Return
    analysis[2,ij]<-sd(dtrettv$retn[dtrettv$year==yr_int])*sqrt(250)          # Risk
    analysis[3,ij]<-max(dtrettv$dd[dtrettv$year==yr_int])                     # Maxdd
    analysis[4,ij]<-mean((dtrettv$retn[dtrettv$year==yr_int]))*sqrt(250)/sd(dtrettv$retn[dtrettv$year==yr_int])
    # Sharpe
    analysis[5,ij]<-sum(dtrettv$retn[dtrettv$year==yr_int])/sum(sum(dtrettv$tv[dtrettv$year==yr_int]))
    # Mtm/tv
    
    mon_lst<-unique(dtrettv$month[dtrettv$year==yr_int])
    n_mon<-length(mon_lst)
    
    p_mon_cnt<-0
    for (ijm in 1:n_mon){
      
      if(sum(dtrettv$retn[dtrettv$year==yr_int & dtrettv$month==mon_lst[ijm]])>=0){p_mon_cnt=p_mon_cnt+1}
      
      
    }
    
    analysis[6,ij]<-p_mon_cnt          # Number of positive months
    analysis[7,ij]<-n_mon-p_mon_cnt    # Number of negative months
    analysis[8,ij]<-sum(dtrettv$tv[dtrettv$year==yr_int])/2
  }
  
  
  # write.csv(allyr_facs,"C:/Users/ChetannC.EDELCAP/Desktop/Bahut_ho_gaya/Data/allyr_facs.csv")
  # write.csv(allyr_ret_ser,"C:/Users/ChetannC.EDELCAP/Desktop/Bahut_ho_gaya/Data/allyr_ret_ser.csv")
  
  analysis[1,length(unique(dtrettv$year))+1]<-mean(dtrettv$retn)*250
  analysis[2,length(unique(dtrettv$year))+1]<-sd(dtrettv$retn)*sqrt(250)
  analysis[3,length(unique(dtrettv$year))+1]<-max(dtrettv$dd)
  analysis[4,length(unique(dtrettv$year))+1]<-analysis[1,length(unique(dtrettv$year))+1]/analysis[2,length(unique(dtrettv$year))+1]
  analysis[5,length(unique(dtrettv$year))+1]<-sum(dtrettv$retn)/sum(dtrettv$tv)
  analysis[6,length(unique(dtrettv$year))+1]<-sum(analysis[6,1:length(unique(dtrettv$year))])
  analysis[7,length(unique(dtrettv$year))+1]<-sum(analysis[7,1:length(unique(dtrettv$year))])
  analysis[8,length(unique(dtrettv$year))+1]<-mean(dtrettv$tv)*125
  #mean(rowSums(ret_lft,na.rm=TRUE))*250^0.5/sd(rowSums(ret_lft,na.rm=TRUE))
  
  return(analysis)
  
}

###################################################################################################################################################################################################



ann_rslt1<-function(dtrettv){
  
  
  colnames(dtrettv)<-c("Date","retn","tv")
  
  dtrettv<-data.frame(cbind(dtrettv,cumsum(dtrettv$retn),substr(dtrettv$Date,1,4),substr(dtrettv$Date,6,7)))
  
  colnames(dtrettv)[4:6]<-c("cumsum","year","month")
  
  dtrettv<-data.frame(cbind(dtrettv,(cummax(dtrettv$cumsum)-dtrettv$cumsum)))
  colnames(dtrettv)[7]<-"dd"
  
  
  analysis<-data.frame(matrix(NA,8,length(unique(dtrettv$year))+1))
  
  colnames(analysis)[1:length(unique(dtrettv$year))]<-c(paste((unique(dtrettv$year))))
  colnames(analysis)[1+length(unique(dtrettv$year))]<-"Total"
  
  row.names(analysis)<-c("Return","Vol","Maxdd","Sharpe","Mtm/tv","Pos Months","Neg Months","trades")
  
  for(ij in 1:length(unique(dtrettv$year))){
    yr_int<-(unique(dtrettv$year))[ij]
    analysis[1,ij]<-sum(dtrettv$retn[dtrettv$year==yr_int])                   # Return
    analysis[2,ij]<-sd(dtrettv$retn[dtrettv$year==yr_int])*sqrt(250)          # Risk
    analysis[3,ij]<-max(dtrettv$dd[dtrettv$year==yr_int])                     # Maxdd
    analysis[4,ij]<-mean((dtrettv$retn[dtrettv$year==yr_int]))*sqrt(250)/sd(dtrettv$retn[dtrettv$year==yr_int])
    # Sharpe
    analysis[5,ij]<-sum(dtrettv$retn[dtrettv$year==yr_int])/sum(sum(dtrettv$tv[dtrettv$year==yr_int]))
    # Mtm/tv
    
    mon_lst<-unique(dtrettv$month[dtrettv$year==yr_int])
    n_mon<-length(mon_lst)
    
    p_mon_cnt<-0
    for (ijm in 1:n_mon){
      
      if(sum(dtrettv$retn[dtrettv$year==yr_int & dtrettv$month==mon_lst[ijm]])>=0){p_mon_cnt=p_mon_cnt+1}
      
      
    }
    
    analysis[6,ij]<-p_mon_cnt          # Number of positive months
    analysis[7,ij]<-n_mon-p_mon_cnt    # Number of negative months
    analysis[8,ij]<-sum(dtrettv$tv[dtrettv$year==yr_int])/2
  }
  
  
  # write.csv(allyr_facs,"C:/Users/ChetannC.EDELCAP/Desktop/Bahut_ho_gaya/Data/allyr_facs.csv")
  # write.csv(allyr_ret_ser,"C:/Users/ChetannC.EDELCAP/Desktop/Bahut_ho_gaya/Data/allyr_ret_ser.csv")
  
  analysis[1,length(unique(dtrettv$year))+1]<-mean(dtrettv$retn)*250
  analysis[2,length(unique(dtrettv$year))+1]<-sd(dtrettv$retn)*sqrt(250)
  analysis[3,length(unique(dtrettv$year))+1]<-max(dtrettv$dd)
  analysis[4,length(unique(dtrettv$year))+1]<-analysis[1,length(unique(dtrettv$year))+1]/analysis[2,length(unique(dtrettv$year))+1]
  analysis[5,length(unique(dtrettv$year))+1]<-sum(dtrettv$retn)/sum(dtrettv$tv)
  analysis[6,length(unique(dtrettv$year))+1]<-sum(analysis[6,1:length(unique(dtrettv$year))])
  analysis[7,length(unique(dtrettv$year))+1]<-sum(analysis[7,1:length(unique(dtrettv$year))])
  analysis[8,length(unique(dtrettv$year))+1]<-mean(dtrettv$tv)*125
  #mean(rowSums(ret_lft,na.rm=TRUE))*250^0.5/sd(rowSums(ret_lft,na.rm=TRUE))
  
  return(analysis)
  
}







###################################################################################################################################################################################################
# S is price seriers
# s is signal

mtm <- function(S,s){
  #S = price_stamps_day$Last
  #s = signal_buy_entry_exit
  #long.signals=which(s>0)
  #s[long.signals]=0
  
  sinitial=s
  s=zoo(s)
  s=na.locf(s,fromFirst = TRUE,na.rm=FALSE)
  to.zero=which(is.na(s))
  if(length(to.zero)) s[to.zero]=0
  #s=c(0,s)
  #sinitial=c(0,sinitial)
  s=as.numeric(s)
  
  
  tcostk = 0.02
  sfiinal=s
  #S=data[,3]
  S=S[((length(S)-length(sfiinal)+1):length(S))]
  ##########
  
  ##########
  i1=seq(2,length(sfiinal),1)
  mtm.1=ifelse(sfiinal[i1]!=sfiinal[i1-1],10000000*sfiinal[i1]/S[i1],NA)
  mtm.1=c(NA,mtm.1)
  
  mtm.1=zoo(mtm.1)
  mtm.1=na.locf(mtm.1,fromFirst = TRUE,na.rm=FALSE)
  to.zero=which(is.na(mtm.1))
  if(length(to.zero)) mtm.1[to.zero]=0
  mtm.1=as.numeric(mtm.1)
  mtm.11=100*((S[i1]-S[i1-1])*mtm.1[i1-1])/10000000-abs(sfiinal[i1]-sfiinal[i1-1])*tcostk
  mtm.11=c(0,mtm.11)
  mtm=mtm.11    
  #mtm=rep(0,length(ret))
  tv=rep(0,length(sfiinal))
  abc1=seq(1,length(sfiinal)-1,1)
  #mtm[abc1]=ret[abc1]*sfiinal[abc1]-abs(sfiinal[abc2]-sfiinal[abc1])*tcostk
  tv[abc1+1]=abs(sfiinal[abc1+1]-sfiinal[abc1])
  plot.ts(cumsum(mtm))
  mtm[is.na(mtm)] <- 0
  print(sum(mtm)/sum(tv))
  print(sum(tv)/2)
  
  return(cbind(mtm,tv))
}

tradesheet<-function(scpp,S,dd){
  mtm=scpp[,1]# column 1 has mtm
  tv=scpp[,2]# column 2 has tv
  s=sfiinal=scpp[,3]
  
  sum(mtm)
  
  ###
  #sum(rr1[,1])
  #sum(rr[,1])
  #
  #View(cbind(as.numeric(cumsum(rr1[,1])),as.numeric(cumsum(rr[,1]))))
  #rr=rbind(rep(0,ncol(rr)),rr)
  rr=cbind(mtm,tv,sfiinal)                
  
  newprice=S
  
  
  trades=rr[,3]
  tradepoints=which(trades[2:length(trades)]!=trades[1:(length(trades)-1)])+1
  tradetable=data.frame(matrix(NA,nrow=length(tradepoints),ncol=12))
  colnames(tradetable)[1]="EntryTime"
  colnames(tradetable)[2]="ExitTime"
  colnames(tradetable)[3]="NetReturn"
  colnames(tradetable)[4]="MtMReturn"
  colnames(tradetable)[5]="Position"
  colnames(tradetable)[6]="Duration"
  
  colnames(tradetable)[7]="MaxGainPnL"
  colnames(tradetable)[8]="MaxLossPnL"
  
  colnames(tradetable)[9]="MaxGainMTM"
  colnames(tradetable)[10]="MaxLossMTM"
  colnames(tradetable)[11]="EntryPrice"
  colnames(tradetable)[12]="ExitPrice"
  
  #dd.rr=dd[((length(dd)-nrow(rr)+1):length(dd))]
  ddfinal=dd[((length(dd)-length(mtm)+1):length(dd))]
  for(ijk in seq(1,length(tradepoints)-1,1)){
    
    if(as.numeric(rr[tradepoints[ijk],3])!=0){
      tradetable[ijk,11]=S[tradepoints[ijk]]#tradepoints[ijk]#
      tradetable[ijk,12]=S[tradepoints[ijk+1]]#tradepoints[ijk]#
      
      tradetable[ijk,1]=tradepoints[ijk]#as.character(ddfinal[tradepoints[ijk]])#
      tradetable[ijk,2]=tradepoints[ijk+1]#as.character(ddfinal[tradepoints[ijk+1]])#
      #tradetable[ijk,3]= as.numeric(rr[tradepoints[ijk],3])*((newprice[tradepoints[ijk+1]]/newprice[tradepoints[ijk]])-1)*100
      #if(ijk>1) tradetable[ijk,3]= (as.numeric(rr[tradepoints[ijk],3])*((newprice[tradepoints[ijk+1]]/newprice[tradepoints[ijk]])-1)*100)-abs((as.numeric(rr[tradepoints[ijk],3])-as.numeric(rr[tradepoints[ijk-1],3]))*tcostk) else tradetable[ijk,3]= as.numeric(rr[tradepoints[ijk],3])*((newprice[tradepoints[ijk+1]]/newprice[tradepoints[ijk]])-1)*100-(0.1)
      tradetable[ijk,3]= (as.numeric(rr[tradepoints[ijk],3])*((newprice[tradepoints[ijk+1]]/newprice[tradepoints[ijk]])-1)*100)
      
      #tradetable[ijk,4]=((newprice[tradepoints[ijk+1]]-newprice[tradepoints[ijk]])*as.numeric(rr[tradepoints[ijk],4]*rr[tradepoints[ijk],5]))/100000
      tradetable[ijk,5]=as.numeric(rr[tradepoints[ijk],3])
      tradetable[ijk,6]=tradepoints[ijk+1]-tradepoints[ijk]
      
      maxprice=max(newprice[tradepoints[ijk]:tradepoints[ijk+1]])
      minprice=min(newprice[tradepoints[ijk]:tradepoints[ijk+1]])
      
      maxgain=maxloss=0
      if(as.numeric(rr[tradepoints[ijk],3])==1) maxgain= as.numeric(((maxprice/newprice[tradepoints[ijk]])-1)*100) else maxgain= -as.numeric(((minprice/newprice[tradepoints[ijk]])-1)*100)
      if(as.numeric(rr[tradepoints[ijk],3])==1) maxloss= as.numeric(((minprice/newprice[tradepoints[ijk]])-1)*100) else maxloss= -as.numeric(((maxprice/newprice[tradepoints[ijk]])-1)*100)
      
      tradetable[ijk,7]=maxgain
      tradetable[ijk,8]=maxloss
      
      maxgainmtm=maxlossmtm=0
      #if(as.numeric(rr[tradepoints[ijk],3])==1) maxgainmtm= ((maxprice-newprice[tradepoints[ijk]])*as.numeric(rr[tradepoints[ijk],4]*rr[tradepoints[ijk],5]))/100000 else maxgainmtm= ((minprice-newprice[tradepoints[ijk]])*as.numeric(rr[tradepoints[ijk],4]*rr[tradepoints[ijk],5]))/100000
      #if(as.numeric(rr[tradepoints[ijk],3])==1) maxlossmtm= ((minprice-newprice[tradepoints[ijk]])*as.numeric(rr[tradepoints[ijk],4]*rr[tradepoints[ijk],5]))/100000  else maxlossmtm= ((maxprice-newprice[tradepoints[ijk]])*as.numeric(rr[tradepoints[ijk],4]*rr[tradepoints[ijk],5]))/100000
      
      
      tradetable[ijk,9]=maxgainmtm
      tradetable[ijk,10]=maxlossmtm
      
    }
    else {
      tradetable[ijk,1]=tradepoints[ijk]#as.character(dd.rr[tradepoints[ijk]])
      tradetable[ijk,2]=tradepoints[ijk+1]#as.character(dd.rr[tradepoints[ijk+1]])
      tradetable[ijk,3:10]=0
    }
    
  }
  
  tradetable=tradetable[-nrow(tradetable),]
  tradetable=tradetable[,c(1,2,3,5,11,12)]
  remove.trades.table=which(tradetable[,4]==0)
  if(length(remove.trades.table)>0) tradetable=tradetable[-remove.trades.table,]
  
  return(tradetable)
  
}

Fischer<-function(price_stamps_day,rolling_win){
  price_stamps_day <- price_stamps_day[order(price_stamps_day$DateTime),]
  i = seq(1,nrow(price_stamps_day),1)
  Close_Price = price_stamps_day$Close_Price
  min_rolling_Win = rollapply(Close_Price[i],rolling_win,min,align="right",fill=NA)
  max_rolling_Win = rollapply(Close_Price[i],rolling_win,max,align="right",fill=NA)
  temp = (Close_Price[i]-min_rolling_Win[i])/(max_rolling_Win[i]-0.99*min_rolling_Win[i]) 
  
  res1temp = 0.33 * 2 * (temp[i] - 0.5)
  res2temp = rep(0,length(res1temp))
  Fish = rep(0,length(res1temp))
  
  for(i in seq(rolling_win+1,length(res1temp),1)){
    res2temp[i]=res1temp[i]+(res2temp[i-1]*(2/3))
  }  
  
  res3temp <- data.table(res2temp)
  res3temp[res2temp>0.99,res2temp:=0.99]
  res3temp[res2temp<-0.99,res2temp:=-0.99]
  
  for(i in seq(rolling_win+1,length(res1temp),1)){
    Fish[i]= 0.5 * log((1 + res3temp$res2temp[i]) / (1 - res3temp$res2temp[i])) + 0.5 *Fish[i-1]
  } 
  
  return(Fish)
}  

Fischer1<-function(price_stamps,rolling_win){
  
  price_stamps_day = copy(price_stamps)
  price_stamps_day[,min_rolling_Win:= rollapply(Close_Price,rolling_win,min,align="right",fill=NA)]
  price_stamps_day[,max_rolling_Win:= rollapply(Close_Price,rolling_win,max,align="right",fill=NA)]
  
  price_stamps_day[,temp:= (Close_Price-min_rolling_Win)/(max_rolling_Win-0.99*min_rolling_Win)]
  
  min_rolling_Win = price_stamps_day$min_rolling_Win
  max_rolling_Win = price_stamps_day$max_rolling_Win
  temp = price_stamps_day$temp
  
  i = seq(1,nrow(price_stamps_day),1)
  
  res1temp = 0.33 * 2 * (temp[i] - 0.5)
  res2temp = rep(0,length(res1temp))
  Fish = rep(0,length(res1temp))
  
  for(i in seq(rolling_win+1,length(res1temp),1)){
    res2temp[i]=res1temp[i]+(res2temp[i-1]*(2/3))
  }  
  
  res3temp <- data.table(res2temp)
  res3temp[res2temp>0.99,res2temp:=0.99]
  res3temp[res2temp<-0.99,res2temp:=-0.99]
  
  for(i in seq(rolling_win+1,length(res1temp),1)){
    Fish[i]= 0.5 * log((1 + res3temp$res2temp[i]) / (1 - res3temp$res2temp[i])) + 0.5 *Fish[i-1]
  }
  rm(price_stamps_day)
  rm(min_rolling_Win)
  rm(max_rolling_Win)
  rm(res1temp)
  rm(res2temp)
  rm(res3temp)
  rm(temp)
  
  return(Fish)
}  

Bollinger <- function(price_stamps,roll_window,m)
{
  price_stamps_day <- copy(price_stamps)
  price_stamps_day$Fish.lag = c(NA,price_stamps_day$Fish[-nrow(price_stamps_day)])
  price_stamps_day$Last.lag = c(NA,price_stamps_day$Close_Price[-nrow(price_stamps_day)])
  
  price_stamps_day[,mean_Fish:= rollapply(Fish.lag,roll_window,mean,align="right",fill=NA)]
  price_stamps_day[,mean_Last:= rollapply(Last.lag,roll_window,mean,align="right",fill=NA)]
  
  price_stamps_day[,high_today:=max(Fish),by=c("current_date")]
  price_stamps_day[,low_today:=min(Fish),by=c("current_date")]
  
  price_stamps_day[,Range_today:=(high_today-low_today)]
  price_stamps_day$Range_yest = c(NA,price_stamps_day$Range_today[-nrow(price_stamps_day)])
  
  price = price_stamps_day$Close_Price
  current_date = price_stamps_day$current_date
  Range_yest = price_stamps_day$Range_yest
  Range_today = price_stamps_day$Range_today
  current_time = price_stamps_day$current_time
  
  j=2
  while(j <= nrow(price_stamps_day))
  {
    if(current_date[j]==current_date[j-1])
    {
      Range_yest[j] = Range_yest[j-1]
    }
    else
    {
      Range_yest[j] = Range_today[j-1]
    }
    j = j+1
    
  }
  price_stamps_day$Range_yest = Range_yest
  
  price_stamps_day[,signal_buy_entry:=ifelse(((Fish>(mean_Fish + Range_yest)) & (Close_Price>mean_Last)),1,0)]
  price_stamps_day[is.na(signal_buy_entry),signal_buy_entry:=0]
  sum(price_stamps_day$signal_buy_entry,na.rm = T)  
  price_stamps_day[current_time>="15:00:00",signal_buy_entry:=0]
  
  price_stamps_day[,signal_sell_entry:=ifelse(((Fish<(mean_Fish-Range_yest)) & (Close_Price<mean_Last)),-1,0)]
  price_stamps_day[is.na(signal_sell_entry),signal_sell_entry:=0]
  sum(price_stamps_day$signal_sell_entry,na.rm = T)  
  price_stamps_day[current_time>="15:00:00",signal_sell_entry:=0]
  
  signal_buy_entry_exit = price_stamps_day$signal_buy_entry
  signal_buy_entry = price_stamps_day$signal_buy_entry
  ssignal_sell_entry_exit = price_stamps_day$signal_sell_entry
  signal_sell_entry = price_stamps_day$signal_sell_entry
  
  i=12;
  while(i <= nrow(price_stamps_day))
  { 
    if(current_time[i]=="15:00:00")
    {
      close_price = price[i]
    }
    
    if(signal_buy_entry_exit[i] == 1) 
    { 
      max = price[i]
      j=i+1
      while((price[j]>=(1-m/100)*max & j<=(nrow(price_stamps_day))) & !(current_time[j]=="15:00:00" & (price[j] < close_price) & (current_date[j]!=current_date[i])) & signal_sell_entry[j]!=-1)
      {
        if(price[j]>max)
        {
          max = price[j]
        }
        if(current_time[j]=="15:00:00")
        {
          close_price = price[j]
        }  
        signal_buy_entry_exit[j]=1;
        j=j+1;
      }
      i = j;
    }
    i = i+1;
  }  
  sum(signal_buy_entry_exit)
  signal_buy_mtm <- mtm(price_stamps_day$Close_Price,signal_buy_entry_exit)
  colnames(signal_buy_mtm) <- c("signal_buy_mtm","buy_tv")
  
  i=12;
  while(i <= nrow(price_stamps_day))
  { 
    if(current_time[i]=="15:00:00")
    {
      close_price = price[i]
    }
    if(ssignal_sell_entry_exit[i] == -1) 
    { 
      min = price[i]
      j=i+1
      while((price[j]<=(1+m/100)*min & j<=(nrow(price_stamps_day))) & !(current_time[j]=="15:00:00" & (price[j] > close_price) & (current_date[j]!=current_date[i]) ) & signal_buy_entry[j]!=1)
      {
        if(price[j]<min)
        {
          min = price[j]
        }
        if(current_time[j]=="15:00:00")
        {
          close_price = price[j]
        }
        ssignal_sell_entry_exit[j]=-1;
        j=j+1;
      }
      i = j;
    }
    i = i+1;
  }  
  sum(ssignal_sell_entry_exit)
  ssignal_sell_mtm <- mtm(price_stamps_day$Close_Price,ssignal_sell_entry_exit)
  colnames(ssignal_sell_mtm) <- c("ssignal_sell_mtm","sell_tv")
  
  x <- cbind(price_stamps_day,signal_buy_entry_exit,signal_buy_mtm,ssignal_sell_entry_exit,ssignal_sell_mtm)
  x[,Total_mtm:=(signal_buy_mtm+ssignal_sell_mtm)]
  x[,Total_trades:=(sell_tv+buy_tv)/2]
  rm(price_stamps_day)
  rm(price_stamps)
  return(x)
}  
