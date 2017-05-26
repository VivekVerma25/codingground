library(zoo)
library(xtx)
library(TTR)
library(timeSeries)
library(slam)
library(quantmod)
library(quantreg)
library(allelematch)

path_input<-"C:/Users/YashV/Desktop/Framework Data/"
path_output<-"C:/Users/YashV/Desktop/Framework Data/"
stck_lvl<-data.frame(read.csv(paste(path_input,"Roll adjusted returns.csv",sep="")))

stck_lvl$Returns<-as.Date(stck_lvl$Returns,"%d/%m/%Y")

tc<-(0.0004)                                # This is one side trnsaction cost 
cs=5 #min of 5 stocks should be there or else no trade
fa=1 #whether fixed allocation or not
#########################################################################################################################################################

lookback_period=c(5,20,60,120,200)
##############################################
for(li in 1:5)
{
  
  pric<-read.csv(paste(path_input,"BEST_CPS.csv",sep=""))
  clafic<-data.frame(read.csv(paste(path_input,"TOP 100 ANALYST.csv",sep="")))[,-1]
  stck_lvl<-data.frame(read.csv(paste(path_input,"Roll adjusted returns.csv",sep="")))
  
  stck_lvl$Returns<-as.Date(stck_lvl$Returns,"%d/%m/%Y")
  names<-colnames(pric)
  dt<-pric[(lookback_period[li]+1):nrow(pric),1]
                  
  pric<-pric[,-1] # Factor
  test1<-pric[(lookback_period[li]+1):nrow(pric),] # Exit Factor
  
  for(j in 1:ncol(pric)){       # To find mean
    if(j==1) {
      roll<-rollapply(pric[,j],lookback_period[li],mean,na.rm=TRUE,fill=NA,align = "right")
      m<-data.frame(roll)
    }
    else{
      roll<-rollapply(pric[,j],lookback_period[li],mean,na.rm=TRUE,fill=NA,align = "right")
      m<-cbind(m,roll)
    }
  }
  
  for(j in 1:ncol(pric)){   # To find STDDEV
     if(j==1) {
      roll<-rollapply(pric[,j],lookback_period[li],sd,na.rm=TRUE,fill=NA,align = "right")
      s<-data.frame(roll)
    }
    else{
      roll<-rollapply(pric[,j],lookback_period[li],sd,na.rm=TRUE,fill=NA,align = "right")
      s<-cbind(s,roll)
    }
  }
  s[is.na(s)] =0
  s[which(!is.finite(as.matrix(s)))] <- 0
  
  m[is.na(m)] =0
  m[which(!is.finite(as.matrix(m)))] <- 0
  st1<-m[(lookback_period[li]):(nrow(m)-1),]
  
  s<-s[lookback_period[li]:(nrow(s)-1),] # REMOVING NA
  m<-m[lookback_period[li]:(nrow(m)-1),] # REMOVING NA
  
  pcr<-pric[(lookback_period[li]+1):nrow(pric),] # REMOVING NA
  
  zpcr<-(pcr-m)/s # Z SCORE
  zpcr<-as.matrix(zpcr) 
  zpcr[is.na(zpcr)] =0
  zpcr[which(!is.finite((zpcr)))] <- 0
  
  pric<-zpcr # FACTOR VARIABLE
  fin<- matrix(0,dim(pric)[1],dim(pric)[2]) # SIGNAL MATRIX
  pric[pric==0] <- NA
  clafic<-clafic[(lookback_period[li]+1):nrow(clafic),]
  pric[clafic==0]<-NA
  
  
  fin[2,which(as.matrix(pric[1,])>2)]<-1 # LONG ENTRY CONDITION/ SIGNAL LOOP
  for(i in 3:nrow(fin)){
    fin[i,which(fin[i-1,]==0  & as.matrix(pric[i-1,])>2)]<-1
    
  }
  
  
  fin[2,which(as.matrix(pric[1,])<(-1))]<--1 # SHORT ENTRY CONDITION/ SIGNAL LOOP
  for(i in 3:nrow(fin)){
    fin[i,which(fin[i-1,]==0  & as.matrix(pric[i-1,])<(-1))]<--1
    
  }
  
  
  #new<-data.frame(DATE = dt,fin)
  #write.csv(new,"pcrres.csv",row.names = FALSE)
  
  #topminc<-data.frame((apply(fin,1,sum,na.rm=TRUE)))
  #colnames(fin)<-colnames(pric)
  
  pric<-data.frame(read.csv(paste(path_input,"PX_LAST.csv",sep="")))[,-1]
  
  
  pricn<-pric[(lookback_period[li]+1):nrow(pric),]
  pricn[is.na(pricn)] =0
  pricn[which(!is.finite(as.matrix(pricn)))] <- 0
  
  st<-matrix(0,dim(as.matrix(pricn))[1],dim(as.matrix(pricn))[2])  # FOR LONG STOPLOSS EXIT
  high<-matrix(0,dim(as.matrix(pricn))[1],dim(as.matrix(pricn))[2])
  entry_l<-matrix(0,dim(as.matrix(pricn))[1],dim(as.matrix(pricn))[2])
  
  st_short<-matrix(0,dim(as.matrix(pricn))[1],dim(as.matrix(pricn))[2]) # FOR SHORT STOPLOSS EXIT
  low<-matrix(0,dim(as.matrix(pricn))[1],dim(as.matrix(pricn))[2])
  entry_s<-matrix(0,dim(as.matrix(pricn))[1],dim(as.matrix(pricn))[2])
  
  test_shortprice<-as.matrix(pricn) # FOR SHORT EXIT
  testprice<-as.matrix(pricn) # FOR LONG EXIT
  
  netp<-0 
  
  if(sum(fin[1,]==1 | fin[1,]==-1)>=cs){         # cHECKING TOTAL POSTIONS > 5
    entry_l[1,which(fin[1,]==1)]=1
    entry_s[1,which(fin[1,]==(-1))]=(-1)
    
    low[1,which(fin[1,]==(-1))]<-test_shortprice[1,which(fin[1,]==(-1))] # FOR STOP LOSS
    high[1,which(fin[1,]==(1))]<-testprice[1,which(fin[1,]==(1))] 
    
    
      st[1,which(entry_l[1,]==1)]<-0.97*testprice[1,which(entry_l[1,]==1)]
      st_short[1,which(fin[1,]==(-1))]<-1.03*test_shortprice[1,which(fin[1,]==(-1))]
    
    netp<-netp+sum(fin[1,]==1)+sum(fin[1,]==-1) # UPDATING NET POSITION
  }
  
  for(i in 2:nrow(testprice)){ # Iterating through the rows 
    
    if(sum(entry_l[i-1,]==1 | entry_s[i-1,]==-1)>0) # This is for checking exit criteria  
      {
      
      
      j<-sum(entry_l[i-1,]==1) # Long Exit
      
      while(j!=0){
        
        c<-which(entry_l[i-1,]==1)
        
        if(testprice[i,c[j]]>high[i-1,c[j]]) 
          {
          st[i,c[j]]<-0.97* (testprice[i,c[j]])
          high[i,c[j]]<- (testprice[i,c[j]])
        }
        
        else {
          st[i,c[j]]<-st[i-1,c[j]]
          high[i,c[j]]<-high[i-1,c[j]]
        }
        
        if(test1[i,c[j]]<st1[i,c[j]] | testprice[i,c[j]]<st[i,c[j]]){ #exit criteria , factor/SL
          entry_l[i,c[j]]=0
          netp=netp-1
        }
        
        else{
          entry_l[i,c[j]]=1
        }
        
        j=j-1
      }
      
      
      
      j<-sum(entry_s[i-1,]==-1) # Short Exit
      
      while(j!=0){
        
        c<-which(entry_s[i-1,]==-1)
        
        if(test_shortprice[i,c[j]]<low[i-1,c[j]]) 
          {
          st_short[i,c[j]]<-1.03* (test_shortprice[i,c[j]])
          low[i,c[j]]<- (test_shortprice[i,c[j]])
        }
        
        else {
          st_short[i,c[j]]<-st_short[i-1,c[j]]
          low[i,c[j]]<-low[i-1,c[j]]
        }
        
        if(test1[i,c[j]]>st1[i,c[j]] | test_shortprice[i,c[j]]>st_short[i,c[j]]){ #exit criteria
          entry_s[i,c[j]]=0
          netp=netp-1
        }
        
        else{
          entry_s[i,c[j]]=-1
        }
        
        j=j-1
      }
      
      
      
      
    } 
    
    if((sum(fin[i,]==1 | fin[i,]==-1)+netp)>=cs) # New postions matrix update
      {
      
      entry_l[i,which(fin[i,]==1)]=1 #new entry flag
      st[i,which(entry_l[i-1,]==0 & entry_l[i,]==1)]<-0.97*(testprice[i,which(entry_l[i-1,]==0 & entry_l[i,]==1)])
      high[i,which(entry_l[i-1,]==0 & entry_l[i,]==1)]<-testprice[i,which(entry_l[i-1,]==0 & entry_l[i,]==1)]
      
      
      entry_s[i,which(fin[i,]==-1)]=-1 #new entry flag
      st_short[i,which(entry_s[i-1,]==0 & entry_s[i,]==-1)]<-1.03*(test_shortprice[i,which(entry_s[i-1,]==0 & entry_s[i,]==-1)])
      low[i,which(entry_s[i-1,]==0 & entry_s[i,]==-1)]<-test_shortprice[i,which(entry_s[i-1,]==0 & entry_s[i,]==-1)]
      
      
      netp<-sum(entry_l[i,]==1 | entry_s[i,]==-1) # update total positions
    }
    
    
    
  }
  
  
  
  
  #write.csv(entry_l,paste(path_output,"entry_l.csv"),row.names = FALSE)
  #write.csv(entry_s,paste(path_output,"entry_s.csv"),row.names = FALSE)
  
  entry<-matrix(0,dim(as.matrix(pricn))[1],dim(as.matrix(pricn))[2]) # final position matrix 
  
  for(i in 1:nrow(entry)){
    if(i==1){
      entry[i,which(entry_l[i,]==1)]=1
      entry[i,which(entry_s[i,]==-1)]=-1
    }
    
    else{
      entry[i,which(entry_l[i,]==1 & entry_s[i,]==0 )]=1            # position updation logic
      # entry[i,which(entry_l[i,]==1 & entry_s[i,]==0 & entry_s[i-1,]==-1 & fin[i,]==1)]=1
      entry[i,which(entry_s[i,]==-1 & entry_l[i,]==0)]=-1
      # entry[i,which(entry_s[i,]==-1 & entry_l[i,]==0 & entry_l[i-1,]==1 & fin[i,]==-1)]=-1
      entry[i,which(entry_l[i,]==1 & entry_s[i,]==-1 & entry_l[i-1,]==0)]=1
      entry[i,which(entry_l[i,]==1 & entry_s[i,]==-1 & entry_s[i-1,]==0)]=-1
      entry[i,which(entry_l[i,]==1 & entry_s[i,]==-1 & entry_s[i-1,]==-1 & entry_l[i-1,]==1 & fin[i,]!=0 )]=fin[i,which(entry_l[i,]==1 & entry_s[i,]==-1 & entry_s[i-1,]==-1 & entry_l[i-1,]==1 & fin[i,]!=0 )]
      entry[i,which(entry_l[i,]==1 & entry_s[i,]==-1 & entry_s[i-1,]==-1 & entry_l[i-1,]==1 & fin[i,]==0 )]=entry[i-1,which(entry_l[i,]==1 & entry_s[i,]==-1 & entry_s[i-1,]==-1 & entry_l[i-1,]==1 & fin[i,]==0 )]
    }
  }
  
 ################################################################################################################################################################# 
  names<-colnames(clafic)
  colnames(entry)<-names
  newentry<-data.frame(DATE =dt,entry)
  names<-colnames(newentry)
  write.csv(newentry,paste(path_output,"entry.csv"),row.names = FALSE) #Entry and exit matrix
  
  start_date<-matrix(0,dim(entry)[1],dim(entry)[2]) # start, end, longs and shorts metrics
  for(i in 1:nrow(entry)){
    if(i==1){
      start_date[i,which(entry[i,]==1)]=paste(dt[i])
      start_date[i,which(entry[i,]==-1)]=paste(dt[i])
    }
    else{
      start_date[i,which(entry[i,]==1 & entry[i-1,]==0)]=paste(dt[i])
      start_date[i,which(entry[i,]==-1 & entry[i-1,]==0)]=paste(dt[i])
    }
  }
  
  start_date<-data.frame(DATE = dt,start_date)
  colnames(start_date)=names
  write.csv(start_date,paste(path_output,"start date.csv"),row.names = FALSE)
  
  end_date<-matrix(0,dim(entry)[1],dim(entry)[2])
  for(i in 2:nrow(entry)){
    
    end_date[i,which(entry[i,]==0 & entry[i-1,]==1)]=paste(dt[i])
    end_date[i,which(entry[i,]==0 & entry[i-1,]==-1)]=paste(dt[i])
    
  }
  end_date<-data.frame(DATE = dt,end_date)
  colnames(end_date)=names
  write.csv(end_date,paste(path_output,"end date.csv"),row.names = FALSE)
  
  LorS<-matrix(0,dim(entry)[1],dim(entry)[2])
  for(i in 1:nrow(entry)){
    if(i==1){
      LorS[i,which(entry[i,]==1)]="L"
      LorS[i,which(entry[i,]==-1)]="S"
    }
    else{
      LorS[i,which(entry[i,]==1 & entry[i-1,]==0)]="L"
      LorS[i,which(entry[i,]==-1 & entry[i-1,]==0)]="S"
    }
  }
  LorS<-data.frame(DATE = dt,LorS)
  colnames(LorS)=names
  write.csv(LorS,paste(path_output,"LorS.csv"),row.names = FALSE)
  
  
  np<-as.matrix(rowSums(abs(entry)))
  np<-data.frame(DATE =dt,np)
  write.csv(np,paste(path_output,"net_position_Long_Short.csv"),row.names = FALSE) #netPositions
  
  newst<-data.frame(DATE=dt,st)
  write.csv(newst,"stop_loss.csv",row.names = FALSE)
  
  
  newp<-data.frame(DATE=dt,pricn)
  write.csv(newp,paste(path_output,"prices.csv"),row.names = FALSE)
  
  stock_names<-matrix(0,nrow = nrow(entry),120)
  for(i in 1:nrow(entry)){
    k=1
    j<-sum(entry[i,]==1)
    while(j!=0){
      c<-which(entry[i,]==1)
      stock_names[i,k]=names[c[j]]
      k=k+1
      j=j-1
    }
  }
  stock_names<-data.frame(DATE = dt,stock_names)
  write.csv(stock_names,paste(path_output,"stock in zpcr.csv"),row.names=FALSE)
  
  ################################################################################################################################################################
  
  
  entry_final<-entry[-1*dim(entry)[1],]
  stck_lvl<-stck_lvl[(lookback_period[li]+1):nrow(stck_lvl),]
  stck_lvl_calc<-stck_lvl[-1,-1]
  
  
  
  #When Fixed Allocation
 
  if(fa==1){
  ###############################################################################  
    exposure_long<-as.matrix(rowSums(abs(entry_final))) # total exposure
    
    for(i in 1:ncol(entry_final)){
      entry_final[,i]<-entry_final[,i]/exposure_long #dividing by total no. of stocks
    }
    
    entry_final[is.na(entry_final)] =0
    entry_final<-as.matrix(entry_final)
    entry_final[which(!is.finite(entry_final))] <- 0
  #############################################################################  
    
    exposure_long_og<-as.matrix(rowSums(abs(entry)))
    
    for(i in 1:ncol(entry)){
      entry[,i]<-entry[,i]/exposure_long_og
    }
    
    entry[is.na(entry)] =0                 #For calculating total traded value
    entry<-as.matrix(entry)
    entry[which(!is.finite(entry))] <- 0
    write.csv(entry,paste(path_output,"entry_after.csv"),row.names=FALSE)
    
  
    #For Both Longs And Shorts
    tv<-matrix(0,dim(entry)[1],dim(entry)[2]) #for calculating TV
    tv[1,]<-abs(entry[1,]) 
    for(i in 2:nrow(entry)){
      tv[i,]=abs(entry[i,]-entry[i-1,])
    }
  
    tot_tv<-rowSums(abs(tv))
    tot_tv<-data.frame(DATE = dt,Tv = tot_tv)
    write.csv(tot_tv,paste(path_output,"GTV each day.csv"),row.names=FALSE)
    
    write.csv(stck_lvl_calc,paste(path_output,"stcklvlcalc.csv"),row.names=FALSE)
    
    fr<-data.frame(entry_final*stck_lvl_calc)
    ####################################################################################################
    lng_ret_tb<-data.frame(rowSums(entry_final*stck_lvl_calc))       # For calculating returns full
    cst_tb<-data.frame(rowSums(abs(entry[-1,]-entry[-1*dim(entry)[1],])*tc))
    # alloc<-rowSums(abs(clafic[-1]))
    # exposure_long<-as.matrix(rowSums(abs(entry_final)))
    tot_ret_tb<-data.frame((lng_ret_tb -cst_tb))
    tot_ret_tb[is.na(tot_ret_tb)] =0
    tot_ret_tb<-as.matrix(tot_ret_tb)
    tot_ret_tb[which(!is.finite(tot_ret_tb))] <- 0
    
    mtm<-tot_ret_tb
    mtm<-data.frame(DATE=dt[-1],MTM_Daily = mtm)
    name<-c("DATE","MTM")
    colnames(mtm)=name
    write.csv(mtm,paste(path_output,"MTM.csv"),row.names=FALSE)
    
    ann_mtm<-data.frame(aggregate(mtm[,2],by=list((substr(mtm[,1],7,10))),sum))
    ann_tv<-data.frame(aggregate(tot_tv[,2],by=list((substr(tot_tv[,1],7,10))),sum))
    ann_mtmtv<-ann_mtm[,2]/ann_tv[,2]
    ann_mtmtv<-data.frame(DATE = ann_mtm[,1],MTMTV<-ann_mtmtv)
    
    ret_fin_tb<-data.frame(stck_lvl[2:nrow(stck_lvl),1], tot_ret_tb, lng_ret_tb)
    colnames(ret_fin_tb) <- c("Date","Total Ret","Ret without tc")
    
    write.csv(ret_fin_tb,paste(path_output,"Long Only.csv"),row.names=FALSE)
    ##########################################################################################################################
    
    ann_ret_zpcrmean_long_only<-data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sum))       # Yearly Sharpe and returns
    ann_mean_zpcrmean_long_only<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),mean))
    ann_sd_zpcrmean_long_only<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),sd))
    t1<-ann_mean_zpcrmean_long_only[,2]
    t2<-ann_sd_zpcrmean_long_only[,2]
    ann_sharpe_zpcrmean_long_only<- data.frame(DATE=ann_mean_zpcrmean_long_only[,1],Sharpe = (t1/t2)*sqrt(252))
    
  ##############################################################################################################################
  
  
    
    cum_ret<-matrix(0,dim(fr)[1],dim(fr)[2]) #ret per day
    fr<-as.matrix(fr)
    entry_final<-as.matrix(entry_final)
    for(i in 1:nrow(fr)){
      if(i==1){
        cum_ret[i,which(entry_final[i,]!=0)]=fr[i,which(entry_final[i,]!=0)]
      }
      else{
        cum_ret[i,which(entry_final[i-1,]==0 & entry_final[i,]!=0)]=fr[i,which(entry_final[i-1,]==0 & entry_final[i,]!=0)]
        c=which(entry_final[i-1,]!=0 & entry_final[i,]!=0)
        j=length(c)
        if(j>0){
          for(k in 1:j){
            cum_ret[i,c[k]]=fr[i,c[k]]+cum_ret[i-1,c[k]]
            
          }
        }
      }
      
    }
    
    
    
    cum_ret_dt<-data.frame(DATE = dt[-1],cum_ret)
    write.csv(cum_ret_dt,paste(path_output,"cum_returns.csv"),row.names=FALSE)
    
    cum_rt<-matrix(0,length(dt),ncol = ncol(cum_ret))
    cum_rt[2:nrow(cum_rt),]=cum_ret
    trade_ret<-matrix(0,dim(cum_rt)[1],dim(cum_rt)[2])
    for(i in 2:nrow(entry)){
      
      c=which(cum_rt[i,]==0 & cum_rt[i-1,]!=0)
      j=length(c)
      if(j>0){
        for(k in 1:j){
          trade_ret[i-1,c[k]]=cum_rt[i-1,c[k]]
        }
      }
    }
    trade_ret[nrow(entry),]=cum_rt[nrow(entry),]
    trade_ret<-data.frame(DATE = dt,trade_ret)
    colnames(trade_ret)<-names
    write.csv(trade_ret,paste(path_output,"Trade Ret.csv"),row.names = FALSE)
    
    
    no_of_pos_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    no_of_neg_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    for(i in 2:nrow(no_of_pos_trades)){
      if(i==nrow(no_of_pos_trades)){
        no_of_pos_trades[i,which(cum_ret[i,]>0)]=1
        no_of_neg_trades[i,which(cum_ret[i,]<0)]=1
      }
      c=which(cum_ret[i-1,]!=0 & cum_ret[i,]==0)
      j=length(c)
      if(j>0){
        for(k in 1:j){
          if(cum_ret[i-1,c[k]]>0) no_of_pos_trades[i-1,c[k]]=1
          if(cum_ret[i-1,c[k]]<0) no_of_neg_trades[i-1,c[k]]=1    
        }
      }
    }
    no_of_trades=no_of_pos_trades+no_of_neg_trades
    write.csv(no_of_trades,paste(path_output,"Total trades.csv"),row.names = FALSE)
    
    pos<-data.frame((apply(no_of_pos_trades,1,sum,na.rm=TRUE)))
    neg<-data.frame((apply(no_of_neg_trades,1,sum,na.rm=TRUE)))
    
    
    pos<-data.frame(DATE = dt[-1],pos_No_OF_TRADES = pos[,1])
    neg<-data.frame(DATE = dt[-1],neg_No_OF_TRADES = neg[,1])
    
    ann_pos<-data.frame(aggregate(pos[,2],by=list((substr(pos[,1],7,10))),sum))
    ann_neg<-data.frame(aggregate(neg[,2],by=list((substr(neg[,1],7,10))),sum))
    ann_no_of_trades<-ann_pos[,2]+ann_neg[,2]
    ann_no_of_trades<-data.frame(DATE = ann_pos[,1], ann_no_of_trades = ann_no_of_trades)
    
    success_ratio<-ann_pos[,2]/ann_no_of_trades[,2]
    success_ratio<-data.frame(DATE = ann_pos[,1], suc_ratio = success_ratio)
    
    avg_pos<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    avg_neg<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    for(i in 2:nrow(avg_pos)){
      if(i==nrow(avg_pos)){
        avg_pos[i,which(cum_ret[i,]>0)]=cum_ret[i,which(cum_ret[i,]>0)]
        avg_neg[i,which(cum_ret[i,]<0)]=cum_ret[i,which(cum_ret[i,]<0)]
        
      }
      c=which(cum_ret[i-1,]!=0 & cum_ret[i,]==0)
      j=length(c)
      if(j>0){
        for(k in 1:j){
          if(cum_ret[i-1,c[k]]>0) avg_pos[i-1,c[k]]=cum_ret[i-1,c[k]]
          else avg_neg[i-1,c[k]]=cum_ret[i-1,c[k]]    
        }
      }
    }
    
    write.csv(avg_pos,paste(path_output,"avg_pos.csv"),row.names=FALSE)
    write.csv(avg_neg,paste(path_output,"avg_neg.csv"),row.names=FALSE)
    avg_pos<-data.frame((apply(avg_pos,1,sum,na.rm=TRUE)))
    avg_neg<-data.frame((apply(avg_neg,1,sum,na.rm=TRUE)))
    
    avg_pos<-data.frame(DATE = dt[-1],avg_pos_ret = avg_pos[,1])
    avg_neg<-data.frame(DATE = dt[-1],avg_neg_ret = avg_neg[,1])
    
    ann_avg_pos<-data.frame(aggregate(avg_pos[,2],by=list((substr(avg_pos[,1],7,10))),sum))
    ann_avg_neg<-data.frame(aggregate(avg_neg[,2],by=list((substr(avg_neg[,1],7,10))),sum))
    
    #Divide total ret pos/total pos
    ann_avg_pos<-ann_avg_pos[,2]/ann_pos[,2]
    ann_avg_neg<-ann_avg_neg[,2]/ann_neg[,2]
    ann_avg_neg<-data.frame(DATE = ann_neg[,1],ann_avg_neg)
    ann_avg_pos<-data.frame(DATE = ann_pos[,1],ann_avg_pos)
    
    wl<-abs(ann_avg_pos[,2]/ann_avg_neg[,2])
    wl<-data.frame(DATE = ann_avg_pos[,1],Win_to_lose = wl)
    
    
    
    
    
    #for long side
    
    entry_long<-matrix(0,dim(entry)[1],dim(entry)[2])
    for(i in 1:nrow(entry)){
      entry_long[i,which(entry[i,]>0)]<-entry[i,which(entry[i,]>0)]
    }
    write.csv(entry_long,paste(path_output,"entry_long.csv"),row.names=FALSE)
    entry_long_final<-entry_long[-1*dim(entry_long)[1],]
    
    fr<-data.frame(entry_long_final*stck_lvl_calc)
    
    lng_ret_tb<-data.frame(rowSums(entry_long_final*stck_lvl_calc))
    cst_tb<-data.frame(rowSums(abs(entry_long[-1,]-entry_long[-1*dim(entry_long)[1],])*tc))
    # alloc<-rowSums(abs(clafic[-1]))
    # exposure_long<-as.matrix(rowSums(abs(entry_long_final)))
    tot_ret_tb<-data.frame((lng_ret_tb -cst_tb))
    tot_ret_tb[is.na(tot_ret_tb)] =0
    tot_ret_tb<-as.matrix(tot_ret_tb)
    tot_ret_tb[which(!is.finite(tot_ret_tb))] <- 0
    
    mtm_long<-tot_ret_tb
    mtm_long<-data.frame(DATE=dt[-1],mtm_long_Daily = mtm_long)
    name<-c("DATE","mtm_long")
    colnames(mtm_long)=name
    write.csv(mtm_long,paste(path_output,"Long_mtm.csv"),row.names=FALSE)
    
    ann_mtm_long<-data.frame(aggregate(mtm_long[,2],by=list((substr(mtm_long[,1],7,10))),sum))
    ann_tv<-data.frame(aggregate(tot_tv[,2],by=list((substr(tot_tv[,1],7,10))),sum))
    ann_mtm_longtv<-ann_mtm_long[,2]/ann_tv[,2]
    ann_mtm_longtv<-data.frame(DATE = ann_mtm_long[,1],mtm_longTV<-ann_mtm_longtv)
    
    
    ann_ret_zpcrmean_long<-data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sum))
    ann_mean_zpcrmean_long<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),mean))
    ann_sd_zpcrmean_long<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),sd))
    t1<-ann_mean_zpcrmean_long[,2]
    t2<-ann_sd_zpcrmean_long[,2]
    ann_sharpe_zpcrmean_long<- data.frame(DATE=ann_mean_zpcrmean_long[,1],Sharpe = (t1/t2)*sqrt(252))
    
    
    cum_ret<-matrix(0,dim(fr)[1],dim(fr)[2]) #ret per day
    fr<-as.matrix(fr)
    entry_long_final<-as.matrix(entry_long_final)
    for(i in 1:nrow(fr)){
      if(i==1){
        cum_ret[i,which(entry_long_final[i,]!=0)]=fr[i,which(entry_long_final[i,]!=0)]
      }
      else{
        cum_ret[i,which(entry_long_final[i-1,]==0 & entry_long_final[i,]!=0)]=fr[i,which(entry_long_final[i-1,]==0 & entry_long_final[i,]!=0)]
        c=which(entry_long_final[i-1,]!=0 & entry_long_final[i,]!=0)
        j=length(c)
        if(j>0){
          for(k in 1:j){
            cum_ret[i,c[k]]=fr[i,c[k]]+cum_ret[i-1,c[k]]
            
          }
        }
      }
      
    }
    
    no_of_pos_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    no_of_neg_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    for(i in 2:nrow(no_of_pos_trades)){
      if(i==nrow(no_of_pos_trades)){
        no_of_pos_trades[i,which(cum_ret[i,]>0)]=1
        no_of_neg_trades[i,which(cum_ret[i,]<0)]=1
        y=i  
      }
      c=which(cum_ret[i-1,]!=0 & cum_ret[i,]==0)
      j=length(c)
      if(j>0){
        for(k in 1:j){
          if(cum_ret[i-1,c[k]]>0) no_of_pos_trades[i-1,c[k]]=1
          if(cum_ret[i-1,c[k]]<0) no_of_neg_trades[i-1,c[k]]=1    
        }
      }
    }
    no_of_trades=no_of_pos_trades+no_of_neg_trades
    write.csv(no_of_trades,paste(path_output,"Total long trades.csv"),row.names = FALSE)
    
    pos<-data.frame((apply(no_of_pos_trades,1,sum,na.rm=TRUE)))
    neg<-data.frame((apply(no_of_neg_trades,1,sum,na.rm=TRUE)))
    
    
    pos<-data.frame(DATE = dt[-1],pos_No_OF_TRADES = pos[,1])
    neg<-data.frame(DATE = dt[-1],neg_No_OF_TRADES = neg[,1])
    
    ann_pos_long<-data.frame(aggregate(pos[,2],by=list((substr(pos[,1],7,10))),sum))
    ann_neg_long<-data.frame(aggregate(neg[,2],by=list((substr(neg[,1],7,10))),sum))
    ann_no_of_trades_long<-ann_pos_long[,2]+ann_neg_long[,2]
    ann_no_of_trades_long<-data.frame(DATE = ann_pos_long[,1], ann_no_of_trades_long = ann_no_of_trades_long)
    
    
    
    
    
    #for short side
    
    entry_short<-matrix(0,dim(entry)[1],dim(entry)[2])
    for(i in 1:nrow(entry)){
      entry_short[i,which(entry[i,]<0)]<-entry[i,which(entry[i,]<0)]
    }
    entry_short_final<-entry_short[-1*dim(entry_short)[1],]
    write.csv(entry_short,paste(path_output,"entry_short_after.csv"),row.names=FALSE)
    
    fr<-data.frame(entry_short_final*stck_lvl_calc)
    
    lng_ret_tb<-data.frame(rowSums(entry_short_final*stck_lvl_calc))
    cst_tb<-data.frame(rowSums(abs(entry_short[-1,]-entry_short[-1*dim(entry_short)[1],])*tc))
    # alloc<-rowSums(abs(clafic[-1]))
    # exposure_short<-as.matrix(rowSums(abs(entry_short_final)))
    tot_ret_tb<-data.frame((lng_ret_tb -cst_tb))
    tot_ret_tb[is.na(tot_ret_tb)] =0
    tot_ret_tb<-as.matrix(tot_ret_tb)
    tot_ret_tb[which(!is.finite(tot_ret_tb))] <- 0
    
    mtm_short<-tot_ret_tb
    mtm_short<-data.frame(DATE=dt[-1],mtm_short_Daily = mtm_short)
    name<-c("DATE","mtm_short")
    colnames(mtm_short)=name
    write.csv(mtm_short,paste(path_output,"Long_mtm.csv"),row.names=FALSE)
    
    ann_mtm_short<-data.frame(aggregate(mtm_short[,2],by=list((substr(mtm_short[,1],7,10))),sum))
    ann_tv<-data.frame(aggregate(tot_tv[,2],by=list((substr(tot_tv[,1],7,10))),sum))
    ann_mtm_shorttv<-ann_mtm_short[,2]/ann_tv[,2]
    ann_mtm_shorttv<-data.frame(DATE = ann_mtm_short[,1],mtm_shortTV<-ann_mtm_shorttv)
    
    
    ann_ret_zpcrmean_short<-data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sum))
    ann_mean_zpcrmean_short<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),mean))
    ann_sd_zpcrmean_short<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),sd))
    t1<-ann_mean_zpcrmean_short[,2]
    t2<-ann_sd_zpcrmean_short[,2]
    ann_sharpe_zpcrmean_short<- data.frame(DATE=ann_mean_zpcrmean_short[,1],Sharpe = (t1/t2)*sqrt(252))
    
    
    cum_ret<-matrix(0,dim(fr)[1],dim(fr)[2]) #ret per day
    fr<-as.matrix(fr)
    entry_short_final<-as.matrix(entry_short_final)
    for(i in 1:nrow(fr)){
      if(i==1){
        cum_ret[i,which(entry_short_final[i,]!=0)]=fr[i,which(entry_short_final[i,]!=0)]
      }
      else{
        cum_ret[i,which(entry_short_final[i-1,]==0 & entry_short_final[i,]!=0)]=fr[i,which(entry_short_final[i-1,]==0 & entry_short_final[i,]!=0)]
        c=which(entry_short_final[i-1,]!=0 & entry_short_final[i,]!=0)
        j=length(c)
        if(j>0){
          for(k in 1:j){
            cum_ret[i,c[k]]=fr[i,c[k]]+cum_ret[i-1,c[k]]
            
          }
        }
      }
      
    }
    
    no_of_pos_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    no_of_neg_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    for(i in 2:nrow(no_of_pos_trades)){
      if(i==nrow(no_of_pos_trades)){
        no_of_pos_trades[i,which(cum_ret[i,]>0)]=1
        no_of_neg_trades[i,which(cum_ret[i,]<0)]=1
        y=i  
      }
      c=which(cum_ret[i-1,]!=0 & cum_ret[i,]==0)
      j=length(c)
      if(j>0){
        for(k in 1:j){
          if(cum_ret[i-1,c[k]]>0) no_of_pos_trades[i-1,c[k]]=1
          if(cum_ret[i-1,c[k]]<0) no_of_neg_trades[i-1,c[k]]=1    
        }
      }
    }
    no_of_trades=no_of_pos_trades+no_of_neg_trades
    write.csv(no_of_trades,paste(path_output,"Total short trades.csv"),row.names = FALSE)
    
    pos<-data.frame((apply(no_of_pos_trades,1,sum,na.rm=TRUE)))
    neg<-data.frame((apply(no_of_neg_trades,1,sum,na.rm=TRUE)))
    
    
    pos<-data.frame(DATE = dt[-1],pos_No_OF_TRADES = pos[,1])
    neg<-data.frame(DATE = dt[-1],neg_No_OF_TRADES = neg[,1])
    
    ann_pos_short<-data.frame(aggregate(pos[,2],by=list((substr(pos[,1],7,10))),sum))
    ann_neg_short<-data.frame(aggregate(neg[,2],by=list((substr(neg[,1],7,10))),sum))
    ann_no_of_trades_short<-ann_pos_short[,2]+ann_neg_short[,2]
    ann_no_of_trades_short<-data.frame(DATE = ann_pos_short[,1], ann_no_of_trades_short = ann_no_of_trades_short)
    
    
    
    
    
    
    
    
    final_res<-data.frame(Date=ann_avg_pos[,1],Number_Of_trades=ann_no_of_trades[,2],No_of_pos_Trades = ann_pos[,2], No_of_Neg_Trades = ann_neg[,2], Average_pos_Trade=ann_avg_pos[,2], Avg_Neg_trade = ann_avg_neg[,2], Success_Ratio=success_ratio[,2], Win_to_Lose = wl[,2], Total_mtm=ann_mtm[,2],Tot_tv=ann_tv[,2],Mtm_tv=ann_mtmtv[,2], Sharpe= ann_sharpe_zpcrmean_long_only[,2],No_of_long_Trades=ann_no_of_trades_long[,2],Mtm_long = ann_mtm_long[,2],No_of_short_Trades=ann_no_of_trades_short[,2],Mtm_short = ann_mtm_short[,2])
    write.csv(final_res[1:8,],paste("C:/Users/YashV/Desktop/Framework Data/Results/","final_resBEST_CPS",lookback_period[li] ,"Topanals_2sig_1sig_meanor3tst_l1.csv"),row.names=FALSE)
    
  }
  
  
  
  
  #if Variable Allocation
  
  
  
  if(fa==0){
    
    #For Both Longs And Shorts
    tv<-matrix(0,dim(entry)[1],dim(entry)[2])
    tv[1,]<-entry[1,]
    for(i in 2:nrow(entry)){
      tv[i,]=abs(entry[i,]-entry[i-1,])
    }
    tot_tv<-rowSums(abs(tv))
    tot_tv<-data.frame(DATE = dt,Tv = tot_tv)
    write.csv(tot_tv,paste(path_output,"GTV each day.csv"),row.names=FALSE)
    
    write.csv(stck_lvl_calc,paste(path_output,"stcklvlcalc.csv"),row.names=FALSE)
    
    fr<-data.frame(entry_final*stck_lvl_calc)
    
    lng_ret_tb<-data.frame(rowSums(entry_final*stck_lvl_calc))
    cst_tb<-data.frame(rowSums(abs(entry[-1,]-entry[-1*dim(entry)[1],])*tc))
    # alloc<-rowSums(abs(clafic[-1]))
    # exposure_long<-as.matrix(rowSums(abs(entry_final)))
    tot_ret_tb<-data.frame((lng_ret_tb -cst_tb))
    tot_ret_tb[is.na(tot_ret_tb)] =0
    tot_ret_tb<-as.matrix(tot_ret_tb)
    tot_ret_tb[which(!is.finite(tot_ret_tb))] <- 0
    
    mtm<-tot_ret_tb
    mtm<-data.frame(DATE=dt[-1],MTM_Daily = mtm)
    name<-c("DATE","MTM")
    colnames(mtm)=name
    write.csv(mtm,paste(path_output,"MTM.csv"),row.names=FALSE)
    
    ann_mtm<-data.frame(aggregate(mtm[,2],by=list((substr(mtm[,1],7,10))),sum))
    ann_tv<-data.frame(aggregate(tot_tv[,2],by=list((substr(tot_tv[,1],7,10))),sum))
    ann_mtmtv<-ann_mtm[,2]/ann_tv[,2]
    ann_mtmtv<-data.frame(DATE = ann_mtm[,1],MTMTV<-ann_mtmtv)
    
    ret_fin_tb<-data.frame(stck_lvl[2:nrow(stck_lvl),1], tot_ret_tb, lng_ret_tb)
    colnames(ret_fin_tb) <- c("Date","Total Ret","Ret without tc")
    
    write.csv(ret_fin_tb,paste(path_output,"Long Only.csv"),row.names=FALSE)
    
    
    ann_ret_zpcrmean_long_only<-data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sum))
    ann_mean_zpcrmean_long_only<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),mean))
    ann_sd_zpcrmean_long_only<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),sd))
    t1<-ann_mean_zpcrmean_long_only[,2]
    t2<-ann_sd_zpcrmean_long_only[,2]
    ann_sharpe_zpcrmean_long_only<- data.frame(DATE=ann_mean_zpcrmean_long_only[,1],Sharpe = (t1/t2)*sqrt(252))
    
    
    cum_ret<-matrix(0,dim(fr)[1],dim(fr)[2]) #ret per day
    fr<-as.matrix(fr)
    entry_final<-as.matrix(entry_final)
    for(i in 1:nrow(fr)){
      if(i==1){
        cum_ret[i,which(entry_final[i,]!=0)]=fr[i,which(entry_final[i,]!=0)]
      }
      else{
        cum_ret[i,which(entry_final[i-1,]==0 & entry_final[i,]!=0)]=fr[i,which(entry_final[i-1,]==0 & entry_final[i,]!=0)]
        c=which(entry_final[i-1,]!=0 & entry_final[i,]!=0)
        j=length(c)
        if(j>0){
          for(k in 1:j){
            cum_ret[i,c[k]]=fr[i,c[k]]+cum_ret[i-1,c[k]]
            
          }
        }
      }
      
    }
    
    
    
    cum_ret_dt<-data.frame(DATE = dt[-1],cum_ret)
    write.csv(cum_ret_dt,paste(path_output,"cum_returns.csv"),row.names=FALSE)
    
    cum_rt<-matrix(0,length(dt),ncol = ncol(cum_ret))
    cum_rt[2:nrow(cum_rt),]=cum_ret
    trade_ret<-matrix(0,dim(cum_rt)[1],dim(cum_rt)[2])
    for(i in 2:nrow(entry)){
      
      c=which(cum_rt[i,]==0 & cum_rt[i-1,]!=0)
      j=length(c)
      if(j>0){
        for(k in 1:j){
          trade_ret[i-1,c[k]]=cum_rt[i-1,c[k]]
        }
      }
    }
    trade_ret[nrow(entry),]=cum_rt[nrow(entry),]
    trade_ret<-data.frame(DATE = dt,trade_ret)
    colnames(trade_ret)<-names
    write.csv(trade_ret,paste(path_output,"Trade Ret.csv"),row.names = FALSE)
    
    
    no_of_pos_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    no_of_neg_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    for(i in 2:nrow(no_of_pos_trades)){
      if(i==nrow(no_of_pos_trades)){
        no_of_pos_trades[i,which(cum_ret[i,]>0)]=1
        no_of_neg_trades[i,which(cum_ret[i,]<0)]=1
      }
      c=which(cum_ret[i-1,]!=0 & cum_ret[i,]==0)
      j=length(c)
      if(j>0){
        for(k in 1:j){
          if(cum_ret[i-1,c[k]]>0) no_of_pos_trades[i-1,c[k]]=1
          if(cum_ret[i-1,c[k]]<0) no_of_neg_trades[i-1,c[k]]=1    
        }
      }
    }
    no_of_trades=no_of_pos_trades+no_of_neg_trades
    write.csv(no_of_trades,paste(path_output,"Total trades.csv"),row.names = FALSE)
    
    pos<-data.frame((apply(no_of_pos_trades,1,sum,na.rm=TRUE)))
    neg<-data.frame((apply(no_of_neg_trades,1,sum,na.rm=TRUE)))
    
    
    pos<-data.frame(DATE = dt[-1],pos_No_OF_TRADES = pos[,1])
    neg<-data.frame(DATE = dt[-1],neg_No_OF_TRADES = neg[,1])
    
    ann_pos<-data.frame(aggregate(pos[,2],by=list((substr(pos[,1],7,10))),sum))
    ann_neg<-data.frame(aggregate(neg[,2],by=list((substr(neg[,1],7,10))),sum))
    ann_no_of_trades<-ann_pos[,2]+ann_neg[,2]
    ann_no_of_trades<-data.frame(DATE = ann_pos[,1], ann_no_of_trades = ann_no_of_trades)
    
    success_ratio<-ann_pos[,2]/ann_no_of_trades[,2]
    success_ratio<-data.frame(DATE = ann_pos[,1], suc_ratio = success_ratio)
    
    avg_pos<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    avg_neg<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    for(i in 2:nrow(avg_pos)){
      if(i==nrow(avg_pos)){
        avg_pos[i,which(cum_ret[i,]>0)]=cum_ret[i,which(cum_ret[i,]>0)]
        avg_neg[i,which(cum_ret[i,]<0)]=cum_ret[i,which(cum_ret[i,]<0)]
        
      }
      c=which(cum_ret[i-1,]!=0 & cum_ret[i,]==0)
      j=length(c)
      if(j>0){
        for(k in 1:j){
          if(cum_ret[i-1,c[k]]>0) avg_pos[i-1,c[k]]=cum_ret[i-1,c[k]]
          else avg_neg[i-1,c[k]]=cum_ret[i-1,c[k]]    
        }
      }
    }
    
    write.csv(avg_pos,paste(path_output,"avg_pos.csv"),row.names=FALSE)
    write.csv(avg_neg,paste(path_output,"avg_neg.csv"),row.names=FALSE)
    avg_pos<-data.frame((apply(avg_pos,1,sum,na.rm=TRUE)))
    avg_neg<-data.frame((apply(avg_neg,1,sum,na.rm=TRUE)))
    
    avg_pos<-data.frame(DATE = dt[-1],avg_pos_ret = avg_pos[,1])
    avg_neg<-data.frame(DATE = dt[-1],avg_neg_ret = avg_neg[,1])
    
    ann_avg_pos<-data.frame(aggregate(avg_pos[,2],by=list((substr(avg_pos[,1],7,10))),sum))
    ann_avg_neg<-data.frame(aggregate(avg_neg[,2],by=list((substr(avg_neg[,1],7,10))),sum))
    
    #Divide total ret pos/total pos
    ann_avg_pos<-ann_avg_pos[,2]/ann_pos[,2]
    ann_avg_neg<-ann_avg_neg[,2]/ann_neg[,2]
    ann_avg_neg<-data.frame(DATE = ann_neg[,1],ann_avg_neg)
    ann_avg_pos<-data.frame(DATE = ann_pos[,1],ann_avg_pos)
    
    wl<-abs(ann_avg_pos[,2]/ann_avg_neg[,2])
    wl<-data.frame(DATE = ann_avg_pos[,1],Win_to_lose = wl)
    
    
    
    
    
    #for long side
    
    entry_long<-matrix(0,dim(entry)[1],dim(entry)[2])
    for(i in 1:nrow(entry)){
      entry_long[i,which(entry[i,]>0)]<-entry[i,which(entry[i,]>0)]
    }
    write.csv(entry_long,paste(path_output,"entry_long.csv"),row.names=FALSE)
    entry_long_final<-entry_long[-1*dim(entry_long)[1],]
    
    fr<-data.frame(entry_long_final*stck_lvl_calc)
    
    lng_ret_tb<-data.frame(rowSums(entry_long_final*stck_lvl_calc))
    cst_tb<-data.frame(rowSums(abs(entry_long[-1,]-entry_long[-1*dim(entry_long)[1],])*tc))
    # alloc<-rowSums(abs(clafic[-1]))
    # exposure_long<-as.matrix(rowSums(abs(entry_long_final)))
    tot_ret_tb<-data.frame((lng_ret_tb -cst_tb))
    tot_ret_tb[is.na(tot_ret_tb)] =0
    tot_ret_tb<-as.matrix(tot_ret_tb)
    tot_ret_tb[which(!is.finite(tot_ret_tb))] <- 0
    
    mtm_long<-tot_ret_tb
    mtm_long<-data.frame(DATE=dt[-1],mtm_long_Daily = mtm_long)
    name<-c("DATE","mtm_long")
    colnames(mtm_long)=name
    write.csv(mtm_long,paste(path_output,"Long_mtm.csv"),row.names=FALSE)
    
    ann_mtm_long<-data.frame(aggregate(mtm_long[,2],by=list((substr(mtm_long[,1],7,10))),sum))
    ann_tv<-data.frame(aggregate(tot_tv[,2],by=list((substr(tot_tv[,1],7,10))),sum))
    ann_mtm_longtv<-ann_mtm_long[,2]/ann_tv[,2]
    ann_mtm_longtv<-data.frame(DATE = ann_mtm_long[,1],mtm_longTV<-ann_mtm_longtv)
    
    
    ann_ret_zpcrmean_long<-data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sum))
    ann_mean_zpcrmean_long<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),mean))
    ann_sd_zpcrmean_long<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),sd))
    t1<-ann_mean_zpcrmean_long[,2]
    t2<-ann_sd_zpcrmean_long[,2]
    ann_sharpe_zpcrmean_long<- data.frame(DATE=ann_mean_zpcrmean_long[,1],Sharpe = (t1/t2)*sqrt(252))
    
    
    cum_ret<-matrix(0,dim(fr)[1],dim(fr)[2]) #ret per day
    fr<-as.matrix(fr)
    entry_long_final<-as.matrix(entry_long_final)
    for(i in 1:nrow(fr)){
      if(i==1){
        cum_ret[i,which(entry_long_final[i,]!=0)]=fr[i,which(entry_long_final[i,]!=0)]
      }
      else{
        cum_ret[i,which(entry_long_final[i-1,]==0 & entry_long_final[i,]!=0)]=fr[i,which(entry_long_final[i-1,]==0 & entry_long_final[i,]!=0)]
        c=which(entry_long_final[i-1,]!=0 & entry_long_final[i,]!=0)
        j=length(c)
        if(j>0){
          for(k in 1:j){
            cum_ret[i,c[k]]=fr[i,c[k]]+cum_ret[i-1,c[k]]
            
          }
        }
      }
      
    }
    
    no_of_pos_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    no_of_neg_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    for(i in 2:nrow(no_of_pos_trades)){
      if(i==nrow(no_of_pos_trades)){
        no_of_pos_trades[i,which(cum_ret[i,]>0)]=1
        no_of_neg_trades[i,which(cum_ret[i,]<0)]=1
        y=i  
      }
      c=which(cum_ret[i-1,]!=0 & cum_ret[i,]==0)
      j=length(c)
      if(j>0){
        for(k in 1:j){
          if(cum_ret[i-1,c[k]]>0) no_of_pos_trades[i-1,c[k]]=1
          if(cum_ret[i-1,c[k]]<0) no_of_neg_trades[i-1,c[k]]=1    
        }
      }
    }
    no_of_trades=no_of_pos_trades+no_of_neg_trades
    write.csv(no_of_trades,paste(path_output,"Total long trades.csv"),row.names = FALSE)
    
    pos<-data.frame((apply(no_of_pos_trades,1,sum,na.rm=TRUE)))
    neg<-data.frame((apply(no_of_neg_trades,1,sum,na.rm=TRUE)))
    
    
    pos<-data.frame(DATE = dt[-1],pos_No_OF_TRADES = pos[,1])
    neg<-data.frame(DATE = dt[-1],neg_No_OF_TRADES = neg[,1])
    
    ann_pos_long<-data.frame(aggregate(pos[,2],by=list((substr(pos[,1],7,10))),sum))
    ann_neg_long<-data.frame(aggregate(neg[,2],by=list((substr(neg[,1],7,10))),sum))
    ann_no_of_trades_long<-ann_pos_long[,2]+ann_neg_long[,2]
    ann_no_of_trades_long<-data.frame(DATE = ann_pos_long[,1], ann_no_of_trades_long = ann_no_of_trades_long)
    
    
    
    
    
    #for short side
    
    entry_short<-matrix(0,dim(entry)[1],dim(entry)[2])
    for(i in 1:nrow(entry)){
      entry_short[i,which(entry[i,]<0)]<-entry[i,which(entry[i,]<0)]
    }
    entry_short_final<-entry_short[-1*dim(entry_short)[1],]
    write.csv(entry_short,paste(path_output,"entry_short_after.csv"),row.names=FALSE)
    
    fr<-data.frame(entry_short_final*stck_lvl_calc)
    
    lng_ret_tb<-data.frame(rowSums(entry_short_final*stck_lvl_calc))
    cst_tb<-data.frame(rowSums(abs(entry_short[-1,]-entry_short[-1*dim(entry_short)[1],])*tc))
    # alloc<-rowSums(abs(clafic[-1]))
    # exposure_short<-as.matrix(rowSums(abs(entry_short_final)))
    tot_ret_tb<-data.frame((lng_ret_tb -cst_tb))
    tot_ret_tb[is.na(tot_ret_tb)] =0
    tot_ret_tb<-as.matrix(tot_ret_tb)
    tot_ret_tb[which(!is.finite(tot_ret_tb))] <- 0
    
    mtm_short<-tot_ret_tb
    mtm_short<-data.frame(DATE=dt[-1],mtm_short_Daily = mtm_short)
    name<-c("DATE","mtm_short")
    colnames(mtm_short)=name
    write.csv(mtm_short,paste(path_output,"Long_mtm.csv"),row.names=FALSE)
    
    ann_mtm_short<-data.frame(aggregate(mtm_short[,2],by=list((substr(mtm_short[,1],7,10))),sum))
    ann_tv<-data.frame(aggregate(tot_tv[,2],by=list((substr(tot_tv[,1],7,10))),sum))
    ann_mtm_shorttv<-ann_mtm_short[,2]/ann_tv[,2]
    ann_mtm_shorttv<-data.frame(DATE = ann_mtm_short[,1],mtm_shortTV<-ann_mtm_shorttv)
    
    
    ann_ret_zpcrmean_short<-data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sum))
    ann_mean_zpcrmean_short<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),mean))
    ann_sd_zpcrmean_short<-data.frame(aggregate(ret_fin_tb[,(2)],by=list((substr(ret_fin_tb[,1],1,4))),sd))
    t1<-ann_mean_zpcrmean_short[,2]
    t2<-ann_sd_zpcrmean_short[,2]
    ann_sharpe_zpcrmean_short<- data.frame(DATE=ann_mean_zpcrmean_short[,1],Sharpe = (t1/t2)*sqrt(252))
    
    
    cum_ret<-matrix(0,dim(fr)[1],dim(fr)[2]) #ret per day
    fr<-as.matrix(fr)
    entry_short_final<-as.matrix(entry_short_final)
    for(i in 1:nrow(fr)){
      if(i==1){
        cum_ret[i,which(entry_short_final[i,]!=0)]=fr[i,which(entry_short_final[i,]!=0)]
      }
      else{
        cum_ret[i,which(entry_short_final[i-1,]==0 & entry_short_final[i,]!=0)]=fr[i,which(entry_short_final[i-1,]==0 & entry_short_final[i,]!=0)]
        c=which(entry_short_final[i-1,]!=0 & entry_short_final[i,]!=0)
        j=length(c)
        if(j>0){
          for(k in 1:j){
            cum_ret[i,c[k]]=fr[i,c[k]]+cum_ret[i-1,c[k]]
            
          }
        }
      }
      
    }
    
    no_of_pos_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    no_of_neg_trades<-matrix(0,dim(cum_ret)[1],dim(cum_ret)[2])
    for(i in 2:nrow(no_of_pos_trades)){
      if(i==nrow(no_of_pos_trades)){
        no_of_pos_trades[i,which(cum_ret[i,]>0)]=1
        no_of_neg_trades[i,which(cum_ret[i,]<0)]=1
        y=i  
      }
      c=which(cum_ret[i-1,]!=0 & cum_ret[i,]==0)
      j=length(c)
      if(j>0){
        for(k in 1:j){
          if(cum_ret[i-1,c[k]]>0) no_of_pos_trades[i-1,c[k]]=1
          if(cum_ret[i-1,c[k]]<0) no_of_neg_trades[i-1,c[k]]=1    
        }
      }
    }
    no_of_trades=no_of_pos_trades+no_of_neg_trades
    write.csv(no_of_trades,paste(path_output,"Total short trades.csv"),row.names = FALSE)
    
    pos<-data.frame((apply(no_of_pos_trades,1,sum,na.rm=TRUE)))
    neg<-data.frame((apply(no_of_neg_trades,1,sum,na.rm=TRUE)))
    
    
    pos<-data.frame(DATE = dt[-1],pos_No_OF_TRADES = pos[,1])
    neg<-data.frame(DATE = dt[-1],neg_No_OF_TRADES = neg[,1])
    
    ann_pos_short<-data.frame(aggregate(pos[,2],by=list((substr(pos[,1],7,10))),sum))
    ann_neg_short<-data.frame(aggregate(neg[,2],by=list((substr(neg[,1],7,10))),sum))
    ann_no_of_trades_short<-ann_pos_short[,2]+ann_neg_short[,2]
    ann_no_of_trades_short<-data.frame(DATE = ann_pos_short[,1], ann_no_of_trades_short = ann_no_of_trades_short)
    
    
    
    
    
    
    
    
    final_res<-data.frame(Date=ann_avg_pos[,1],Number_Of_trades=ann_no_of_trades[,2],No_of_pos_Trades = ann_pos[,2], No_of_Neg_Trades = ann_neg[,2], Average_pos_Trade=ann_avg_pos[,2], Avg_Neg_trade = ann_avg_neg[,2], Success_Ratio=success_ratio[,2], Win_to_Lose = wl[,2], Total_mtm=ann_mtm[,2],Tot_tv=ann_tv[,2],Mtm_tv=ann_mtmtv[,2], Sharpe= ann_sharpe_zpcrmean_long_only[,2],No_of_long_Trades=ann_no_of_trades_long[,2],Mtm_long = ann_mtm_long[,2],No_of_short_Trades=ann_no_of_trades_short[,2],Mtm_short = ann_mtm_short[,2])
    write.csv(final_res[1:8,],paste("C:/Users/YashV/Desktop/Framework Data/Results/","final_resav5estEPS",lookback_period[li] ,"MCAP 50_2sig_2sig_meanstor5tst_l1_va.csv"),row.names=FALSE)
    
  }
  
  
}


