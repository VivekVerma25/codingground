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
  
  #write.csv(dly_retn[,1],"C:/Users/ChetannC.EDELCAP/Desktop/Bahut_ho_gaya/Data/14date.csv")
  
  
  
  return(analysis)
  
  
  
  
}
