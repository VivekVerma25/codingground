# This is the code for creating return, given stock list and weights , every rebalancing date

##################################################### Libraries

library("xts")
library("zoo")
library("matrixStats")

############################ Inputs to the code
setwd("D:/LFT/Template/tester/R test data/full simulation")
path_input<-"P:/HFT Quant/Shared folder/Mitali/LIVE DATABASE/INPUT/"
path_output<-"D:/LFT/Template/tester/R test data/full simulation/FINAL FILES/OUTPUT/PMN05 WEIGHTS/"

stck_lvl<-data.frame(read.csv(paste(path_input,"FutDB/","RollAdjRet.csv",sep="")))
stck_lvl$Date<-as.Date(stck_lvl$Date,"%d/%m/%Y")


bna<-0  # Keep this 1 if you need beta neutral
xnum<-10  # This is the number of stocks you need to select both on the top side and bottom side
dsort <-0 # For enabling double sort
dnum1<-25 # No. of stocks for primary factor
dnum2<-10 # no. of stocks for sec factor 
NOS <-1 # No. of strategies
##############################################

tc<-(0.00025)                                # This is one side trnsaction cost 
cs=6 #min of 5 stocks should be there or else no trade

#################################################################################################################################################################
WEIGHT_OP<-data.frame(matrix(0,NOS,dim(stck_lvl)[2]-1))
colnames(WEIGHT_OP)<- colnames(stck_lvl[1:NOS,-1])
list_full <- vector('list',NOS)

#################################################################################################################################################################

raw_cash_close= data.frame(read.csv(paste(path_input,"CashDB/","px_last.csv",sep="")))
raw_est_eps= data.frame(read.csv(paste(path_input,"AnalystDB/","BEST_EPS.csv",sep="")))
#list_PEST <- vector('list',1)

RAW_PEST<-raw_cash_close
dt<-RAW_PEST[1:nrow(RAW_PEST),1]
x = raw_cash_close[,-1]/raw_est_eps[,-1]
x<-as.matrix(x)
x[which(!is.finite(x))] <- NA
RAW_PEST<-data.frame(DATE=dt,x)

lookback_period=c(250)

for(l in 1:length(lookback_period)){
  
  wq1<-RAW_PEST
  dt<-wq1[1:nrow(wq1),1]
  m<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  s<-rollapply(wq1[,-1],lookback_period[l],sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  roll= (wq1[,-1]-m)/s
  
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  rollfin<-data.frame(DATE=dt,rollfin)
  
  list_full[[l]] <-rollfin      
}
#######################################################################################################################################################
names(list_full) <- list("PMN05")
SNAMES <- data.frame(names(list_full))
colnames(SNAMES) <- "MN WEIGHTS"
DATE_TEMP = stck_lvl[nrow(stck_lvl),1]
DATE = data.frame(DATE_TEMP)
DATE=t(DATE)
colnames(DATE) <- "PORTFOLIO_DATE"
#names(list_full) <- list("BASIS 5","BASIS 10","BASIS 20","ANALYST 20","ANALYST 60","PT 20","PT 60","BUY 20", "BUY 60" ,"SELL 20" ,"SELL 60","25 DELTA 10","25 DELTA 20","40 DELTA 10","40 DELTA 20")
#names(list_op) <- list("25 DELTA 10","25 DELTA 20","40 DELTA 10","40 DELTA 20")

#names(list_est) <- list("ANALYST 20","ANALYST 60","PT 20","PT 60","BUY 20", "BUY 60" ,"SELL 20" ,"SELL 60")

#names(list_basis) <- list("BASIS 5","BASIS 10","BASIS 20")

#######################################################################################################################################################

fac_list = list_full

for(z in 1 : length(fac_list))
{
  
  fac_1<-fac_list[[z]] # factor file input
  
  #stapp<-data.frame(read.csv(paste(uni_list[u,1],"/",uni_list[u,2],".csv",sep="")))
  
  stapp = data.frame(read.csv(paste(path_input,"UNIVERSE/","N50_TOP50_ANALYST.csv",sep="")))
  
  f_factor<-data.frame(fac_1)                                 # This will be the final clculate foactor to be used for ranking
  f_factor<-(f_factor[,-1])
  ff_factor=f_factor
  f_factor[stapp[,-1]==0]<-NA
  #f_factor[f_factor<0.0000001 & f_factor>-0.0000001]<-NA
  f_factor[is.na(f_factor)] =0
  ff_factor[is.na(ff_factor)] =0
  
  fin<- matrix(0,dim(f_factor)[1],dim(f_factor)[2]) # SIGNAL MATRIX
  #pric[pric==0] <- NA
  #clafic<-clafic[(lookback_period[li]+1):nrow(clafic),]
  #pric[clafic==0]<-NA
  
  
  fin[2,which(as.matrix(f_factor[1,]) < (-1))]=1 # LONG ENTRY CONDITION/ SIGNAL LOOP
  for(i in 3:nrow(fin)){
    fin[i,which((fin[i-1,]==0)  & (as.matrix(f_factor[i-1,])< (-1)))]=1
    
  }
  
  # 
  # fin[2,which(as.matrix(pric[1,])<(-1))]<--1 # SHORT ENTRY CONDITION/ SIGNAL LOOP
  # for(i in 3:nrow(fin)){
  #   fin[i,which(fin[i-1,]==0  & as.matrix(pric[i-1,])<(-1))]<--1
  #   
  # }
  
  entry_l<-matrix(0,dim(f_factor)[1],dim(f_factor)[2])
  
  entry_s<-matrix(0,dim(f_factor)[1],dim(f_factor)[2])
  
  netp<-0 
  
  if(sum(fin[1,]==1)>=cs){         # cHECKING TOTAL POSTIONS > 5
    entry_l[1,which(fin[1,]==1)]=1
    #entry_s[1,which(fin[1,]==(-1))]=(-1)
    
    netp<-netp+sum(fin[1,]==1)
    
  }
  
  
  for(i in 2:nrow(f_factor)) { # Iterating through the rows 
    
    if(sum(entry_l[i-1,]==1)>0) # This is for checking exit criteria  
    {
      
      
      j<-sum(entry_l[i-1,]==1) # Long Exit
      
      while(j>0){
        
        c<-which(entry_l[i-1,]==1)
        
        
        if(ff_factor[i-1,c[j]]>0) { #exit criteria , factor/SL
          entry_l[i,c[j]]=0
          netp=netp-1
        }
        
        else{
          entry_l[i,c[j]]=1
        }
        
        j=j-1
      }
      
    }
    
    
    if((sum(fin[i,]==1))>0) # New postions matrix update
    {
      
      entry_l[i,which(fin[i,]==1)]=1 #new entry flag
      #st[i,which(entry_l[i-1,]==0 & entry_l[i,]==1)]<-0.97*(testprice[i,which(entry_l[i-1,]==0 & entry_l[i,]==1)])
      #high[i,which(entry_l[i-1,]==0 & entry_l[i,]==1)]<-testprice[i,which(entry_l[i-1,]==0 & entry_l[i,]==1)]
      
      
      #entry_s[i,which(fin[i,]==-1)]=-1 #new entry flag
      #st_short[i,which(entry_s[i-1,]==0 & entry_s[i,]==-1)]<-1.03*(test_shortprice[i,which(entry_s[i-1,]==0 & entry_s[i,]==-1)])
      #low[i,which(entry_s[i-1,]==0 & entry_s[i,]==-1)]<-test_shortprice[i,which(entry_s[i-1,]==0 & entry_s[i,]==-1)]
      
      
      netp<-sum(entry_l[i,]==1) # update total positions
    }
    
    
    
  }
  
  entry <- entry_l
  
  for(i in 1:nrow(entry))
  {
    if(sum(entry[i,])<cs)
    {
      entry[i,]=0;
    }
    
  }
  
  
  colnames(entry) <- colnames(stapp[,-1])
  entry_final = entry
  
  for(i in 1:nrow(entry_final))
  {
    entry_final[i,1] = (-1) * sum(entry[i,])
    
  }
  
  entry_final[entry_final==0] =NA
  #entry_final[entry_final<0.0000001 & entry_final>-0.0000001]<-NA
  wts_fin = entry_final/rowSums(abs(entry_final),na.rm=TRUE)
  wts_fin[is.na(wts_fin)]<-0
  
  
  
  wts_int_full<-data.frame((wts_fin))
  
  
  wts_int_pos_full<-wts_int_full
  wts_int_neg_full<-wts_int_full
  
  wts_int_pos_full[wts_int_full<=0]<-0
  wts_int_neg_full[wts_int_full>=0]<-0
  
  
  ##############################################################################################################################
  #wts_int_pos_full_calc<-wts_int_pos_full[c(-1*dim(wts_int_full)[1],-1*dim(wts_int_full)[1] +1),]             #Full universe factor returns
  #wts_int_neg_full_calc<-wts_int_neg_full[c(-1*dim(wts_int_full)[1],-1*dim(wts_int_full)[1] +1),]
  wts_int_pos_full_calc<-wts_int_pos_full[-1*dim(wts_int_full)[1],]             #Full universe factor returns
  wts_int_neg_full_calc<-wts_int_neg_full[-1*dim(wts_int_full)[1],]
  
  
  #wts_int_pos_full_calc<-wts_int_pos_full_calc[-1*dim(wts_int_pos_full_calc)[1],]             #Full universe factor returns
  #wts_int_neg_full_calc<-wts_int_neg_full_calc[-1*dim(wts_int_neg_full_calc)[1],]
  
  stck_lvl_calc<-stck_lvl[-1,-1]
  
  lng_ret_full<-data.frame(rowSums(wts_int_pos_full_calc*stck_lvl_calc))
  sht_ret_full<-data.frame(rowSums(wts_int_neg_full_calc*stck_lvl_calc))
  tot_ret_full = data.frame(rowSums(wts_int_full[-1*dim(wts_int_full)[1],]*stck_lvl_calc))
  
  lng_beta_full<- matrix(1,dim(lng_ret_full)[1],1)
  sht_beta_full<- matrix(1,dim(sht_ret_full)[1],1)
  
  
  if (bna==1){        # This will check if beta neutral needs to be done or not
    
    
    for(m in 251: dim(lng_ret_full)[1])        # This will quantify the beta ratio between long and short in the portfolio
    {
      
      #print(m)  
      
      lng_beta_full[m,1]<- (cov(lng_ret_full[(m-250):(m-1),1],stck_lvl[c(-1,-2),2][(m-250):(m-1)])/var(stck_lvl[c(-1,-2),2][(m-250):(m-1)]))
      sht_beta_full[m,1]<- -(cov(sht_ret_full[(m-250):(m-1),1],stck_lvl[c(-1,-2),2][(m-250):(m-1)])/var(stck_lvl[c(-1,-2),2][(m-250):(m-1)]))
    }
    
    wts_int_neg_full_calc<-wts_int_neg_full_calc*(lng_beta_full/sht_beta_full)
    wts_int_full[c(-1*dim(wts_int_full)[1],-1*dim(wts_int_full)[1] +1),] <- ( wts_int_pos_full_calc + wts_int_neg_full_calc)
    wts_int_full[(dim(wts_int_full)[1]-1),] <- wts_int_pos_full[(dim(wts_int_full)[1]-1),]+wts_int_neg_full[(dim(wts_int_full)[1]-1),]*(lng_beta_full[dim(lng_beta_full)[1],1]/sht_beta_full[dim(sht_beta_full)[1],1])
    wts_int_neg_full<-wts_int_full
    wts_int_neg_full[wts_int_full>=0]<-0
    sht_ret_full<-data.frame(rowSums(wts_int_neg_full_calc*stck_lvl_calc))
  } 
  
  wts_int_full1=wts_int_full
  wts_int_full1[]=0
  wts_int_full1[-1,] = wts_int_full[-1*dim(wts_int_full)[1],]
  cst_full<-data.frame(rowSums(abs(wts_int_full-wts_int_full1)*tc))
  cst_full<-cst_full[(-1)*dim(cst_full)[1],]
  
  cst_full_L<-data.frame(rowSums(abs(wts_int_pos_full[-1,]-wts_int_pos_full[-1*dim(wts_int_pos_full)[1],])*tc))
  #cst_full_L<-cst_full_L[-1*dim(cst_full_L)[1],]
  
  cst_full_S<-data.frame(rowSums(abs(wts_int_neg_full[-1,]-wts_int_neg_full[-1*dim(wts_int_neg_full)[1],])*tc))
  #cst_full_S<-cst_full_S[-1*dim(cst_full_S)[1],]
  
  
  #lng_ret_full<-data.frame(rowSums(wts_int_pos_full_calc*stck_lvl_calc))
  #tot_ret_full<-data.frame((wts_int_full[c(-1*dim(wts_int_full)[1],-1*dim(wts_int_full)[1] +1),]*stck_lvl_calc)-cst_full)  
  
  tot_ret_full<-data.frame((tot_ret_full)-cst_full)
  
  
  lng_ret_full<-data.frame(lng_ret_full-cst_full_L)
  sht_ret_full<-data.frame(sht_ret_full-cst_full_S)
  
  ret_fin_full<-data.frame(cbind(stck_lvl$Date[-1],tot_ret_full,lng_ret_full,sht_ret_full,wts_int_full[-1,]))
  WEIGHT_OP[z,] = wts_int_full[(dim(wts_int_full)[1]),]
  #write.csv(ret_fin_full, file = "ret_fin_full.csv")
  
  #write.csv(ret_fin_full,paste(path_output,fac_list[z,2],"_",uni_list[u,2],"_FULL.csv"),row.names=FALSE)
  #write.csv(ret_fin_full,paste(path_output,names(fac_list)[z],"_FULL.csv"),row.names=FALSE)
  ################################################################################################################################
  
  ################################################################################################################################
#   ret_full_cmbn = data.frame(cbind(ret_full_cmbn,tot_ret_full))
#   colnames(ret_full_cmbn)[z+1]= names(fac_list)[z]
#   
#   ret_full_cmbn_L = data.frame(cbind(ret_full_cmbn_L,lng_ret_full))
#   colnames(ret_full_cmbn_L)[z+1]= names(fac_list)[z]
#   
#   ret_full_cmbn_S = data.frame(cbind(ret_full_cmbn_S, sht_ret_full))
#   colnames(ret_full_cmbn_S)[z+1]= names(fac_list)[z]
  
}


FINAL_FILE = data.frame((cbind(DATE,WEIGHT_OP)))
rownames(FINAL_FILE) <- names(list_full)
#rownames(WEIGHT_OP) <- names(list_full)
#write.csv(t(WEIGHT_OP),paste(path_output,"MN ALLOC.csv"))
write.csv(t(FINAL_FILE),paste(path_output,"MN ALLOC 22-05-2017_MORNING.csv"))
