# This is the code for creating return, given stock list and weights , every rebalancing date

##################################################### Libraries

library("xts")
library("zoo")
library("matrixStats")

############################ Inputs to the code
setwd("P:/HFT Quant/Shared folder/Mitali/LIVE DATABASE")
path_input<-"P:/HFT Quant/Shared folder/Mitali/LIVE DATABASE/INPUT/"
path_output<-"P:/HFT Quant/Shared folder/Mitali/LIVE DATABASE/OUTPUT/"

stck_lvl<-data.frame(read.csv(paste(path_input,"FutDB/","RollAdjRet.csv",sep="")))
stck_lvl$Date<-as.Date(stck_lvl$Date,"%d/%m/%Y")

bna<-1  # Keep this 1 if you need beta neutral
topbot<-2 # Make this 1 if you neeed top x and bottom x equay weighted
xnum<-20  # This is the number of stocks you need to select both on the top side and bottom side
dsort <-0 # For enabling double sort
dnum1<-25 # No. of stocks for primary factor
dnum2<-10 # no. of stocks for sec factor 
NOS <-15 # No. of strategies
##############################################

tc<-(0.0004)                                # This is one side trnsaction cost 

############################################################ Factor inout, can be daily_monthly but in same order

#clafic<-data.frame(read.csv(paste(path_input,"sector neutral.csv",sep="")))[,-1]    # This means the sectors applicable stocks                   # This will differentiate between sectors or others
#clafic<-data.frame(read.csv(paste(path_input,"Sectors.csv",sep="")))[,-1]    # This means the sectors applicable stocks                   # This will differentiate between sectors or others

#n<-max(clafic)                                # This signifies the numbers of sectors etc

#################################################################################################################################################################
WEIGHT_OP<-data.frame(matrix(0,NOS,dim(stck_lvl)[2]-1))
colnames(WEIGHT_OP)<- colnames(stck_lvl[1:NOS,-1])
list_full <- vector('list',NOS)


#################################################################################################################################################################
raw_cash_close= data.frame(read.csv(paste(path_input,"CashDB/","px_last.csv",sep="")))
raw_fut_close= data.frame(read.csv(paste(path_input,"FutDB/","px_last.csv",sep="")))
raw_basis = raw_cash_close
raw_basis[,-1] = (raw_cash_close[,-1] - raw_fut_close[,-1])/(raw_cash_close[,-1])

lookback_period=c(5,10,20)

for(l in 1:length(lookback_period)){
  
  wq1<-raw_basis
  dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  rollfin<-data.frame(DATE=dt,rollfin)
  
  list_full[[l]] <-rollfin      
}

#################################################################################################################################################################
RAW_BEST_AR= data.frame(read.csv(paste(path_input,"AnalystDB/","BEST_ANALYST_RATING.csv",sep="")))
RAW_BEST_TP= data.frame(read.csv(paste(path_input,"AnalystDB/","BEST_TARGET_PRICE.csv",sep="")))
RAW_TR= data.frame(read.csv(paste(path_input,"CashDB/","tot_analyst_rec.csv",sep="")))
RAW_BR= data.frame(read.csv(paste(path_input,"CashDB/","TOT_BUY_REC.csv",sep="")))
RAW_SR = data.frame(read.csv(paste(path_input,"CashDB/","TOT_SELL_REC.csv",sep="")))

RAW_BT<-RAW_TR
dt<-RAW_BT[1:nrow(RAW_BT),1]
x = RAW_BR[,-1]/RAW_TR[,-1]
x<-as.matrix(x)
x[which(!is.finite(x))] <- NA
RAW_BT<-data.frame(DATE=dt,x)


RAW_ST<-RAW_TR
dt<-RAW_ST[1:nrow(RAW_ST),1]
y = RAW_SR[,-1]/RAW_TR[,-1]
y<-as.matrix(y)
y[which(!is.finite(y))] <- NA
RAW_ST<-data.frame(DATE=dt,y)



lookback_period=c(20,60)

for(l in 1:length(lookback_period)){
  
  wq1<-RAW_BEST_AR
  dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  rollfin<- wq1[,-1]/rollfin
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  rollfin<-data.frame(DATE=dt,rollfin)
  
  list_full[[l+3]] <-rollfin      
}


for(l in 1:length(lookback_period)){
  
  wq1<-RAW_BEST_TP
  dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  rollfin<- wq1[,-1]/rollfin
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  rollfin<-data.frame(DATE=dt,rollfin)
  
  list_full[[l+2+3]] <-rollfin      
}

for(l in 1:length(lookback_period)){
  
  wq1<-RAW_BT
  dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  rollfin<- wq1[,-1]/rollfin
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  rollfin<-data.frame(DATE=dt,rollfin)
  
  list_full[[l+4+3]] <-rollfin      
}

for(l in 1:length(lookback_period)){
  
  wq1<-RAW_ST
  dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  rollfin<- wq1[,-1]/rollfin
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-(-1*as.matrix(rollfin))
  rollfin[which(!is.finite(rollfin))] <- NA
  rollfin<-data.frame(DATE=dt,rollfin)
  
  list_full[[l+6+3]] <-rollfin      
}
####################################################################################################################################################################################################
RAW_PUT25D= data.frame(read.csv(paste(path_input,"CashDB/","1M_PUT_IMP_VOL_25DELTA_DFLT.csv",sep="")))
RAW_CALL25D= data.frame(read.csv(paste(path_input,"CashDB/","1M_CALL_IMP_VOL_25DELTA_DFLT.csv",sep="")))
RAW_PUT40D= data.frame(read.csv(paste(path_input,"CashDB/","1M_PUT_IMP_VOL_40DELTA_DFLT.csv",sep="")))
RAW_CALL40D= data.frame(read.csv(paste(path_input,"CashDB/","1M_CALL_IMP_VOL_40DELTA_DFLT.csv",sep="")))


lookback_period=c(10,20)

for(l in 1:length(lookback_period)){
  
  wq1<-RAW_PUT25D
  dt<-wq1[1:nrow(wq1),1]
  roll1<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin1 <- data.frame(roll1)
  
  wq2<-RAW_CALL25D
  
  roll2<-rollapply(wq2[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin2 <- data.frame(roll2)
  
  
  rollfin <- rollfin1/rollfin2
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  
  
  rollfin<-data.frame(DATE=dt,rollfin)
  
  list_full[[l+11]] <-rollfin      
}

for(l in 1:length(lookback_period)){
  
  wq1<-RAW_PUT40D
  dt<-wq1[1:nrow(wq1),1]
  roll1<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin1 <- data.frame(roll1)
  
  wq2<-RAW_CALL40D
  
  roll2<-rollapply(wq2[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin2 <- data.frame(roll2)
  
  
  rollfin <- rollfin1/rollfin2
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  
  
  rollfin<-data.frame(DATE=dt,rollfin)
  
  list_full[[l+2+11]] <-rollfin      
}

######################################################################################################################################################################################

names(list_full) <- list("PMN06","PMN07","PMN08","PMN11","PMN12","PMN13","PMN14","PMN15", "PMN16" ,"PMN17" ,"PMN18","PMN21","PMN22","PMN23","PMN24")
SNAMES <- data.frame(names(list_full))
colnames(SNAMES) <- "MN WEIGHTS"
DATE_TEMP = stck_lvl[nrow(stck_lvl),1]
DATE = data.frame(DATE_TEMP)
for(t in 2:length(list_full))
{
  DATE = cbind(DATE,DATE_TEMP)
}
DATE=t(DATE)
colnames(DATE) <- "PORTFOLIO_DATE"
#names(list_full) <- list("BASIS 5","BASIS 10","BASIS 20","ANALYST 20","ANALYST 60","PT 20","PT 60","BUY 20", "BUY 60" ,"SELL 20" ,"SELL 60","25 DELTA 10","25 DELTA 20","40 DELTA 10","40 DELTA 20")
#names(list_op) <- list("25 DELTA 10","25 DELTA 20","40 DELTA 10","40 DELTA 20")

#names(list_est) <- list("ANALYST 20","ANALYST 60","PT 20","PT 60","BUY 20", "BUY 60" ,"SELL 20" ,"SELL 60")

#names(list_basis) <- list("BASIS 5","BASIS 10","BASIS 20")

#####################################################################################################################################################################################

fac_list = list_full


for(z in 1 : length(fac_list))
{
  
  fac_1<-fac_list[[z]] # factor file input
  
  #stapp<-data.frame(read.csv(paste(uni_list[u,1],"/",uni_list[u,2],".csv",sep="")))
  
  if(z <4)
  {
    stapp = data.frame(read.csv(paste(path_input,"UNIVERSE/","ACTIVEFO.csv",sep="")))
    
  }
  
  if(z>=4 && z<12){
    stapp = data.frame(read.csv(paste(path_input,"UNIVERSE/","TOP 100 ANALYST.csv",sep="")))
  }
  
  if(z>=12 && z<=length(fac_list)){
    stapp = data.frame(read.csv(paste(path_input,"UNIVERSE/","TOP 100 OPTIONS.csv",sep="")))
  }
  
  
  f_factor<-data.frame(fac_1)                                 # This will be the final clculate foactor to be used for ranking
  f_factor<-(f_factor[,-1])
  
  f_factor[stapp[,-1]==0]<-NA
  f_factor[f_factor<0.0000001 & f_factor>-0.0000001]<-NA
  
  rnk <- data.frame(t(apply(f_factor, 1, rank,na.last="keep",ties.method="max")))
  fac1_rank <-rnk
  
  #fac_clas<-rnk             # Change this based on the desired factor
  fac_clas<-rnk  
  
  #bbeta<-data.frame(read.csv(paste(path_input,"bbeta.csv",sep="")))            # Here input beta's for all the sectors
  
  if(dsort!=1){
    
    wts_fin<-NULL
    
    #for (i in 1:n){
    
    #fac_clas_sel<-fac_clas[,clafic[1,]==i]   # This will select the particular sector
    
    fac_clas_sel_1<-as.matrix(fac_clas)
    
    
    #fac_clas_sel_2<-(fac_clas_sel_1-rowSums(fac_clas_sel_1,na.rm=TRUE)/(dim(fac_clas_sel_1)[2]-rowSums(is.na(fac_clas_sel_1))))/rowSums(abs(fac_clas_sel_1-rowSums(fac_clas_sel_1,na.rm=TRUE)/(dim(fac_clas_sel_1)[2]-rowSums(is.na(fac_clas_sel_1)))),na.rm=TRUE)
    
    fac_clas_sel_2<-(fac_clas_sel_1-rowMeans(fac_clas_sel_1,na.rm=TRUE))/rowSums(abs(fac_clas_sel_1-rowMeans(fac_clas_sel_1,na.rm=TRUE)),na.rm=TRUE)
    
    #fac_clas_sel_2<-(fac_clas_sel_1-rowMedians(fac_clas_sel_1,na.rm=TRUE))/rowSums(abs(fac_clas_sel_1-rowMedians(fac_clas_sel_1,na.rm=TRUE)),na.rm=TRUE)
    
    #fac_clas_sel_2<-((fac_clas_sel_1-rowMeans(fac_clas_sel_1,na.rm=TRUE))/sd(fac_clas_sel_1[,1:dim(fac_clas_sel_1)[2]],na.rm=TRUE))/rowSums(abs((fac_clas_sel_1-rowMeans(fac_clas_sel_1,na.rm=TRUE))/sd(fac_clas_sel_1[,1:dim(fac_clas_sel_1)[2]],na.rm=TRUE)),na.rm=TRUE)
    
    #fac_clas_sel_2[is.na(fac_clas_sel_2)]<-0    # This will give 0 zero wts to the ones whic are nt active
    # LNo need for this, remove this and change the below adjusted for NA
    
    wts_ini<-data.frame(fac_clas_sel_2)
    
    wts_tb<-wts_ini
    wts_ini_top<-matrix(0,dim(wts_ini)[1],dim(wts_ini)[2])
    wts_ini_bot<-matrix(0,dim(wts_ini)[1],dim(wts_ini)[2])
    
    
    if (topbot==1){  # This will select the top x and bottom x stocks and equaly weigh the same
      
      
      
      rnk_top<-data.frame(t(apply(wts_ini, 1, rank,na.last="keep",ties.method="first"))) 
      #rnk_top<-rnk
      
      rnk_bot<-data.frame(t(apply(wts_ini, 1, rank,na.last="keep",ties.method="first"))) 
      #rnk_bot<-rnk
      
      ############ The below will calculate the top and bottom ranks
      
      topminc<-data.frame((apply(rnk_top,1,max,na.rm=TRUE)))
      topminc[topminc<=xnum]<-xnum     # This is to make sure that the total number is not less than selected numbers
      
      topminc_cbnd <- topminc-xnum +1
      
      for (j in 2:dim(wts_ini)[2] ){
        topminc_cbnd<-cbind(topminc_cbnd,topminc-xnum +1)
        
      }
      
      wts_ini_top[rnk_top>=topminc_cbnd]<-1/(xnum)
      wts_ini_top<-data.frame(wts_ini_top)
      
      
      botminc<-data.frame((apply(rnk_bot,1,max,na.rm=TRUE)))
      botminc[botminc<=xnum]<-xnum
      
      botminc[]<-xnum
      
      botminc_cbnd <- botminc
      
      for (j in 2:dim(wts_ini)[2] ){
        botminc_cbnd<-cbind(botminc_cbnd,botminc)
        
      }
      
      
      wts_ini_bot[rnk_bot<=botminc_cbnd]<-(-1/(xnum))
      
      wts_ini_bot<-data.frame(wts_ini_bot)
      
      
      colnames(wts_ini_bot)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
      colnames(wts_ini_top)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
      
      
      topname<-matrix(0,dim(wts_ini_top)[1],xnum)
      botname<-matrix(0,dim(wts_ini_bot)[1],xnum)
      
      
      
      
      for (k in 1:dim(wts_ini_bot)[1] ){
        
        if((sum(wts_ini_top[k,]>0) ==10) && (sum(wts_ini_bot[k,]<0) ==10))
          
        {
          topname[k,]<-colnames(wts_ini_top[k,wts_ini_top[k,]>0])
          botname[k,]<-colnames(wts_ini_bot[k,wts_ini_bot[k,]<0])
        }
        else
        {
          wts_ini_top[k,]<-0
          wts_ini_bot[k,]<-0
        }
        
      }
      
      
    }
    ####################################################################################################################################################
    
    
    if (topbot==2){  # This will select the top x and bottom x stocks and equaly weigh the same
      
      
      
      rnk_top<-data.frame(t(apply(wts_ini, 1, rank,na.last="keep",ties.method="first"))) 
      #rnk_top<-rnk
      
      rnk_bot<-data.frame(t(apply(wts_ini, 1, rank,na.last="keep",ties.method="first"))) 
      #rnk_bot<-rnk
      
      ############ The below will calculate the top and bottom ranks
      topminc<-data.frame((apply(fac1_rank,1,max,na.rm=TRUE)))
      #topminc<-data.frame((apply(rnk_top,1,max,na.rm=TRUE)))
      topminc[topminc<=xnum]<-xnum     # This is to make sure that the total number is not less than selected numbers
      
      topminc_cbnd <- topminc-xnum +1
      
      for (j in 2:dim(wts_ini)[2] ){
        topminc_cbnd<-cbind(topminc_cbnd,topminc-xnum +1)
        
      }
      
      botminc<-data.frame((apply(fac1_rank,1,max,na.rm=TRUE)))
      
      botminc[]<-xnum
      
      botminc_cbnd <- botminc
      
      for (j in 2:dim(wts_ini)[2] ){
        botminc_cbnd<-cbind(botminc_cbnd,botminc)
        
      }
      
      wts_tb[fac1_rank<topminc_cbnd & fac1_rank >botminc_cbnd] <-NA
      wts_tb<-wts_tb/rowSums(abs(wts_tb),na.rm=TRUE)
      
      wts_tb[is.na(wts_tb)]<-0
      wts_tb<-data.frame(wts_tb)
      
      wts_ini_top<-wts_tb
      wts_ini_bot<-wts_tb
      
      wts_ini_top[wts_tb<=0]<-0
      wts_ini_bot[wts_tb>=0]<-0
      
      
      colnames(wts_ini_bot)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
      colnames(wts_ini_top)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
      
      
      topname<-matrix(0,dim(wts_ini_top)[1],xnum)
      botname<-matrix(0,dim(wts_ini_bot)[1],xnum)
      
      
      
      
      for (k in 1:dim(wts_ini_bot)[1] ){
        
        if((sum(wts_ini_top[k,]>0) ==xnum) && (sum(wts_ini_bot[k,]<0) ==xnum))
          
        {
          topname[k,]<-colnames(wts_ini_top[k,wts_ini_top[k,]>0])
          botname[k,]<-colnames(wts_ini_bot[k,wts_ini_bot[k,]<0])
        }
        else
        {
          wts_ini_top[k,]<-0
          wts_ini_bot[k,]<-0
        }
        
      }
      
      
    }
    
    ####################################################################################################################################################
    
    
    #wts_sel_int<-wts_ini[wts_ini[2,]==i]     # This is the internal wts for calculation
    
    wts_fin<-wts_ini
    
    
    
    
    ################################## Beta Neutral amongst sectors
    
    #     if (bna==1){        # This will check if beta neutral needs to be done or not
    #       
    #       brtio<-data.frame((rowSums(wts_sel_int_pos*bbeta[,clafic[1,]==i][,-1]))/abs(rowSums(wts_sel_int_neg*bbeta[,clafic[1,]==i][,-1])))
    #       # This will quantify the beta ratio between long and short in the portfolio
    #       
    #       wts_sel_int[wts_sel_int>=0]<-(wts_sel_int[wts_sel_int>=0]/brtio)  # This will change the weights of long to make it beta neutral
    #       
    #     }
    #          
    ###############################################################
    
    # This will supply beta neutral weights to the main weights table
    
    
    wts_fin[is.na(wts_fin)]<-0
    
    wts_int_full<-data.frame((wts_fin))
    
    
    wts_int_pos_full<-wts_int_full
    wts_int_neg_full<-wts_int_full
    
    wts_int_pos_full[wts_int_full<=0]<-0
    wts_int_neg_full[wts_int_full>=0]<-0
    
    
    ##############################################################################################################################
    wts_int_pos_full_calc<-wts_int_pos_full[c(-1*dim(wts_int_full)[1],-1*dim(wts_int_full)[1] +1),]             #Full universe factor returns
    wts_int_neg_full_calc<-wts_int_neg_full[c(-1*dim(wts_int_full)[1],-1*dim(wts_int_full)[1] +1),]
    
    
    #wts_int_pos_full_calc<-wts_int_pos_full_calc[-1*dim(wts_int_pos_full_calc)[1],]             #Full universe factor returns
    #wts_int_neg_full_calc<-wts_int_neg_full_calc[-1*dim(wts_int_neg_full_calc)[1],]
    
    stck_lvl_calc<-stck_lvl[c(-1,-2),-1]
    
    lng_ret_full<-data.frame(rowSums(wts_int_pos_full_calc*stck_lvl_calc))
    sht_ret_full<-data.frame(rowSums(wts_int_neg_full_calc*stck_lvl_calc))
    
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
    
    
    cst_full<-data.frame(rowSums(abs(wts_int_full[-1,]-wts_int_full[-1*dim(wts_int_full)[1],])*tc))
    cst_full<-cst_full[-1*dim(cst_full)[1],]
    
    cst_full_L<-data.frame(rowSums(abs(wts_int_pos_full[-1,]-wts_int_pos_full[-1*dim(wts_int_pos_full)[1],])*tc))
    cst_full_L<-cst_full_L[-1*dim(cst_full_L)[1],]
    
    cst_full_S<-data.frame(rowSums(abs(wts_int_neg_full[-1,]-wts_int_neg_full[-1*dim(wts_int_neg_full)[1],])*tc))
    cst_full_S<-cst_full_S[-1*dim(cst_full_S)[1],]
    
    
    #lng_ret_full<-data.frame(rowSums(wts_int_pos_full_calc*stck_lvl_calc))
    #tot_ret_full<-data.frame((wts_int_full[c(-1*dim(wts_int_full)[1],-1*dim(wts_int_full)[1] +1),]*stck_lvl_calc)-cst_full)  
    
    tot_ret_full<-data.frame((lng_ret_full+sht_ret_full)-cst_full)
    
    lng_ret_full<-data.frame(lng_ret_full-cst_full_L)
    sht_ret_full<-data.frame(sht_ret_full-cst_full_S)
    
    ret_fin_full<-data.frame(cbind(stck_lvl$Date[c(-1,-2)],tot_ret_full,lng_ret_full,sht_ret_full,wts_int_full[c(-1,-1*dim(wts_int_full)[1]),]))
    
    #write.csv(ret_fin_full, file = "ret_fin_full.csv")
    
    #write.csv(ret_fin_full,paste(path_output,fac_list[z,2],"_",uni_list[u,2],"_FULL.csv"),row.names=FALSE)
    #write.csv(ret_fin_full,paste(path_output,names(fac_list)[z],"_FULL.csv"),row.names=FALSE)
    ################################################################################################################################
    
    ################################################################################################################################
    if (topbot==2){       
      
      wts_ini_full_tb <- (wts_tb)
      
      
      wts_ini_top_calc<-wts_ini_top[c(-1*dim(wts_ini_top)[1],-1*dim(wts_ini_top)[1]+1),]                        #return calculation for top-bot 10
      wts_ini_bot_calc<-wts_ini_bot[c(-1*dim(wts_ini_bot)[1],-1*dim(wts_ini_bot)[1]+1),]
      
      #wts_ini_top_calc<-wts_ini_top_calc[-1*dim(wts_ini_top_calc)[1],]                        #return calculation for top-bot 10
      #wts_ini_bot_calc<-wts_ini_bot_calc[-1*dim(wts_ini_bot_calc)[1],]
      
      #cst_tb<-data.frame(rowSums(abs(wts_ini_full_tb[-1,]-wts_ini_full_tb[-1*dim(wts_ini_full_tb)[1],])*tc))
      
      #lng_cst_tb <- data.frame(rowSums(abs(wts_ini_top[-1,]-wts_ini_top[-1*dim(wts_ini_top)[1],])*tc/2))
      
      #stck_lvl_calc<-stck_lvl[-1,-1]
      
      lng_ret_tb<-data.frame(rowSums(wts_ini_top_calc*stck_lvl_calc))
      sht_ret_tb<-data.frame(rowSums(wts_ini_bot_calc*stck_lvl_calc))
      
      
      lng_beta_tb<- matrix(1,dim(lng_ret_tb)[1],1)
      sht_beta_tb<- matrix(1,dim(sht_ret_tb)[1],1)
      
      
      if (bna==1){        # This will check if beta neutral needs to be done or not
        
        
        for(m in 251: dim(lng_beta_tb)[1])     # This will quantify the beta ratio between long and short in the portfolio
        {                                  
          
          #print(m)  
          
          lng_beta_tb[m,1]<- (cov(lng_ret_tb[(m-250):(m-1),1],stck_lvl[c(-1,-2),2][(m-250):(m-1)])/var(stck_lvl[c(-1,-2),2][(m-250):(m-1)]))
          sht_beta_tb[m,1]<- -(cov(sht_ret_tb[(m-250):(m-1),1],stck_lvl[c(-1,-2),2][(m-250):(m-1)])/var(stck_lvl[c(-1,-2),2][(m-250):(m-1)]))
        }
        
        wts_ini_bot_calc<-wts_ini_bot_calc*(lng_beta_tb/sht_beta_tb)
        wts_ini_full_tb[c(-1*dim(wts_ini_full_tb)[1],-1*dim(wts_ini_full_tb)[1] +1),] <-(wts_ini_top_calc + wts_ini_bot_calc)
        wts_ini_full_tb[(dim(wts_ini_full_tb)[1]-1),] <- wts_ini_top[(dim(wts_ini_full_tb)[1]-1),]+wts_ini_bot[(dim(wts_ini_full_tb)[1]-1),]*(lng_beta_tb[dim(lng_beta_tb)[1],1]/sht_beta_tb[dim(sht_beta_tb)[1],1])
        wts_ini_bot<-wts_ini_full_tb
        wts_ini_bot[wts_ini_full_tb>=0]<-0
        sht_ret_tb<-data.frame(rowSums(wts_ini_bot_calc*stck_lvl_calc))
        
      } 
      
      cst_tb<-data.frame(rowSums(abs(wts_ini_full_tb[-1,]-wts_ini_full_tb[-1*dim(wts_ini_full_tb)[1],])*tc))
      cst_tb<-cst_tb[-1*dim(cst_tb)[1],]
      
      cst_tb_L<-data.frame(rowSums(abs(wts_ini_top[-1,]-wts_ini_top[-1*dim(wts_ini_top)[1],])*tc))
      cst_tb_L<-cst_tb_L[-1*dim(cst_tb_L)[1],]
      
      cst_tb_S<-data.frame(rowSums(abs(wts_ini_bot[-1,]-wts_ini_bot[-1*dim(wts_ini_bot)[1],])*tc))
      cst_tb_S<-cst_tb_S[-1*dim(cst_tb_S)[1],]
      
      
      tot_ret_tb<-data.frame((lng_ret_tb+sht_ret_tb) - cst_tb)
      
      lng_ret_tb<-data.frame(lng_ret_tb-cst_tb_L)
      sht_ret_tb<-data.frame(sht_ret_tb-cst_tb_S)
      
      #ret_fin_tb<-data.frame(cbind(stck_lvl$Returns[c(-1,-2)],tot_ret_tb,lng_ret_tb,sht_ret_tb,topname[c(-1,-1*dim(topname)[1]),],botname[c(-1,-1*dim(botname)[1]),]))
      
      ret_fin_tb<-data.frame(cbind(stck_lvl$Date[c(-1,-2)],tot_ret_tb,lng_ret_tb,sht_ret_tb,wts_ini_full_tb[c(-1,-1*dim(wts_ini_full_tb)[1]),]))
      WEIGHT_OP[z,] = wts_ini_full_tb[(dim(wts_ini_full_tb)[1]-1),]
      
      #ret_fin_tb1<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sum))
      #ret_fin_tb2<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),mean))
      #ret_fin_tb3<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sd))
      
      #write.csv(ret_fin_tb, file = "ret_fin_tb.csv")
      #write.csv(ret_fin_tb,paste(path_output,fac_list[z,2],"_",uni_list[u,2],"_TOPBOT.csv"),row.names=FALSE)
      #write.csv(ret_fin_tb,paste(path_output,names(fac_list)[z],"_TOPBOT.csv"),row.names=FALSE)
      
    }
  }
  #ret_full_cmbn = data.frame(cbind(ret_full_cmbn,tot_ret_full))
  #colnames(ret_full_cmbn)[z+1]= names(fac_list)[z]
  
  #ret_full_cmbn_L = data.frame(cbind(ret_full_cmbn_L,lng_ret_full))
  #colnames(ret_full_cmbn_L)[z+1]= names(fac_list)[z]
  
  #ret_full_cmbn_S = data.frame(cbind(ret_full_cmbn_S, sht_ret_full))
  #colnames(ret_full_cmbn_S)[z+1]= names(fac_list)[z]
  
  #if (topbot==2){   
  #ret_tb_cmbn = data.frame(cbind(ret_tb_cmbn,tot_ret_tb))
  #colnames(ret_tb_cmbn)[z+1]= names(fac_list)[z]
  
  #ret_tb_cmbn_L = data.frame(cbind(ret_tb_cmbn_L,lng_ret_tb))
  #colnames(ret_tb_cmbn_L)[z+1]= names(fac_list)[z]
  
  # ret_tb_cmbn_S = data.frame(cbind(ret_tb_cmbn_S, sht_ret_tb))
  #colnames(ret_tb_cmbn_S)[z+1]= names(fac_list)[z]
  
  #}
  
}
FINAL_FILE = data.frame((cbind(DATE,WEIGHT_OP)))
rownames(FINAL_FILE) <- names(list_full)
#rownames(WEIGHT_OP) <- names(list_full)
#write.csv(t(WEIGHT_OP),paste(path_output,"MN ALLOC.csv"))
write.csv(t(FINAL_FILE),paste(path_output,"MN ALLOC 22-05-2017_MORNING.csv"))
