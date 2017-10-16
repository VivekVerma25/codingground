
##################################################### Libraries

library("xts")
library("zoo")
library("matrixStats")
library("TTR")
library("quantmod")
library("PerformanceAnalytics")
library("plyr")
library("stringi")
library("lubridate")
library("caTools")

############################ Inputs to the code

setwd("F:/DT/ALPHAGREP/OUTPUT/")
path_input<-"F:/DT/ALPHAGREP/INPUT/"
FOLDER_NAME = "AUTOCORR PRICE GRP1"
dir.create(FOLDER_NAME)
path_output<-paste("F:/DT/ALPHAGREP/OUTPUT/",FOLDER_NAME,"/",sep="")
source("F:/LFT/MN/ALL R CODES/R_FUNC_FULL.R")

RAW_CLOSE= data.frame(read.csv(paste(path_input,"CASH DB/","px_last_adjusted.csv",sep="")))
RAW_CLOSE[,1] = as.Date(RAW_CLOSE[,1],"%Y-%m-%d")

stck_lvl<-data.frame(read.csv(paste(path_input,"CASH DB/","stock_returns.csv",sep="")))     # INPUT FOR RETURN SERIES OF FUTURES/STOCKS
stck_lvl[,1]<-RAW_CLOSE[,1] # Convert to R date format
colnames(stck_lvl)[1]="Date"

stck_lvl1<-data.frame(read.csv(paste(path_input,"CASH DB/","index_ret.csv",sep="")))     # INPUT FOR RETURN SERIES OF FUTURES/STOCKS
stck_lvl1[,1]<-RAW_CLOSE[,1] # Convert to R date format


bna<-0  # BETA NEUTRAL: Keep this 1 if you need beta neutral , 0 : DOLLAR NEUTRAL
grouping = 1
topbot<-0 # 2 : TOP/BOTTOM RANK WEIGHTED, 1 : TOP/BOTTOM EQUAL WEIGHTED, SINGLE FACTOR TOP/BOT, 3: LOW SLOPE RANK WEIGHING
ptop = 0.85 # For defining portfolio based on top percentile, SINGLE FACTOR
pbot = 0.15 # For defining portfolio based on bot percentile, SINGLE FACTOR
tc<-(0.000175)  # This is one side trnsaction cost 
dsort=0 # For enabling DOUBLE SORT, 1: for DSORT, 0: SIngle factor
fac1_per = 0.70 # For defining portfolio of based on percentiles (0-1) for 1st FACTOR
fac2_per = 0.85 # For defining portfolio of based on percentiles (0-1) for 2nd FACTOR
liq_weight = 0 # LIQUIDITY WEIGHTS: Keep this 1 for Liqudity weighing of stock portfolio (Both for SINGLE AND MULTIFACTOR)
turnover_cutoff = 0.75 # Percentile cuttoff for turnover within each LONG and short portfolio
stock_cap = 0.1 # MAx percentage Allocation on a stock in the Portflio
n = 5 #Number of recursions for checking Max stock weight



######################################################################################################################################################
RAW_CLOSE= data.frame(read.csv(paste(path_input,"CASH DB/","px_last_adjusted.csv",sep="")))
RAW_HIGH= data.frame(read.csv(paste(path_input,"CASH DB/","px_high_adjusted.csv",sep="")))
RAW_LOW= data.frame(read.csv(paste(path_input,"CASH DB/","px_low_adjusted.csv",sep="")))
RAW_OPEN= data.frame(read.csv(paste(path_input,"CASH DB/","px_open_adjusted.csv",sep="")))


RAW_CLOSE[,-1] = as.numeric(as.matrix(RAW_CLOSE[,-1]))
RAW_HIGH[,-1] = as.numeric(as.matrix(RAW_HIGH[,-1]))
RAW_LOW[,-1] = as.numeric(as.matrix(RAW_LOW[,-1]))
RAW_OPEN[,-1] = as.numeric(as.matrix(RAW_OPEN[,-1]))

#RAW_VOL[,-1] = RAW_VOL[,-1]/100 
# raw_basis = raw_cash_close
# raw_basis[,-1] = (raw_cash_close[,-1] - raw_fut_close[,-1])/(raw_cash_close[,-1])
# 
#
# DVD_SH1 = DVD_SH
# temp = as.matrix(DVD_SH1[,-1])
# temp[which(!is.finite(temp))] = NA
# temp[is.na(temp)] = 0
# DVD_SH1[,-1] = temp

lookback_period=c(5,10,20,40,60,125,250)
# lookback_period2=c(20,40,60)

#list_factor1 <- vector('list',length(lookback_period1)*length(lookback_period2))
list_factor1 =vector('list',7)

for(l in 1:length(lookback_period)){
  
  
  list_factor1[[l]] = ROLL_CORRELATION(RAW_CLOSE,DELAY(RAW_CLOSE,1),lookback_period[[l]])
  
}

# 
# for(l in 1:length(lookback_period)){
#   
#   
#   list_factor1[[l+5]] = ROLL_CORRELATION(RAW_VOL,DELAY(RAW_VOL,1),lookback_period[[l]])
#   
#   
# }
# 
# 
# for(l in 1:length(lookback_period)){
#   
#   
#   list_factor1[[l+10]] = ROLL_CORRELATION(RAW_DVOL,DELAY(RAW_DVOL,1),lookback_period[[l]])
#   
# }
# 




# 
# for(ll in 1: length(lookback_period1)){
#   
#   for(l in 1:length(lookback_period2)){
#     
#     # wq1<-raw_basis
#     # dt<-wq1[1:nrow(wq1),1]
#     # roll<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
#     # rollfin <- data.frame(roll)
#     # 
#     # #rollfin[is.na(rollfin)] =0
#     # rollfin<-as.matrix(rollfin)
#     # rollfin[which(!is.finite(rollfin))] <- NA
#     # rollfin<-data.frame(DATE=dt,rollfin)
#     
#     list_factor1[[(length(lookback_period2))*(ll-1) +l ]] <-  TS_RANK(ROLL_MEAN(raw_basis,lookback_period2[l]),lookback_period1[l])    
#   }
#   
# }
names(list_factor1) <- list("PRICE MOM 5","PRICE MOM 10","PRICE MOM 20","PRICE MOM 40","PRICE MOM 60","PRICE MOM 125","PRICE MOM 250")

######################################################################## FACTOR 2
#list_factor2 <- vector('list',2)

if (dsort == 1)
{
  
  list_full <- vector('list',51)
  
  ROE= data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Annual/","roe.csv",sep="")))
  ATOR= data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Annual/","atover.csv",sep="")))
  PAT= data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Quarterly/","PAT.csv",sep="")))
  TA = data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Annual/","totalasset.csv",sep="")))
  
  
  ROE = DELAY(ROE,80)
  
  ATOR = DELAY(ATOR,80)
  
  TA = DELAY(TA,80)
  
  list_full[[1]] = ROE
  list_full[[2]] = ATOR
  ATOR_YOY = PER_CHANGE(ATOR,250)
  
  ######################################################################################################################################################
  PAT_A= PAT
  PAT_A[,-1] = (PAT[,-1] + DELAY(PAT,70)[,-1] + DELAY(PAT,140)[,-1] + DELAY(PAT,210)[,-1])
  
  PAT_TA = PAT_A
  PAT_TA[,-1] = PAT_A[,-1]/TA[,-1]
  
  rollfin<-as.matrix(PAT_TA[,-1])
  rollfin[which(!is.finite(rollfin))] <- NA
  PAT_TA<-data.frame(stck_lvl$Date,rollfin)
  
  ROA=PAT_TA
  list_full[[3]] = PAT_TA
  ######################################################################################################################################################
  PBIDT = data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Annual/","PBIDT.csv",sep="")))
  PBIDT = DELAY(PBIDT,80)
  
  PBIDT_TA = PBIDT
  PBIDT_TA[,-1] = PBIDT[,-1]/TA[,-1]
  
  rollfin<-as.matrix(PBIDT_TA[,-1])
  rollfin[which(!is.finite(rollfin))] <- NA
  PBIDT_TA<-data.frame(stck_lvl$Date,rollfin)
  list_full[[4]] = PBIDT_TA
  PBIDT_TA_YOY = PER_CHANGE(PBIDT_TA,250)
  ######################################################################################################################################################
  
  TD = data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Annual/","totaldebt.csv",sep="")))
  MCAP = data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Monthly/","mcap.csv",sep="")))
  DE = data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Annual/","de.csv",sep="")))
  INV = data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Annual/","inventories.csv",sep="")))
  
  TD = DELAY(TD,80)
  DE =DELAY(DE,80)
  INV = DELAY(INV,80)
  TD_YOY = NEGATIVE(PER_CHANGE(TD,250))
  MCAP_MOM = PER_CHANGE(MCAP,250)
  INV_YOY = NEGATIVE(PER_CHANGE(INV,250))
  
  DE1 = DE
  DE1[,-1] = ((-1)*DE[,-1])
  ######################################################################################################################################################
  
  
  TD_MCAP = TD
  TD_MCAP[,-1] = TD[,-1]/MCAP[,-1]
  TD_MCAP_YOY = NEGATIVE(PER_CHANGE(TD_MCAP,250))
  
  rollfin<-as.matrix(TD_MCAP[,-1])
  rollfin[which(!is.finite(rollfin))] <- NA
  TD_MCAP<-data.frame(stck_lvl$Date,((-1)*rollfin))
  list_full[[5]] = TD_MCAP
  list_full[[6]] = DE1
  ######################################################################################################################################################
  
  TD_INV = TD
  TD_INV[,-1] = TD[,-1]/INV[,-1]
  TD_INV_YOY = NEGATIVE(PER_CHANGE(TD_INV,255))
  
  rollfin<-as.matrix(TD_INV[,-1])
  rollfin[which(!is.finite(rollfin))] <- NA
  TD_INV<-data.frame(stck_lvl$Date,((-1)*rollfin))
  list_full[[7]] = TD_INV
  ######################################################################################################################################################
  PAYOUT = data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Annual/","payout.csv",sep="")))
  PROM = data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Quarterly/","prom.csv",sep="")))
  INST = data.frame(read.csv(paste(path_input,"FUNDAMENTAL/Quarterly/","inst.csv",sep="")))
  PROM1= PROM
  PROM1[,-1] = ((-1)*PROM[,-1])
  PAYOUT = DELAY(PAYOUT,80)
  PAYOUT_YOY = PER_CHANGE(PAYOUT,250)
  INST_QOQ = PER_CHANGE(INST,66)
  INST_YOY = PER_CHANGE(INST,250)
  PROM_QOQ = NEGATIVE(PER_CHANGE(INST,66))
  PROM_YOY = NEGATIVE(PER_CHANGE(INST,250))
  
  list_full[[8]] = PAYOUT
  list_full[[9]] = INST          
  list_full[[10]] = PROM1
  ######################################################################################################################################################
  DELAY_ROE = ROE[,-1]
  DELAY_ROE[c(-1:-255),] =DELAY_ROE[c((-dim(DELAY_ROE)[1]):(-dim(DELAY_ROE)[1]+255-1)),]
  
  DELTA_ROE = (ROE[,-1]-DELAY_ROE)/DELAY_ROE
  
  rollfin<-as.matrix(DELTA_ROE)
  rollfin[which(!is.finite(rollfin))] <- NA
  DELTA_ROE<-data.frame(stck_lvl$Date,rollfin)
  ######################################################################################################################################################
  
  DELAY_DE = DE[,-1]
  DELAY_DE[c(-1:-255),] =DELAY_DE[c((-dim(DELAY_DE)[1]):(-dim(DELAY_DE)[1]+255-1)),]
  
  DELTA_DE = (DE[,-1]-DELAY_DE)/DELAY_DE
  
  rollfin<-as.matrix(DELTA_DE)
  rollfin[which(!is.finite(rollfin))] <- NA
  DELTA_DE<-data.frame(stck_lvl$Date,((-1)*rollfin))
  ######################################################################################################################################################
  
  PAT_A_GROWTH = PER_CHANGE(PAT_A,250)
  PAT_Q_GROWTH = PER_CHANGE(PAT,70)
  ROA_A_GROWTH =  PER_CHANGE(ROA,250)
  ######################################################################################################################################################
  
  raw_cash_close= data.frame(read.csv(paste(path_input,"CASH DB/","px_last1.csv",sep="")))
  
  DELAY_raw_cash_close = raw_cash_close[,-1]
  DELAY_raw_cash_close[c(-1:-255),] =DELAY_raw_cash_close[c((-dim(DELAY_raw_cash_close)[1]):(-dim(DELAY_raw_cash_close)[1]+255-1)),]
  
  DELTA_raw_cash_close = (raw_cash_close[,-1]-DELAY_raw_cash_close)/DELAY_raw_cash_close
  
  rollfin<-as.matrix(DELTA_raw_cash_close)
  rollfin[which(!is.finite(rollfin))] <- NA
  MOM250 <-data.frame(stck_lvl$Date,rollfin)
  
  MOM250_YOY = PER_CHANGE(MOM250,250)
  ######################################################################################################################################################
  
  #raw_cash_close= data.frame(read.csv(paste(path_input,"CASH DB/","px_last.csv",sep="")))
  
  DELAY_raw_cash_close = raw_cash_close[,-1]
  DELAY_raw_cash_close[-1,] = DELAY_raw_cash_close[-dim(DELAY_raw_cash_close)[1],]
  
  DELTA_raw_cash_close = (raw_cash_close[,-1]-DELAY_raw_cash_close)/DELAY_raw_cash_close
  
  rollfin<-as.matrix(DELTA_raw_cash_close)
  rollfin[which(!is.finite(rollfin))] <- NA
  s<-rollapply(rollfin,250,sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  RET = data.frame(stck_lvl$Date,rollfin)
  STD_RET250 <-data.frame(stck_lvl$Date,((-1)*s))
  STD_RET250_YOY  = PER_CHANGE(STD_RET250,250)
  ######################################################################################################################################################
  
  RET1 = RET
  
  for( i in 1: (ncol(RET)-1))
  {
    RET1[,1+i] = RET[,2]
    
  }
  
  BETA250 = RET
  BETA250[,-1] = 0
  
  for(m in 251: dim(RET)[1])
    
  {
    BETA250[,-1][m,] = (cov(RET[,-1][(m-250):(m-1),],RET1[,-1][(m-250):(m-1),])/var(RET1[,-1][(m-250):(m-1),]))
    
  }
  
  #ALPHA = RET
  
  #ALPHA[,-1] = RET[,-1]- BETA250[,-1]*RET[,-1]
  #ALPHA250 = ALPHA
  
  #ALPHA250[,-1] = rollapply(ALPHA[,-1],250,mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  BETA250[,-1] = (-1)*BETA250[,-1]
  BETA250_YOY  =  PER_CHANGE(BETA250,250)
  
  
  list_full[[11]] = MOM250
  list_full[[12]] = STD_RET250
  list_full[[13]] = BETA250
  list_full[[14]] = DELTA_DE
  list_full[[15]] = DELTA_ROE
  list_full[[16]] = PAT_A_GROWTH
  list_full[[17]] = PAT_Q_GROWTH
  list_full[[18]] = ROA_A_GROWTH
  list_full[[19]] = ATOR_YOY
  list_full[[20]] = PBIDT_TA_YOY
  list_full[[21]] = TD_YOY
  list_full[[22]] = MCAP_MOM
  list_full[[23]] = INV_YOY
  list_full[[24]] = TD_MCAP_YOY
  list_full[[25]] = TD_INV_YOY
  list_full[[26]] = PAYOUT_YOY
  list_full[[27]] = INST_QOQ
  list_full[[28]] = INST_YOY
  list_full[[29]] = PROM_QOQ
  list_full[[30]] = PROM_YOY
  list_full[[31]] = MOM250_YOY
  list_full[[32]] = STD_RET250_YOY
  list_full[[33]] = BETA250_YOY
  
  
  ###################################################################################################################################################
  raw_cash_close= data.frame(read.csv(paste(path_input,"CASH DB/","px_last1.csv",sep="")))
  raw_fut_close= data.frame(read.csv(paste(path_input,"FUT DB/","px_last.csv",sep="")))
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
    
    list_full[[l+33]] <-rollfin      
  }
  
  ####################################################################################################################################################
  
  RAW_BEST_AR= data.frame(read.csv(paste(path_input,"ANALYST DB/","BEST_ANALYST_RATING.csv",sep="")))
  RAW_BEST_TP= data.frame(read.csv(paste(path_input,"ANALYST DB/","BEST_TARGET_PRICE.csv",sep="")))
  RAW_TR= data.frame(read.csv(paste(path_input,"ANALYST DB/","TOT_ANALYST_REC.csv",sep="")))
  RAW_BR= data.frame(read.csv(paste(path_input,"ANALYST DB/","TOT_BUY_REC.csv",sep="")))
  RAW_SR = data.frame(read.csv(paste(path_input,"ANALYST DB/","TOT_SELL_REC.csv",sep="")))
  
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
    
    list_full[[l+36]] <-rollfin      
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
    
    list_full[[l+38]] <-rollfin      
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
    
    list_full[[l+40]] <-rollfin      
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
    
    list_full[[l+42]] <-rollfin      
  }
  
  ####################################################################################################################################################
  
  RAW_PUT25D= data.frame(read.csv(paste(path_input,"OPTIONS DB/","1M_PUT_IMP_VOL_25DELTA_DFLT.csv",sep="")))
  RAW_CALL25D= data.frame(read.csv(paste(path_input,"OPTIONS DB/","1M_CALL_IMP_VOL_25DELTA_DFLT.csv",sep="")))
  RAW_PUT40D= data.frame(read.csv(paste(path_input,"OPTIONS DB/","1M_PUT_IMP_VOL_40DELTA_DFLT.csv",sep="")))
  RAW_CALL40D= data.frame(read.csv(paste(path_input,"OPTIONS DB/","1M_CALL_IMP_VOL_40DELTA_DFLT.csv",sep="")))
  
  
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
    
    list_full[[44+l]] <-rollfin      
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
    
    list_full[[l+46]] <-rollfin      
  }
  
  #######################################################################################################################################################
  
  RAW_PRICE= data.frame(read.csv(paste(path_input,"CASH DB/","px_last1.csv",sep="")))
  RAW_DVOL= data.frame(read.csv(paste(path_input,"CASH DB/","total_india_delivery_volume.csv",sep="")))
  
  list_P <- vector('list',3)
  list_DV <- vector('list',3)
  
  lookback_period=c(90,120,150)
  
  for(l in 1:length(lookback_period)){
    
    wq1<-RAW_PRICE
    dt<-wq1[1:nrow(wq1),1]
    m<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
    s<-rollapply(wq1[,-1],lookback_period[l],sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
    roll= (wq1[,-1]-m)/s
    final_fac = roll
    final_fac[] = NA
    
    rollfin <- data.frame(roll)
    
    #rollfin[is.na(rollfin)] =0
    rollfin<-as.matrix(rollfin)
    rollfin[which(!is.finite(rollfin))] <- NA
    rollfin<-data.frame(DATE=dt,rollfin)
    
    #list_P[[l]] <-rollfin
    
    wq2<-RAW_DVOL
    dt<-wq2[1:nrow(wq1),1]
    m2<-rollapply(wq2[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
    s2<-rollapply(wq2[,-1],lookback_period[l],sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
    roll2= (wq2[,-1]-m2)/s2
    
    rollfin2 <- data.frame(roll2)
    
    roll2[is.na(roll2)] =0
    rollfin2<-as.matrix(rollfin2)
    rollfin2[which(!is.finite(rollfin2))] <- NA
    rollfin2<-data.frame(DATE=dt,rollfin2)
    
    #list_DV[[l]] <-rollfin2
    
    for(i in 2: ncol(final_fac))
    {
      for(j in 2: nrow(final_fac))
      {
        if(roll2[j,i] > 0)
          final_fac[j,i] = roll[j,i]
        else
          final_fac[j,i] = final_fac[j-1,i]
        
      }
    }
    
    final_fac<-as.matrix(final_fac)
    final_fac[which(!is.finite(final_fac))] <- NA
    final_fac<-data.frame(DATE=dt,final_fac)
    
    list_full[[l+48]] <- final_fac
  }
  
  #######################################################################################################################################################
  
  list_factor2 = list_full
  
  names(list_factor2) <- list("ROE","ATOVER","ROA","EBITDA_TA","TOT DEBT_MCAP","TOT DEBT_E", "TOT DEBT_INV" ,"PAYOUT" ,"INSTI", "PROM","MOM250","STD_RET250", "BETA250", "DELTA_DE", "DELTA_ROE","PAT_YOY","PAT_QOQ","ROA_YOY","ATOR_YOY","EBITDA_TA_YOY","TD_DEBT_YOY","MCAP_MOM", "INVENTORY_YOY","TOT DEBT_MCAP_YOY", "TOT DEBT_INV_YOY", "PAYOUT_YOY","INST_QOQ", "INST_YOY", "PROM_QOQ","PROM_YOY", "MOM250_YOY", "STD_RET250_YOY", "BETA250_YOY", "BASIS 5","BASIS 10","BASIS 20","ANALYST 20","ANALYST 60","PT 20","PT 60","BUY 20", "BUY 60" ,"SELL 20" ,"SELL 60","25 DELTA 10","25 DELTA 20","40 DELTA 10","40 DELTA 20","MD 90","MD 120","MD 150")
  
  # names(list_factor2) <- list("MOM 125","MOM 250") # Factor2 NAMES WITH PARAMETERS USED
  ##########################################################################
  
}
#stapp = data.frame(read.csv(paste(path_input,"UNIVERSE/","ActiveFO.csv",sep=""))) # INPUT THE UNIVERSE TO RUN ON
uni_list = vector("list",1)
uni_list[[1]] = data.frame(read.csv(paste(path_input,"UNIVERSE/","FULL.csv",sep="")))
# uni_list[[2]] = data.frame(read.csv(paste(path_input,"UNIVERSE/","Nifty50.csv",sep="")))
# uni_list[[3]] = data.frame(read.csv(paste(path_input,"UNIVERSE/","Midcap50.csv",sep="")))
# uni_list[[2]] = data.frame(read.csv(paste(path_input,"UNIVERSE/","Nifty50_SHORT.csv",sep="")))
# uni_list[[3]] = data.frame(read.csv(paste(path_input,"UNIVERSE/","N50_TOP50_ANALYST.csv",sep="")))

names(uni_list) = list("FULL")

if(grouping == 0)
{
  group = data.frame(read.csv(paste(path_input,"UNIVERSE/","FULL.csv",sep="")))[,-1]
}

if (grouping == 1){
  
  group = data.frame(read.csv(paste(path_input,"UNIVERSE/","grouping1.csv",sep="")))[,-1]
  
}



if (grouping == 2) {
  
  group = data.frame(read.csv(paste(path_input,"UNIVERSE/","grouping2.csv",sep="")))[,-1]
  
}

nn = max(group[1,])


# FACTOR COMPUTATION ENDS, DON'T CHANGE THE CODE BELOW THIS

#######################################################################################################################################################
#XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX
#######################################################################################################################################################


fac_list1 = list_factor1
fac_list2 = list_factor2


ret_full_cmbn = data.frame(stck_lvl$Date[c(-1,-2)])
colnames(ret_full_cmbn)= "DATE"

ret_tb_cmbn = ret_full_cmbn
ret_full_cmbn_L = ret_full_cmbn
ret_full_cmbn_S = ret_full_cmbn
ret_tb_cmbn_L = ret_tb_cmbn
ret_tb_cmbn_S= ret_tb_cmbn
result_full_cmbn = NULL
result_tb_cmbn = NULL


for(u in 1: length(uni_list))
{
  stapp = uni_list[[u]]
  
  for(z in 1 : length(fac_list1))
  {
    
    fac_1<-fac_list1[[z]] # factor file input
    
    #stapp<-data.frame(read.csv(paste(uni_list[u,1],"/",uni_list[u,2],".csv",sep="")))
    
    
    f_factor<-data.frame(fac_1)                                 
    f_factor<-(f_factor[,-1])
    
    f_factor[stapp[,-1]==0]<-NA
    f_factor[f_factor<0.0000001 & f_factor>-0.0000001]<-NA      # This will be the final clculate foactor to be used for ranking
    
    fac1_rnk <- data.frame(t(apply(f_factor, 1, rank,na.last="keep",ties.method="max")))
    
    factor_rnk = data.frame(t(apply(f_factor, 1, rank,na.last="keep",ties.method="random")))
    
    fac_clas<-factor_rnk  # Rank weight of the final factor
    
    
    if(dsort ==1){
      
      #fac_2<-data.frame(read.csv(paste(path_input,"factor.csv",sep=""))) 
      #fac_list2 = list_factor2
      #fac_list2 = fac_list1
      
      for(zz in 1 : length(fac_list2))
      {
        
        fac_2<- fac_list2[[zz]] # factor file input
        
        f_factor2<-data.frame(fac_2)                                 
        f_factor2<-(f_factor2[,-1])
        
        f_factor2[stapp[,-1]==0]<-NA
        f_factor2[f_factor2<0.0000001 & f_factor2>-0.0000001]<-NA      # This will be the final clculate foactor to be used for ranking
        
        fac2_top<-f_factor2
        fac2_bot<-f_factor2
        
        topmax<-data.frame((apply(factor_rnk,1,max,na.rm=TRUE)))
        
        topminc = round((apply(factor_rnk, 1, quantile,probs=fac1_per ,na.rm =TRUE,ties.method="random")))
        
        botminc = round((apply(factor_rnk, 1, quantile,probs=1-fac1_per ,na.rm =TRUE,ties.method="random")))
        
        topminc_cbnd <- topminc
        
        for (j in 2:dim(factor_rnk)[2] ){
          topminc_cbnd<-cbind(topminc_cbnd,topminc)
          
        }
        #       
        
        
        botminc_cbnd <- botminc
        
        for (j in 2:dim(factor_rnk)[2] ){
          botminc_cbnd<-cbind(botminc_cbnd,botminc)
          
        }
        
        
        
        
        
        
        #botminc = 1/(2*(botminc))      
        
        
        #         
        #         
        #         wts_ini_top[rnk_top>=topminc_cbnd]<-topminc_cbnd1[rnk_top>=topminc_cbnd]
        #         wts_ini_top<-data.frame(wts_ini_top)
        #         
        #         
        #         wts_ini_bot[rnk_bot<=botminc_cbnd]<-botminc_cbnd1[rnk_bot<=botminc_cbnd]
        #         wts_ini_bot<-data.frame(wts_ini_bot)
        #         
        
        
        fac2_top[factor_rnk < topminc_cbnd]<- NA  
        
        fac2_rank_top<- data.frame(t(apply(fac2_top, 1, rank,na.last="keep",ties.method="random"))) 
        
        fac2_bot[factor_rnk > botminc_cbnd] <- NA  
        
        fac2_rank_bot<- data.frame(t(apply(fac2_bot, 1, rank,na.last="keep",ties.method="random")))
        
        
        fac2_rank_top2<-fac2_rank_top
        
        fac2_rank_bot2<-fac2_rank_bot 
        
        topmax2<-((apply(fac2_rank_top,1,max,na.rm=TRUE)))
        
        #topminc2 = round((apply(factor_rnk, 1, quantile,probs=fac2_per ,na.rm =TRUE,ties.method="random")))
        
        botminc2 = round((apply(factor_rnk, 1, quantile,probs=1-fac2_per ,na.rm =TRUE,ties.method="random")))
        
        
        fac2_rank_top2[fac2_rank_top < (topmax2 - botminc2+1)]<- NA
        
        #fac2_rank_top1[fac2_rank_top1 <(dim(fac2_rank_top1)[2]-rowSums(is.na(fac2_rank_top1)))- dnum2+1]<- NA
        
        
        fac2_rank_bot2[fac2_rank_bot>botminc2]<- NA
        
        
        topminc_cbnd2 <- (1/(2*(botminc2)))
        
        for (j in 2:dim(fac2_rank_top2)[2] ){
          topminc_cbnd2<-cbind(topminc_cbnd2,(1/(2*(botminc2))))
          
        }
        #       
        
        
        botminc_cbnd2 <- -(1/(2*(botminc2)))
        
        for (j in 2:dim(fac2_rank_bot2)[2] ){
          botminc_cbnd2<-cbind(botminc_cbnd2,-1/(2*(botminc2)))
          
        }
        
        
        botminc_cbnd2[is.na(botminc_cbnd2)] = 0
        #botminc_cbnd[is.na(botminc_cbnd)] = 0
        #topminc_cbnd[is.na(topminc_cbnd)] = 0
        topminc_cbnd2[is.na(topminc_cbnd2)] = 0
        fac2_rank_top2[is.na(fac2_rank_top2)] = 0
        fac2_rank_bot2[is.na(fac2_rank_bot2)] = 0         
        
        
        fac2_rank_top2[fac2_rank_top2>0]<-topminc_cbnd2[fac2_rank_top2>0]
        fac2_rank_top2<-data.frame(fac2_rank_top2)
        
        
        fac2_rank_bot2[fac2_rank_bot2>0]<-botminc_cbnd2[fac2_rank_bot2>0]
        fac2_rank_bot2<-data.frame(fac2_rank_bot2)
        
        # 
        #         fac2_rank_top2[is.na(fac2_rank_top2)] <-0   #Final double sorted equal weights
        #         fac2_rank_top2[fac2_rank_top2 >0]<-(1/(2*botmin2))
        #         fac2_rank_bot2[is.na(fac2_rank_bot2)] <-0
        #         fac2_rank_bot2[fac2_rank_bot2 >0]<-(-1/(2*botmin2))
        
        
        colnames(fac2_rank_top2)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
        colnames(fac2_rank_bot2)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
        
        
        if(liq_weight ==1)
        {
          turnover = data.frame(read.csv(paste(path_input,"CASH DB/","turnover.csv",sep="")))
          
          turnover_m = turnover[,-1]
          
          turnover_m = (rollapply(turnover[,-1],66,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE))
          
          turnover_m_top = turnover_m
          turnover_m_bot = turnover_m
          
          turnover_m_top1 = turnover_m_top
          turnover_m_bot1 = turnover_m_bot
          
          turnover_m_top[is.na(turnover_m_top)] = 0
          turnover_m_bot[is.na(turnover_m_bot)] = 0
          
          turnover_m_top[fac2_rank_top2 <= 0] = 0
          
          turnover_m_bot[fac2_rank_bot2 >= 0] = 0
          
          
          turnover_m_top1[fac2_rank_top2 <= 0] = NA
          
          turnover_m_bot1[fac2_rank_bot2 >= 0] = NA
          
          turnover_top_cap  = fac2_rank_top2
          turnover_top_cap[,] = NA
          turnover_bot_cap = turnover_top_cap
          #turnover_top_cap = as.matrix(turnover_top_cap)
          
          
          turnover_top_cap1 = round((apply(turnover_m_top1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          
          turnover_bot_cap1 = round((apply(turnover_m_bot1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          
          
          #           
          #           x1= data.frame(turnover_top_cap)
          #           x2= data.frame(turnover_bot_cap)
          #           #x3 = data.frame(round((apply(turnover_m_top1, 1, quantile,probs=0.9 ,na.rm =TRUE,ties.method="random"))))
          #           
          turnover_top_cap1[is.na(turnover_top_cap1)]  = 0
          turnover_bot_cap1[is.na(turnover_bot_cap1)]  = 0
          
          turnover_top_cap[,1:ncol(turnover_top_cap)] = (turnover_top_cap1) 
          turnover_bot_cap[,1:ncol(turnover_bot_cap)] = (turnover_bot_cap1)
          
          
          #turnover_m_top[fac2_rank_top2 <= 0] = NA
          #x = turnover_m_top1
          #turnover_m_top1[turnover_m_top[which(!is.na(turnover_m_top))] > turnover_top_cap[which(!is.na(turnover_top_cap))]] = turnover_top_cap
          
          turnover_m_top1[turnover_m_top > turnover_top_cap] = turnover_top_cap[turnover_m_top > turnover_top_cap]
          turnover_m_bot1[turnover_m_bot > turnover_bot_cap] = turnover_bot_cap[turnover_m_bot > turnover_bot_cap]
          
          
          
          #turnover_m_top1 = data.frame(turnover_m_top1)
          
          
          fac2_rank_top2 = turnover_m_top1/(2*rowSums(abs(turnover_m_top1),na.rm=TRUE))
          fac2_rank_bot2 = turnover_m_bot1/(2*rowSums(abs(turnover_m_bot1),na.rm=TRUE))
          
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
          
          
          for(iii in 1:n-1)
          {
            
            final_weight  = STOCK_CAP(fac2_rank_top2,fac2_rank_bot2)
            
            fac2_rank_top2  =(final_weight[[1]])
            fac2_rank_bot2  =(final_weight[[2]])
          }
          
          
          fac2_rank_top2<-data.frame(fac2_rank_top2)
          fac2_rank_bot2<-data.frame((-1)*fac2_rank_bot2)
          
          
        }
        
        if(liq_weight ==2)
        {
          
          turnover = data.frame(read.csv(paste(path_input,"CASH DB/","turnover.csv",sep="")))
          
          turnover_m = turnover[,-1]
          
          turnover_m = (rollapply(turnover[,-1],66,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE))
          
          turnover_m_top = turnover_m
          turnover_m_bot = turnover_m
          
          turnover_m_top1 = turnover_m_top
          turnover_m_bot1 = turnover_m_bot
          
          turnover_m_top[is.na(turnover_m_top)] = 0
          turnover_m_bot[is.na(turnover_m_bot)] = 0
          
          turnover_m_top[fac2_rank_top2 <= 0] = 0
          
          turnover_m_bot[fac2_rank_bot2 >= 0] = 0
          
          
          turnover_m_top1[fac2_rank_top2 <= 0] = NA
          
          turnover_m_bot1[fac2_rank_bot2 >= 0] = NA
          # 
          # turnover_top_cap  = fac2_rank_top2
          # turnover_top_cap[,] = NA
          # turnover_bot_cap = turnover_top_cap
          #turnover_top_cap = as.matrix(turnover_top_cap)
          
          
          # turnover_top_cap1 = round((apply(turnover_m_top1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          # 
          # turnover_bot_cap1 = round((apply(turnover_m_bot1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          # 
          # 
          # #           
          # #           x1= data.frame(turnover_top_cap)
          # #           x2= data.frame(turnover_bot_cap)
          # #           #x3 = data.frame(round((apply(turnover_m_top1, 1, quantile,probs=0.9 ,na.rm =TRUE,ties.method="random"))))
          # #           
          # turnover_top_cap1[is.na(turnover_top_cap1)]  = 0
          # turnover_bot_cap1[is.na(turnover_bot_cap1)]  = 0
          # 
          # turnover_top_cap[,1:ncol(turnover_top_cap)] = (turnover_top_cap1) 
          # turnover_bot_cap[,1:ncol(turnover_bot_cap)] = (turnover_bot_cap1)
          # 
          # 
          # #turnover_m_top[fac2_rank_top2 <= 0] = NA
          # #x = turnover_m_top1
          # #turnover_m_top1[turnover_m_top[which(!is.na(turnover_m_top))] > turnover_top_cap[which(!is.na(turnover_top_cap))]] = turnover_top_cap
          # 
          # turnover_m_top1[turnover_m_top > turnover_top_cap] = turnover_top_cap[turnover_m_top > turnover_top_cap]
          # turnover_m_bot1[turnover_m_bot > turnover_bot_cap] = turnover_bot_cap[turnover_m_bot > turnover_bot_cap]
          # 
          # 
          
          #turnover_m_top1 = data.frame(turnover_m_top1)
          
          wts_ini_top = data.frame(t(apply(turnover_m_top1, 1, rank,na.last="keep",ties.method="random")))
          wts_ini_bot = data.frame(t(apply(turnover_m_bot1, 1, rank,na.last="keep",ties.method="random")))
          
          # wts_ini_top = wts_ini_top1 + wts_ini_top2 
          # wts_ini_bot = wts_ini_bot1 + wts_ini_bot2
          # 
          
          fac2_rank_top2<-wts_ini_top/(2*rowSums(abs(wts_ini_top),na.rm=TRUE))
          fac2_rank_bot2<-((-1)*wts_ini_bot)/(2*rowSums(abs(wts_ini_bot),na.rm=TRUE))
          
          
          fac2_rank_top2[is.na(fac2_rank_top2)]<-0
          fac2_rank_bot2[is.na(fac2_rank_bot2)]<-0
          
          fac2_rank_top2<-data.frame(fac2_rank_top2)
          fac2_rank_bot2<-data.frame(fac2_rank_bot2)
          
          
        }
        
        
        
        #       
        #       
        #       topname<-matrix(0,dim(fac2_rank_top2)[1],dnum2)
        #       botname<-matrix(0,dim(fac2_rank_bot2)[1],dnum2)
        #       
        #       
        #       
        #       
        #       for (k in 1:dim(fac2_rank_top2)[1]){
        #         
        #         if((sum(fac2_rank_top2[k,]>0) ==dnum2) && (sum(fac2_rank_bot2[k,]<0) ==dnum2))
        #         {
        #         
        #         topname[k,]<-colnames(fac2_rank_top2[k,fac2_rank_top2[k,]>0])
        #         botname[k,]<-colnames(fac2_rank_bot2[k,fac2_rank_bot2[k,]<0])
        #         
        #         }
        #         
        #         else
        #         {
        #           fac2_rank_top2[k,] =0
        #           fac2_rank_bot2[k,] =0
        #         }
        #       }
        #       
        
        wts_tb<-(fac2_rank_top2 + fac2_rank_bot2)
        
        wts_tb<-data.frame(wts_tb)
        
        wts_ini_top<-wts_tb
        wts_ini_bot<-wts_tb
        
        wts_ini_top[wts_tb<=0]<-0
        wts_ini_bot[wts_tb>=0]<-0
        
        
        wts_ini_full_tb <- (wts_tb)
        
        ###################################################################################################################################################################
        
        wts_ini_top_calc<-wts_ini_top[c(-1*dim(wts_ini_top)[1],-1*dim(wts_ini_top)[1]+1),]                        #return calculation
        wts_ini_bot_calc<-wts_ini_bot[c(-1*dim(wts_ini_bot)[1],-1*dim(wts_ini_bot)[1]+1),]
        
        stck_lvl_calc<-stck_lvl[c(-1,-2),-1]
        
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
        
        TV_tb<-data.frame(rowSums(abs(wts_ini_full_tb[-1,]-wts_ini_full_tb[-1*dim(wts_ini_full_tb)[1],])))[-1,]
        
        #result_tb = data.frame(cbind(t(ann_rslt(data.frame(stck_lvl$Date[c(-1,-2)],tot_ret_tb,TV_tb))),paste(names(fac_list1)[z],"_",names(uni_list)[u])))
        
        tot_ret_tb<-data.frame((lng_ret_tb+sht_ret_tb) - cst_tb)
        
        lng_ret_tb<-data.frame(lng_ret_tb-cst_tb_L)
        sht_ret_tb<-data.frame(sht_ret_tb-cst_tb_S)
        
        ret_fin_tb<-data.frame(cbind(stck_lvl$Date[c(-1,-2)],tot_ret_tb,lng_ret_tb,sht_ret_tb,wts_ini_full_tb[c(-1,-1*dim(wts_ini_full_tb)[1]),]))
        
        tot_ret_tb[is.na(tot_ret_tb)]<-0
        TV_tb[is.na(TV_tb)]<-0
        
        result_tb = data.frame(cbind(t(ann_rslt(data.frame(stck_lvl$Date[c(-1,-2)],tot_ret_tb,TV_tb))),paste(names(fac_list1)[z],"_",names(fac_list2)[zz],"_",names(uni_list)[u])))
        #result_full = data.frame(cbind(t(ann_rslt(data.frame(stck_lvl$Date[c(-1,-2)],tot_ret_full,TV_full))),paste(names(fac_list1)[z],"_",names(uni_list)[u])))
        colnames(result_tb)[dim(result_tb)[2]] = "FACTOR"
        
        write.csv(result_tb,paste(path_output,names(fac_list1)[z],"_",names(fac_list2)[zz],"_",names(uni_list)[u],"_TB_Results.csv"),row.names=FALSE)
        
        write.csv(ret_fin_tb,paste(path_output,names(fac_list1)[z],"_",names(fac_list2)[zz],"_",names(uni_list)[u],"_TB_PORTFOLIO.csv"),row.names=FALSE)
        
        ret_tb_cmbn = data.frame(cbind(ret_tb_cmbn,tot_ret_tb))
        colnames(ret_tb_cmbn)[((u-1)*(length(fac_list1)*length(fac_list2))) +(z-1)*length(fac_list2)+1+zz]= paste(names(fac_list1)[z],"_",names(fac_list2)[zz],"_",names(uni_list)[u])
        
        result_tb_cmbn = data.frame(rbind(result_tb_cmbn,result_tb))
        
        ret_tb_cmbn_L = data.frame(cbind(ret_tb_cmbn_L,lng_ret_tb))
        colnames(ret_tb_cmbn_L)[((u-1)*(length(fac_list1)*length(fac_list2))) +(z-1)*length(fac_list2)+1+zz]= paste(names(fac_list1)[z],"_",names(fac_list2)[zz],"_",names(uni_list)[u])
        
        ret_tb_cmbn_S = data.frame(cbind(ret_tb_cmbn_S, sht_ret_tb))
        colnames(ret_tb_cmbn_S)[((u-1)*(length(fac_list1)*length(fac_list2))) +(z-1)*length(fac_list2)+1+zz]= paste(names(fac_list1)[z],"_",names(fac_list2)[zz],"_",names(uni_list)[u])
        
      }
      
      
    }
    
    
    if(dsort!=1){
      
      wts_fin<-NULL
      
      for (i in 1:nn){
        
        fac_clas_sel_1<-as.matrix(fac_clas[,group[1,]==i])
        fac_clas_sel_2 = fac_clas_sel_1
        
        if (sum(group[1,]==i)<=1)
        {
          fac_clas_sel_2[,] = 0
          colnames(fac_clas_sel_2) = colnames(fac_clas)[which(group[1,]==i)]
        }
        
        if (sum(group[1,]==i)>1)
        {
          
          fac_clas_sel_2<-as.matrix((fac_clas_sel_1-rowMeans(fac_clas_sel_1,na.rm=TRUE)))
          
          #fac_clas_sel_2<-as.matrix((fac_clas_sel_1-rowMeans(fac_clas_sel_1,na.rm=TRUE))/rowSums(abs(fac_clas_sel_1-rowMeans(fac_clas_sel_1,na.rm=TRUE)),na.rm=TRUE))
          
          
        }
        wts_ini<-data.frame(fac_clas_sel_2)
        
        wts_tb<-wts_ini
        wts_ini_top<-matrix(0,dim(wts_ini)[1],dim(wts_ini)[2])
        wts_ini_bot<-matrix(0,dim(wts_ini)[1],dim(wts_ini)[2])
        
        
        
        if( i==1)
        {
          
          wts_fin<-wts_ini
        }
        
        else{
          
          wts_fin<-cbind(wts_fin,wts_ini)
          
        }
        
      }
      
      
      if (topbot==1){  # This will select the top x and bottom x stocks and equaly weigh the same
        
        
        rnk_top<-factor_rnk 
        #rnk_top<-fac1_rnk
        
        rnk_bot<-factor_rnk 
        #rnk_bot<-fac1_rnk
        
        ############ The below will calculate the top and bottom ranks
        
        topmax<-data.frame((apply(rnk_top,1,max,na.rm=TRUE)))
        
        topminc = round((apply(rnk_top, 1, quantile,probs=ptop ,na.rm =TRUE,ties.method="random")))
        
        botminc = round((apply(rnk_top, 1, quantile,probs=pbot ,na.rm =TRUE,ties.method="random")))
        #topminc[topminc<=xnum]<-xnum     # This is to make sure that the total number is not less than selected numbers
        
        topminc_cbnd <- topminc
        
        for (j in 2:dim(wts_ini)[2] ){
          topminc_cbnd<-cbind(topminc_cbnd,topminc)
          
        }
        #       
        
        
        botminc_cbnd <- botminc
        
        for (j in 2:dim(wts_ini)[2] ){
          botminc_cbnd<-cbind(botminc_cbnd,botminc)
          
        }
        
        
        topminc_cbnd1 <- (1/(2*(topmax - topminc+1)))
        
        for (j in 2:dim(wts_ini)[2] ){
          topminc_cbnd1<-cbind(topminc_cbnd1,(1/(2*(topmax - topminc+1))))
          
        }
        #       
        
        
        botminc_cbnd1 <- -1/(2*(botminc))
        
        for (j in 2:dim(wts_ini)[2] ){
          botminc_cbnd1<-cbind(botminc_cbnd1,-1/(2*(botminc)))
          
        }
        
        
        
        
        #botminc = 1/(2*(botminc))      
        
        botminc_cbnd1[is.na(botminc_cbnd1)] = 0
        botminc_cbnd[is.na(botminc_cbnd)] = 0
        topminc_cbnd[is.na(topminc_cbnd)] = 0
        topminc_cbnd1[is.na(topminc_cbnd1)] = 0
        rnk_top[is.na(rnk_top)] = -999999999
        rnk_bot[is.na(rnk_bot)] = 999999999         
        
        
        #topminc[is.na(topminc)] = 0
        
        
        
        
        wts_ini_top[rnk_top>=topminc_cbnd]<-topminc_cbnd1[rnk_top>=topminc_cbnd]
        wts_ini_top<-data.frame(wts_ini_top)
        
        
        wts_ini_bot[rnk_bot<=botminc_cbnd]<-botminc_cbnd1[rnk_bot<=botminc_cbnd]
        wts_ini_bot<-data.frame(wts_ini_bot)
        
        
        #wts_ini_bot[rnk_bot<=botminc_cbnd]<-(-1/(2*xnum))
        
        #wts_ini_bot<-data.frame(wts_ini_bot)
        
        wts_ini_top[is.na(wts_ini_top)]<-0
        wts_ini_bot[is.na(wts_ini_bot)]<-0
        
        wts_ini_top<-data.frame(wts_ini_top)
        wts_ini_bot<-data.frame(wts_ini_bot)
        
        
        #         
        #         topname<-matrix(0,dim(wts_ini_top)[1],xnum)
        #         botname<-matrix(0,dim(wts_ini_bot)[1],xnum)
        #         
        
        #       
        #       
        #       for (k in 1:dim(wts_ini_bot)[1] ){
        #         
        #         if((sum(wts_ini_top[k,]>0) ==xnum) && (sum(wts_ini_bot[k,]<0) ==xnum))
        #           
        #         {
        #           topname[k,]<-colnames(wts_ini_top[k,wts_ini_top[k,]>0])
        #           botname[k,]<-colnames(wts_ini_bot[k,wts_ini_bot[k,]<0])
        #         }
        #         else
        #         {
        #           wts_ini_top[k,]<-0
        #           wts_ini_bot[k,]<-0
        #         }
        #         
        #       }
        
        fac2_rank_top2 = wts_ini_top           
        fac2_rank_bot2 = wts_ini_bot
        
        if(liq_weight ==1)
        {
          turnover = data.frame(read.csv(paste(path_input,"CASH DB/","turnover.csv",sep="")))
          
          turnover_m = turnover[,-1]
          
          turnover_m = (rollapply(turnover[,-1],66,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE))
          
          turnover_m_top = turnover_m
          turnover_m_bot = turnover_m
          
          turnover_m_top1 = turnover_m_top
          turnover_m_bot1 = turnover_m_bot
          
          turnover_m_top[is.na(turnover_m_top)] = 0
          turnover_m_bot[is.na(turnover_m_bot)] = 0
          
          turnover_m_top[fac2_rank_top2 <= 0] = 0
          
          turnover_m_bot[fac2_rank_bot2 >= 0] = 0
          
          
          turnover_m_top1[fac2_rank_top2 <= 0] = NA
          
          turnover_m_bot1[fac2_rank_bot2 >= 0] = NA
          
          turnover_top_cap  = fac2_rank_top2
          turnover_top_cap[,] = NA
          turnover_bot_cap = turnover_top_cap
          #turnover_top_cap = as.matrix(turnover_top_cap)
          
          
          turnover_top_cap1 = round((apply(turnover_m_top1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          
          turnover_bot_cap1 = round((apply(turnover_m_bot1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          
          
          #           
          #           x1= data.frame(turnover_top_cap)
          #           x2= data.frame(turnover_bot_cap)
          #           #x3 = data.frame(round((apply(turnover_m_top1, 1, quantile,probs=0.9 ,na.rm =TRUE,ties.method="random"))))
          #           
          turnover_top_cap1[is.na(turnover_top_cap1)]  = 0
          turnover_bot_cap1[is.na(turnover_bot_cap1)]  = 0
          
          turnover_top_cap[,1:ncol(turnover_top_cap)] = (turnover_top_cap1) 
          turnover_bot_cap[,1:ncol(turnover_bot_cap)] = (turnover_bot_cap1)
          
          
          #turnover_m_top[fac2_rank_top2 <= 0] = NA
          #x = turnover_m_top1
          #turnover_m_top1[turnover_m_top[which(!is.na(turnover_m_top))] > turnover_top_cap[which(!is.na(turnover_top_cap))]] = turnover_top_cap
          
          turnover_m_top1[turnover_m_top > turnover_top_cap] = turnover_top_cap[turnover_m_top > turnover_top_cap]
          turnover_m_bot1[turnover_m_bot > turnover_bot_cap] = turnover_bot_cap[turnover_m_bot > turnover_bot_cap]
          
          
          
          #turnover_m_top1 = data.frame(turnover_m_top1)
          
          
          fac2_rank_top2 = turnover_m_top1/(2*rowSums(abs(turnover_m_top1),na.rm=TRUE))
          fac2_rank_bot2 = turnover_m_bot1/(2*rowSums(abs(turnover_m_bot1),na.rm=TRUE))
          
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
          
          
          
          for(iii in 1:n-1)
          {
            
            final_weight  = STOCK_CAP(fac2_rank_top2,fac2_rank_bot2)
            
            fac2_rank_top2  =(final_weight[[1]])
            fac2_rank_bot2  =(final_weight[[2]])
          }
          
          wts_ini_top  = data.frame(fac2_rank_top2)            
          wts_ini_bot = data.frame((-1)*fac2_rank_bot2)
          
          
          
        }
        
        if(liq_weight ==2)
        {
          
          turnover = data.frame(read.csv(paste(path_input,"CASH DB/","turnover.csv",sep="")))
          
          turnover_m = turnover[,-1]
          
          turnover_m = (rollapply(turnover[,-1],66,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE))
          
          turnover_m_top = turnover_m
          turnover_m_bot = turnover_m
          
          turnover_m_top1 = turnover_m_top
          turnover_m_bot1 = turnover_m_bot
          
          turnover_m_top[is.na(turnover_m_top)] = 0
          turnover_m_bot[is.na(turnover_m_bot)] = 0
          
          turnover_m_top[fac2_rank_top2 <= 0] = 0
          
          turnover_m_bot[fac2_rank_bot2 >= 0] = 0
          
          
          turnover_m_top1[fac2_rank_top2 <= 0] = NA
          
          turnover_m_bot1[fac2_rank_bot2 >= 0] = NA
          # 
          # turnover_top_cap  = fac2_rank_top2
          # turnover_top_cap[,] = NA
          # turnover_bot_cap = turnover_top_cap
          #turnover_top_cap = as.matrix(turnover_top_cap)
          
          
          # turnover_top_cap1 = round((apply(turnover_m_top1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          # 
          # turnover_bot_cap1 = round((apply(turnover_m_bot1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          # 
          # 
          # #           
          # #           x1= data.frame(turnover_top_cap)
          # #           x2= data.frame(turnover_bot_cap)
          # #           #x3 = data.frame(round((apply(turnover_m_top1, 1, quantile,probs=0.9 ,na.rm =TRUE,ties.method="random"))))
          # #           
          # turnover_top_cap1[is.na(turnover_top_cap1)]  = 0
          # turnover_bot_cap1[is.na(turnover_bot_cap1)]  = 0
          # 
          # turnover_top_cap[,1:ncol(turnover_top_cap)] = (turnover_top_cap1) 
          # turnover_bot_cap[,1:ncol(turnover_bot_cap)] = (turnover_bot_cap1)
          # 
          # 
          # #turnover_m_top[fac2_rank_top2 <= 0] = NA
          # #x = turnover_m_top1
          # #turnover_m_top1[turnover_m_top[which(!is.na(turnover_m_top))] > turnover_top_cap[which(!is.na(turnover_top_cap))]] = turnover_top_cap
          # 
          # turnover_m_top1[turnover_m_top > turnover_top_cap] = turnover_top_cap[turnover_m_top > turnover_top_cap]
          # turnover_m_bot1[turnover_m_bot > turnover_bot_cap] = turnover_bot_cap[turnover_m_bot > turnover_bot_cap]
          # 
          # 
          
          #turnover_m_top1 = data.frame(turnover_m_top1)
          
          wts_ini_top = data.frame(t(apply(turnover_m_top1, 1, rank,na.last="keep",ties.method="random")))
          wts_ini_bot = data.frame(t(apply(turnover_m_bot1, 1, rank,na.last="keep",ties.method="random")))
          
          # wts_ini_top = wts_ini_top1 + wts_ini_top2 
          # wts_ini_bot = wts_ini_bot1 + wts_ini_bot2
          # 
          
          wts_ini_top<-wts_ini_top/(2*rowSums(abs(wts_ini_top),na.rm=TRUE))
          wts_ini_bot<-((-1)*wts_ini_bot)/(2*rowSums(abs(wts_ini_bot),na.rm=TRUE))
          
          
          wts_ini_top[is.na(wts_ini_top)]<-0
          wts_ini_bot[is.na(wts_ini_bot)]<-0
          
          wts_ini_top<-data.frame(wts_ini_top)
          wts_ini_bot<-data.frame(wts_ini_bot)
          
          
        }
        
        wts_tb =  wts_ini_top + wts_ini_bot
        
        
        #         wts_ini_top[wts_tb<=0]<-0
        #         wts_ini_bot[wts_tb>=0]<-0
        #         
        
        colnames(wts_ini_bot)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
        colnames(wts_ini_top)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
        
        
        
        
      }
      ####################################################################################################################################################
      
      
      if (topbot==2){  # This will select the top x and bottom x stocks and rank weigh the same
        
        fac_clas_sel_1_tb<-as.matrix(factor_rnk)
        
        fac_clas_sel_2_tb<-(fac_clas_sel_1_tb-rowMeans(fac_clas_sel_1_tb,na.rm=TRUE))/rowSums(abs(fac_clas_sel_1_tb-rowMeans(fac_clas_sel_1_tb,na.rm=TRUE)),na.rm=TRUE)
        
        wts_ini_tb<-data.frame(fac_clas_sel_2_tb)
        
        wts_tb<-wts_ini_tb
        
        
        rnk_top<-factor_rnk
        #rnk_top<-fac1_rnk
        
        rnk_bot<-factor_rnk 
        #rnk_bot<-fac1_rnk
        
        ############ The below will calculate the top and bottom ranks
        
        topmax<-data.frame((apply(rnk_top,1,max,na.rm=TRUE)))
        ############ The below will calculate the top and bottom ranks
        topminc = round((apply(rnk_top, 1, quantile,probs=ptop ,na.rm =TRUE,ties.method="random")))
        
        botminc = round((apply(rnk_top, 1, quantile,probs=pbot ,na.rm =TRUE,ties.method="random")))
        #topminc<-data.frame((apply(rnk_top,1,max,na.rm=TRUE)))
        #topminc[topminc<=xnum]<-xnum     # This is to make sure that the total number is not less than selected numbers
        
        topminc_cbnd <- topminc
        
        for (j in 2:dim(wts_ini)[2] ){
          topminc_cbnd<-cbind(topminc_cbnd,topminc)
          
        }
        
        
        botminc_cbnd <- botminc
        
        for (j in 2:dim(wts_ini)[2] ){
          botminc_cbnd<-cbind(botminc_cbnd,botminc)
          
        }
        
        wts_ini_top<-wts_tb
        wts_ini_bot<-wts_tb
        
        wts_ini_top[rnk_top < topminc_cbnd] <-NA
        wts_ini_bot[rnk_bot > botminc_cbnd] = NA
        
        wts_ini_top = data.frame(t(apply(wts_ini_top, 1, rank,na.last="keep",ties.method="random")))
        wts_ini_bot = data.frame(t(apply((-1)*wts_ini_bot, 1, rank,na.last="keep",ties.method="random")))
        
        
        wts_ini_top<-wts_ini_top/(2*rowSums(abs(wts_ini_top),na.rm=TRUE))
        wts_ini_bot<-((-1)*wts_ini_bot)/(2*rowSums(abs(wts_ini_bot),na.rm=TRUE))
        
        #         
        #         wts_tb[rnk_top < topminc_cbnd & rnk_top >botminc_cbnd] <-NA
        #         wts_tb<-wts_tb/rowSums(abs(wts_tb),na.rm=TRUE)
        #       
        
        wts_ini_top[is.na(wts_ini_top)]<-0
        wts_ini_bot[is.na(wts_ini_bot)]<-0
        
        #         wts_ini_top<-data.frame(wts_ini_top)
        #         wts_ini_bot<-data.frame(wts_ini_bot)
        
        fac2_rank_top2 = wts_ini_top           
        fac2_rank_bot2 = wts_ini_bot
        
        if(liq_weight ==1)
        {
          turnover = data.frame(read.csv(paste(path_input,"CASH DB/","turnover.csv",sep="")))
          
          turnover_m = turnover[,-1]
          
          turnover_m = (rollapply(turnover[,-1],66,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE))
          
          turnover_m_top = turnover_m
          turnover_m_bot = turnover_m
          
          turnover_m_top1 = turnover_m_top
          turnover_m_bot1 = turnover_m_bot
          
          turnover_m_top[is.na(turnover_m_top)] = 0
          turnover_m_bot[is.na(turnover_m_bot)] = 0
          
          turnover_m_top[fac2_rank_top2 <= 0] = 0
          
          turnover_m_bot[fac2_rank_bot2 >= 0] = 0
          
          
          turnover_m_top1[fac2_rank_top2 <= 0] = NA
          
          turnover_m_bot1[fac2_rank_bot2 >= 0] = NA
          
          turnover_top_cap  = fac2_rank_top2
          turnover_top_cap[,] = NA
          turnover_bot_cap = turnover_top_cap
          #turnover_top_cap = as.matrix(turnover_top_cap)
          
          
          turnover_top_cap1 = round((apply(turnover_m_top1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          
          turnover_bot_cap1 = round((apply(turnover_m_bot1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          
          
          #           
          #           x1= data.frame(turnover_top_cap)
          #           x2= data.frame(turnover_bot_cap)
          #           #x3 = data.frame(round((apply(turnover_m_top1, 1, quantile,probs=0.9 ,na.rm =TRUE,ties.method="random"))))
          #           
          turnover_top_cap1[is.na(turnover_top_cap1)]  = 0
          turnover_bot_cap1[is.na(turnover_bot_cap1)]  = 0
          
          turnover_top_cap[,1:ncol(turnover_top_cap)] = (turnover_top_cap1) 
          turnover_bot_cap[,1:ncol(turnover_bot_cap)] = (turnover_bot_cap1)
          
          
          #turnover_m_top[fac2_rank_top2 <= 0] = NA
          #x = turnover_m_top1
          #turnover_m_top1[turnover_m_top[which(!is.na(turnover_m_top))] > turnover_top_cap[which(!is.na(turnover_top_cap))]] = turnover_top_cap
          
          turnover_m_top1[turnover_m_top > turnover_top_cap] = turnover_top_cap[turnover_m_top > turnover_top_cap]
          turnover_m_bot1[turnover_m_bot > turnover_bot_cap] = turnover_bot_cap[turnover_m_bot > turnover_bot_cap]
          
          
          
          #turnover_m_top1 = data.frame(turnover_m_top1)
          
          
          fac2_rank_top2 = turnover_m_top1/(2*rowSums(abs(turnover_m_top1),na.rm=TRUE))
          fac2_rank_bot2 = turnover_m_bot1/(2*rowSums(abs(turnover_m_bot1),na.rm=TRUE))
          
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
          
          
          
          for(iii in 1:n-1)
          {
            
            final_weight  = STOCK_CAP(fac2_rank_top2,fac2_rank_bot2)
            
            fac2_rank_top2  =(final_weight[[1]])
            fac2_rank_bot2  =(final_weight[[2]])
          }
          
          
          wts_ini_top  = data.frame(fac2_rank_top2)            
          wts_ini_bot = data.frame((-1)*fac2_rank_bot2)
          
          
        }
        
        if(liq_weight ==2)
        {
          
          turnover = data.frame(read.csv(paste(path_input,"CASH DB/","turnover.csv",sep="")))
          
          turnover_m = turnover[,-1]
          
          turnover_m = (rollapply(turnover[,-1],66,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE))
          
          turnover_m_top = turnover_m
          turnover_m_bot = turnover_m
          
          turnover_m_top1 = turnover_m_top
          turnover_m_bot1 = turnover_m_bot
          
          turnover_m_top[is.na(turnover_m_top)] = 0
          turnover_m_bot[is.na(turnover_m_bot)] = 0
          
          turnover_m_top[fac2_rank_top2 <= 0] = 0
          
          turnover_m_bot[fac2_rank_bot2 >= 0] = 0
          
          
          turnover_m_top1[fac2_rank_top2 <= 0] = NA
          
          turnover_m_bot1[fac2_rank_bot2 >= 0] = NA
          # 
          # turnover_top_cap  = fac2_rank_top2
          # turnover_top_cap[,] = NA
          # turnover_bot_cap = turnover_top_cap
          #turnover_top_cap = as.matrix(turnover_top_cap)
          
          
          # turnover_top_cap1 = round((apply(turnover_m_top1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          # 
          # turnover_bot_cap1 = round((apply(turnover_m_bot1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          # 
          # 
          # #           
          # #           x1= data.frame(turnover_top_cap)
          # #           x2= data.frame(turnover_bot_cap)
          # #           #x3 = data.frame(round((apply(turnover_m_top1, 1, quantile,probs=0.9 ,na.rm =TRUE,ties.method="random"))))
          # #           
          # turnover_top_cap1[is.na(turnover_top_cap1)]  = 0
          # turnover_bot_cap1[is.na(turnover_bot_cap1)]  = 0
          # 
          # turnover_top_cap[,1:ncol(turnover_top_cap)] = (turnover_top_cap1) 
          # turnover_bot_cap[,1:ncol(turnover_bot_cap)] = (turnover_bot_cap1)
          # 
          # 
          # #turnover_m_top[fac2_rank_top2 <= 0] = NA
          # #x = turnover_m_top1
          # #turnover_m_top1[turnover_m_top[which(!is.na(turnover_m_top))] > turnover_top_cap[which(!is.na(turnover_top_cap))]] = turnover_top_cap
          # 
          # turnover_m_top1[turnover_m_top > turnover_top_cap] = turnover_top_cap[turnover_m_top > turnover_top_cap]
          # turnover_m_bot1[turnover_m_bot > turnover_bot_cap] = turnover_bot_cap[turnover_m_bot > turnover_bot_cap]
          # 
          # 
          
          #turnover_m_top1 = data.frame(turnover_m_top1)
          
          wts_ini_top = data.frame(t(apply(turnover_m_top1, 1, rank,na.last="keep",ties.method="random")))
          wts_ini_bot = data.frame(t(apply(turnover_m_bot1, 1, rank,na.last="keep",ties.method="random")))
          
          # wts_ini_top = wts_ini_top1 + wts_ini_top2 
          # wts_ini_bot = wts_ini_bot1 + wts_ini_bot2
          # 
          
          wts_ini_top<-wts_ini_top/(2*rowSums(abs(wts_ini_top),na.rm=TRUE))
          wts_ini_bot<-((-1)*wts_ini_bot)/(2*rowSums(abs(wts_ini_bot),na.rm=TRUE))
          
          
          wts_ini_top[is.na(wts_ini_top)]<-0
          wts_ini_bot[is.na(wts_ini_bot)]<-0
          
          wts_ini_top<-data.frame(wts_ini_top)
          wts_ini_bot<-data.frame(wts_ini_bot)
          
          
        }
        
        
        
        wts_tb =  wts_ini_top + wts_ini_bot
        
        
        #         wts_ini_top[wts_tb<=0]<-0
        #         wts_ini_bot[wts_tb>=0]<-0
        #         
        
        colnames(wts_ini_bot)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
        colnames(wts_ini_top)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
        
        
      }
      
      ####################################################################################################################################################
      
      
      if (topbot==3){  # This will select the top x and bottom x stocks and equaly weigh the same
        
        fac_clas_sel_1_tb<-as.matrix(factor_rnk)
        
        fac_clas_sel_2_tb<-(fac_clas_sel_1_tb-rowMeans(fac_clas_sel_1_tb,na.rm=TRUE))/rowSums(abs(fac_clas_sel_1_tb-rowMeans(fac_clas_sel_1_tb,na.rm=TRUE)),na.rm=TRUE)
        
        wts_ini_tb<-data.frame(fac_clas_sel_2_tb)
        
        wts_tb<-wts_ini_tb
        
        rnk_top<-factor_rnk
        #rnk_top<-fac1_rnk
        
        rnk_bot<-factor_rnk 
        #rnk_bot<-fac1_rnk
        
        ############ The below will calculate the top and bottom ranks
        
        topmax<-data.frame((apply(rnk_top,1,max,na.rm=TRUE)))
        ############ The below will calculate the top and bottom ranks
        topminc = round((apply(rnk_top, 1, quantile,probs=ptop ,na.rm =TRUE,ties.method="random")))
        
        botminc = round((apply(rnk_top, 1, quantile,probs=pbot ,na.rm =TRUE,ties.method="random")))
        #topminc<-data.frame((apply(rnk_top,1,max,na.rm=TRUE)))
        #topminc[topminc<=xnum]<-xnum     # This is to make sure that the total number is not less than selected numbers
        
        topminc_cbnd <- topminc
        
        for (j in 2:dim(wts_ini)[2] ){
          topminc_cbnd<-cbind(topminc_cbnd,topminc)
          
        }
        
        
        botminc_cbnd <- botminc
        
        for (j in 2:dim(wts_ini)[2] ){
          botminc_cbnd<-cbind(botminc_cbnd,botminc)
          
        }
        
        wts_ini_top<-wts_tb
        wts_ini_bot<-wts_tb
        
        wts_ini_top[rnk_top < topminc_cbnd] <-NA
        wts_ini_bot[rnk_bot > botminc_cbnd] = NA
        
        wts_ini_top<-wts_ini_top/(2*rowSums(abs(wts_ini_top),na.rm=TRUE))
        wts_ini_bot<-wts_ini_bot/(2*rowSums(abs(wts_ini_bot),na.rm=TRUE))
        
        #         
        #         wts_tb[rnk_top < topminc_cbnd & rnk_top >botminc_cbnd] <-NA
        #         wts_tb<-wts_tb/rowSums(abs(wts_tb),na.rm=TRUE)
        #       
        
        wts_ini_top[is.na(wts_ini_top)]<-0
        wts_ini_bot[is.na(wts_ini_bot)]<-0
        
        #         wts_ini_top<-data.frame(wts_ini_top)
        #         wts_ini_bot<-data.frame(wts_ini_bot)
        
        fac2_rank_top2 = wts_ini_top           
        fac2_rank_bot2 = wts_ini_bot
        
        if(liq_weight ==1)
        {
          turnover = data.frame(read.csv(paste(path_input,"CASH DB/","turnover.csv",sep="")))
          
          turnover_m = turnover[,-1]
          
          turnover_m = (rollapply(turnover[,-1],66,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE))
          
          turnover_m_top = turnover_m
          turnover_m_bot = turnover_m
          
          turnover_m_top1 = turnover_m_top
          turnover_m_bot1 = turnover_m_bot
          
          turnover_m_top[is.na(turnover_m_top)] = 0
          turnover_m_bot[is.na(turnover_m_bot)] = 0
          
          turnover_m_top[fac2_rank_top2 <= 0] = 0
          
          turnover_m_bot[fac2_rank_bot2 >= 0] = 0
          
          
          turnover_m_top1[fac2_rank_top2 <= 0] = NA
          
          turnover_m_bot1[fac2_rank_bot2 >= 0] = NA
          
          turnover_top_cap  = fac2_rank_top2
          turnover_top_cap[,] = NA
          turnover_bot_cap = turnover_top_cap
          #turnover_top_cap = as.matrix(turnover_top_cap)
          
          
          turnover_top_cap1 = round((apply(turnover_m_top1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          
          turnover_bot_cap1 = round((apply(turnover_m_bot1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          
          
          #           
          #           x1= data.frame(turnover_top_cap)
          #           x2= data.frame(turnover_bot_cap)
          #           #x3 = data.frame(round((apply(turnover_m_top1, 1, quantile,probs=0.9 ,na.rm =TRUE,ties.method="random"))))
          #           
          turnover_top_cap1[is.na(turnover_top_cap1)]  = 0
          turnover_bot_cap1[is.na(turnover_bot_cap1)]  = 0
          
          turnover_top_cap[,1:ncol(turnover_top_cap)] = (turnover_top_cap1) 
          turnover_bot_cap[,1:ncol(turnover_bot_cap)] = (turnover_bot_cap1)
          
          
          #turnover_m_top[fac2_rank_top2 <= 0] = NA
          #x = turnover_m_top1
          #turnover_m_top1[turnover_m_top[which(!is.na(turnover_m_top))] > turnover_top_cap[which(!is.na(turnover_top_cap))]] = turnover_top_cap
          
          turnover_m_top1[turnover_m_top > turnover_top_cap] = turnover_top_cap[turnover_m_top > turnover_top_cap]
          turnover_m_bot1[turnover_m_bot > turnover_bot_cap] = turnover_bot_cap[turnover_m_bot > turnover_bot_cap]
          
          
          
          #turnover_m_top1 = data.frame(turnover_m_top1)
          
          
          fac2_rank_top2 = turnover_m_top1/(2*rowSums(abs(turnover_m_top1),na.rm=TRUE))
          fac2_rank_bot2 = turnover_m_bot1/(2*rowSums(abs(turnover_m_bot1),na.rm=TRUE))
          
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
          
          
          
          for(iii in 1:n-1)
          {
            
            final_weight  = STOCK_CAP(fac2_rank_top2,fac2_rank_bot2)
            
            fac2_rank_top2  =(final_weight[[1]])
            fac2_rank_bot2  =(final_weight[[2]])
          }
          
          
          wts_ini_top  = data.frame(fac2_rank_top2)            
          wts_ini_bot = data.frame((-1)*fac2_rank_bot2)
          
          
        }
        
        if(liq_weight ==2)
        {
          
          turnover = data.frame(read.csv(paste(path_input,"CASH DB/","turnover.csv",sep="")))
          
          turnover_m = turnover[,-1]
          
          turnover_m = (rollapply(turnover[,-1],66,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE))
          
          turnover_m_top = turnover_m
          turnover_m_bot = turnover_m
          
          turnover_m_top1 = turnover_m_top
          turnover_m_bot1 = turnover_m_bot
          
          turnover_m_top[is.na(turnover_m_top)] = 0
          turnover_m_bot[is.na(turnover_m_bot)] = 0
          
          turnover_m_top[fac2_rank_top2 <= 0] = 0
          
          turnover_m_bot[fac2_rank_bot2 >= 0] = 0
          
          
          turnover_m_top1[fac2_rank_top2 <= 0] = NA
          
          turnover_m_bot1[fac2_rank_bot2 >= 0] = NA
          # 
          # turnover_top_cap  = fac2_rank_top2
          # turnover_top_cap[,] = NA
          # turnover_bot_cap = turnover_top_cap
          #turnover_top_cap = as.matrix(turnover_top_cap)
          
          
          # turnover_top_cap1 = round((apply(turnover_m_top1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          # 
          # turnover_bot_cap1 = round((apply(turnover_m_bot1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
          # 
          # 
          # #           
          # #           x1= data.frame(turnover_top_cap)
          # #           x2= data.frame(turnover_bot_cap)
          # #           #x3 = data.frame(round((apply(turnover_m_top1, 1, quantile,probs=0.9 ,na.rm =TRUE,ties.method="random"))))
          # #           
          # turnover_top_cap1[is.na(turnover_top_cap1)]  = 0
          # turnover_bot_cap1[is.na(turnover_bot_cap1)]  = 0
          # 
          # turnover_top_cap[,1:ncol(turnover_top_cap)] = (turnover_top_cap1) 
          # turnover_bot_cap[,1:ncol(turnover_bot_cap)] = (turnover_bot_cap1)
          # 
          # 
          # #turnover_m_top[fac2_rank_top2 <= 0] = NA
          # #x = turnover_m_top1
          # #turnover_m_top1[turnover_m_top[which(!is.na(turnover_m_top))] > turnover_top_cap[which(!is.na(turnover_top_cap))]] = turnover_top_cap
          # 
          # turnover_m_top1[turnover_m_top > turnover_top_cap] = turnover_top_cap[turnover_m_top > turnover_top_cap]
          # turnover_m_bot1[turnover_m_bot > turnover_bot_cap] = turnover_bot_cap[turnover_m_bot > turnover_bot_cap]
          # 
          # 
          
          #turnover_m_top1 = data.frame(turnover_m_top1)
          
          wts_ini_top = data.frame(t(apply(turnover_m_top1, 1, rank,na.last="keep",ties.method="random")))
          wts_ini_bot = data.frame(t(apply(turnover_m_bot1, 1, rank,na.last="keep",ties.method="random")))
          
          # wts_ini_top = wts_ini_top1 + wts_ini_top2 
          # wts_ini_bot = wts_ini_bot1 + wts_ini_bot2
          # 
          
          wts_ini_top<-wts_ini_top/(2*rowSums(abs(wts_ini_top),na.rm=TRUE))
          wts_ini_bot<-((-1)*wts_ini_bot)/(2*rowSums(abs(wts_ini_bot),na.rm=TRUE))
          
          
          wts_ini_top[is.na(wts_ini_top)]<-0
          wts_ini_bot[is.na(wts_ini_bot)]<-0
          
          wts_ini_top<-data.frame(wts_ini_top)
          wts_ini_bot<-data.frame(wts_ini_bot)
          
          
        }
        
        
        
        wts_tb =  wts_ini_top + wts_ini_bot
        
        
        #         wts_ini_top[wts_tb<=0]<-0
        #         wts_ini_bot[wts_tb>=0]<-0
        #         
        
        colnames(wts_ini_bot)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
        colnames(wts_ini_top)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
        
        
      }
      
      ####################################################################################################################################################
      
      
      if (topbot==4){  # This will select the top x and bottom x stocks and rank weigh the same
        
        fac_clas_sel_1_tb<-as.matrix(factor_rnk)
        
        fac_clas_sel_2_tb<-(fac_clas_sel_1_tb-rowMeans(fac_clas_sel_1_tb,na.rm=TRUE))/rowSums(abs(fac_clas_sel_1_tb-rowMeans(fac_clas_sel_1_tb,na.rm=TRUE)),na.rm=TRUE)
        
        wts_ini_tb<-data.frame(fac_clas_sel_2_tb)
        
        wts_tb<-wts_ini_tb
        
        
        rnk_top<-factor_rnk
        #rnk_top<-fac1_rnk
        
        rnk_bot<-factor_rnk 
        #rnk_bot<-fac1_rnk
        
        ############ The below will calculate the top and bottom ranks
        
        topmax<-data.frame((apply(rnk_top,1,max,na.rm=TRUE)))
        ############ The below will calculate the top and bottom ranks
        topminc = round((apply(rnk_top, 1, quantile,probs=ptop ,na.rm =TRUE,ties.method="random")))
        
        botminc = round((apply(rnk_top, 1, quantile,probs=pbot ,na.rm =TRUE,ties.method="random")))
        #topminc<-data.frame((apply(rnk_top,1,max,na.rm=TRUE)))
        #topminc[topminc<=xnum]<-xnum     # This is to make sure that the total number is not less than selected numbers
        
        topminc_cbnd <- topminc
        
        for (j in 2:dim(wts_ini)[2] ){
          topminc_cbnd<-cbind(topminc_cbnd,topminc)
          
        }
        
        
        botminc_cbnd <- botminc
        
        for (j in 2:dim(wts_ini)[2] ){
          botminc_cbnd<-cbind(botminc_cbnd,botminc)
          
        }
        
        wts_ini_top<-wts_tb
        wts_ini_bot<-wts_tb
        
        wts_ini_top[rnk_top < topminc_cbnd] <-NA
        wts_ini_bot[rnk_bot > botminc_cbnd] = NA
        
        wts_ini_top1 = data.frame(t(apply(wts_ini_top, 1, rank,na.last="keep",ties.method="random")))
        wts_ini_bot1 = data.frame(t(apply((-1)*wts_ini_bot, 1, rank,na.last="keep",ties.method="random")))
        
        
        
        #         
        #         wts_tb[rnk_top < topminc_cbnd & rnk_top >botminc_cbnd] <-NA
        #         wts_tb<-wts_tb/rowSums(abs(wts_tb),na.rm=TRUE)
        #       
        
        # wts_ini_top[is.na(wts_ini_top)]<-0
        # wts_ini_bot[is.na(wts_ini_bot)]<-0
        
        #         wts_ini_top<-data.frame(wts_ini_top)
        #         wts_ini_bot<-data.frame(wts_ini_bot)
        
        fac2_rank_top2 = wts_ini_top1           
        fac2_rank_bot2 = wts_ini_bot1
        
        
        turnover = data.frame(read.csv(paste(path_input,"CASH DB/","turnover.csv",sep="")))
        
        turnover_m = turnover[,-1]
        
        turnover_m = (rollapply(turnover[,-1],66,median,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE))
        
        turnover_m_top = turnover_m
        turnover_m_bot = turnover_m
        
        turnover_m_top1 = turnover_m_top
        turnover_m_bot1 = turnover_m_bot
        
        turnover_m_top[is.na(turnover_m_top)] = 0
        turnover_m_bot[is.na(turnover_m_bot)] = 0
        
        turnover_m_top[fac2_rank_top2 <= 0] = 0
        
        turnover_m_bot[fac2_rank_bot2 >= 0] = 0
        
        
        turnover_m_top1[is.na(fac2_rank_top2)] = NA
        
        turnover_m_bot1[is.na(fac2_rank_bot2)] = NA
        # 
        # turnover_top_cap  = fac2_rank_top2
        # turnover_top_cap[,] = NA
        # turnover_bot_cap = turnover_top_cap
        #turnover_top_cap = as.matrix(turnover_top_cap)
        
        
        # turnover_top_cap1 = round((apply(turnover_m_top1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
        # 
        # turnover_bot_cap1 = round((apply(turnover_m_bot1, 1, quantile,probs=turnover_cutoff ,na.rm =TRUE,ties.method="random")))
        # 
        # 
        # #           
        # #           x1= data.frame(turnover_top_cap)
        # #           x2= data.frame(turnover_bot_cap)
        # #           #x3 = data.frame(round((apply(turnover_m_top1, 1, quantile,probs=0.9 ,na.rm =TRUE,ties.method="random"))))
        # #           
        # turnover_top_cap1[is.na(turnover_top_cap1)]  = 0
        # turnover_bot_cap1[is.na(turnover_bot_cap1)]  = 0
        # 
        # turnover_top_cap[,1:ncol(turnover_top_cap)] = (turnover_top_cap1) 
        # turnover_bot_cap[,1:ncol(turnover_bot_cap)] = (turnover_bot_cap1)
        # 
        # 
        # #turnover_m_top[fac2_rank_top2 <= 0] = NA
        # #x = turnover_m_top1
        # #turnover_m_top1[turnover_m_top[which(!is.na(turnover_m_top))] > turnover_top_cap[which(!is.na(turnover_top_cap))]] = turnover_top_cap
        # 
        # turnover_m_top1[turnover_m_top > turnover_top_cap] = turnover_top_cap[turnover_m_top > turnover_top_cap]
        # turnover_m_bot1[turnover_m_bot > turnover_bot_cap] = turnover_bot_cap[turnover_m_bot > turnover_bot_cap]
        # 
        # 
        
        #turnover_m_top1 = data.frame(turnover_m_top1)
        
        wts_ini_top2 = data.frame(t(apply(turnover_m_top1, 1, rank,na.last="keep",ties.method="random")))
        wts_ini_bot2 = data.frame(t(apply(turnover_m_bot1, 1, rank,na.last="keep",ties.method="random")))
        
        wts_ini_top = wts_ini_top1 + wts_ini_top2 
        wts_ini_bot = wts_ini_bot1 + wts_ini_bot2
        
        
        wts_ini_top<-wts_ini_top/(2*rowSums(abs(wts_ini_top),na.rm=TRUE))
        wts_ini_bot<-((-1)*wts_ini_bot)/(2*rowSums(abs(wts_ini_bot),na.rm=TRUE))
        
        
        wts_ini_top[is.na(wts_ini_top)]<-0
        wts_ini_bot[is.na(wts_ini_bot)]<-0
        
        wts_ini_top<-data.frame(wts_ini_top)
        wts_ini_bot<-data.frame(wts_ini_bot)
        
        
        wts_tb =  wts_ini_top + wts_ini_bot
        
        
        #         wts_ini_top[wts_tb<=0]<-0
        #         wts_ini_bot[wts_tb>=0]<-0
        #         
        
        colnames(wts_ini_bot)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
        colnames(wts_ini_top)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
        
        
      }
      
      ####################################################################################################################################################
      
      wts_fin = data.frame(wts_fin/rowSums(abs(wts_fin),na.rm=TRUE))
      
      #wts_sel_int<-wts_ini[wts_ini[2,]==i]     # This is the internal wts for calculation
      
      if(i>1)
      {
        fac_clas[,group[1,]==0]<-NA
        fac_clas_sell<-fac_clas[,group[1,]==0]
        flag=1
        
        wts_fin<-cbind(wts_fin,data.frame(fac_clas_sell))
        
        
        
        ############################################################################ Order Here
        # list_names = colnames(factor_rnk[1])
        
        wts_fin = wts_fin[,colnames(factor_rnk)]
        ########################################################################### Ordering ends  
        
      }
      
      
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
          
          lng_beta_full[m,1]<- (cov(lng_ret_full[(m-250):(m-1),1],stck_lvl1[c(-1,-2),2][(m-250):(m-1)])/var(stck_lvl1[c(-1,-2),2][(m-250):(m-1)]))
          sht_beta_full[m,1]<- -(cov(sht_ret_full[(m-250):(m-1),1],stck_lvl1[c(-1,-2),2][(m-250):(m-1)])/var(stck_lvl1[c(-1,-2),2][(m-250):(m-1)]))
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
      
      
      TV_full = data.frame(rowSums(abs(wts_int_full[-1,]-wts_int_full[-1*dim(wts_int_full)[1],])))[-1,]
      
      
      #lng_ret_full<-data.frame(rowSums(wts_int_pos_full_calc*stck_lvl_calc))
      #tot_ret_full<-data.frame((wts_int_full[c(-1*dim(wts_int_full)[1],-1*dim(wts_int_full)[1] +1),]*stck_lvl_calc)-cst_full)  
      
      tot_ret_full<-data.frame((lng_ret_full+sht_ret_full)-cst_full)
      
      
      lng_ret_full<-data.frame(lng_ret_full-cst_full_L)
      sht_ret_full<-data.frame(sht_ret_full-cst_full_S)
      
      ret_fin_full<-data.frame(cbind(stck_lvl$Date[c(-1,-2)],tot_ret_full,lng_ret_full,sht_ret_full,wts_int_full[c(-1,-1*dim(wts_int_full)[1]),]))
      
      tot_ret_full[is.na(tot_ret_full)]<-0
      TV_full[is.na(TV_full)]<-0
      
      result_full = data.frame(cbind(t(ann_rslt(data.frame(stck_lvl$Date[c(-1,-2)],tot_ret_full,TV_full))),paste(names(fac_list1)[z],"_",names(uni_list)[u])))
      colnames(result_full)[dim(result_full)[2]] = "FACTOR"
      #write.csv(ret_fin_full,paste(path_output,fac_list[z,2],"_",uni_list[u,2],"_FULL.csv"),row.names=FALSE)
      write.csv(result_full,paste(path_output,names(fac_list1)[z],"_",names(uni_list)[u],"_FULL_Results.csv"),row.names=FALSE)
      
      #write.csv(ret_fin_full, file = "ret_fin_full.csv")
      
      #write.csv(ret_fin_full,paste(path_output,fac_list[z,2],"_",uni_list[u,2],"_FULL.csv"),row.names=FALSE)
      write.csv(ret_fin_full,paste(path_output,names(fac_list1)[z],"_",names(uni_list)[u],"_FULL_PORTFOLIO.csv"),row.names=FALSE)
      ################################################################################################################################
      
      ################################################################################################################################
      if (topbot!=0){       
        
        wts_ini_full_tb <- (wts_ini_top+wts_ini_bot)
        
        
        wts_ini_top_calc<-wts_ini_top[c(-1*dim(wts_ini_top)[1],-1*dim(wts_ini_top)[1]+1),]                        #return calculation for top-bot 10
        wts_ini_bot_calc<-wts_ini_bot[c(-1*dim(wts_ini_bot)[1],-1*dim(wts_ini_bot)[1]+1),]
        
        lng_ret_tb<-data.frame(rowSums(wts_ini_top_calc*stck_lvl_calc))
        sht_ret_tb<-data.frame(rowSums(wts_ini_bot_calc*stck_lvl_calc))
        
        
        lng_beta_tb<- matrix(1,dim(lng_ret_tb)[1],1)
        sht_beta_tb<- matrix(1,dim(sht_ret_tb)[1],1)
        
        
        if (bna==1){        # This will check if beta neutral needs to be done or not
          
          
          for(m in 251: dim(lng_beta_tb)[1])     # This will quantify the beta ratio between long and short in the portfolio
          {                                  
            
            #print(m)  
            
            lng_beta_tb[m,1]<- (cov(lng_ret_tb[(m-250):(m-1),1],stck_lvl1[c(-1,-2),2][(m-250):(m-1)])/var(stck_lvl1[c(-1,-2),2][(m-250):(m-1)]))
            sht_beta_tb[m,1]<- -(cov(sht_ret_tb[(m-250):(m-1),1],stck_lvl1[c(-1,-2),2][(m-250):(m-1)])/var(stck_lvl1[c(-1,-2),2][(m-250):(m-1)]))
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
        
        
        TV_tb<-data.frame(rowSums(abs(wts_ini_full_tb[-1,]-wts_ini_full_tb[-1*dim(wts_ini_full_tb)[1],])))[-1,]
        
        tot_ret_tb<-data.frame((lng_ret_tb+sht_ret_tb) - cst_tb)
        
        lng_ret_tb<-data.frame(lng_ret_tb-cst_tb_L)
        sht_ret_tb<-data.frame(sht_ret_tb-cst_tb_S)
        
        #ret_fin_tb<-data.frame(cbind(stck_lvl$Returns[c(-1,-2)],tot_ret_tb,lng_ret_tb,sht_ret_tb,topname[c(-1,-1*dim(topname)[1]),],botname[c(-1,-1*dim(botname)[1]),]))
        
        ret_fin_tb<-data.frame(cbind(stck_lvl$Date[c(-1,-2)],tot_ret_tb,lng_ret_tb,sht_ret_tb,wts_ini_full_tb[c(-1,-1*dim(wts_ini_full_tb)[1]),]))
        
        
        #ret_fin_tb1<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sum))
        #ret_fin_tb2<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),mean))
        #ret_fin_tb3<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sd))
        
        #write.csv(ret_fin_tb, file = "ret_fin_tb.csv")
        #write.csv(ret_fin_tb,paste(path_output,fac_list[z,2],"_",uni_list[u,2],"_TOPBOT.csv"),row.names=FALSE)
        
        tot_ret_tb[is.na(tot_ret_tb)]<-0
        TV_tb[is.na(TV_tb)]<-0
        
        result_tb = data.frame(cbind(t(ann_rslt(data.frame(stck_lvl$Date[c(-1,-2)],tot_ret_tb,TV_tb))),paste(names(fac_list1)[z],"_",names(uni_list)[u])))
        #result_full = data.frame(cbind(t(ann_rslt(data.frame(stck_lvl$Date[c(-1,-2)],tot_ret_full,TV_full))),paste(names(fac_list1)[z],"_",names(uni_list)[u])))
        colnames(result_tb)[dim(result_tb)[2]] = "FACTOR"
        #write.csv(ret_fin_full,paste(path_output,fac_list[z,2],"_",uni_list[u,2],"_FULL.csv"),row.names=FALSE)
        write.csv(result_tb,paste(path_output,names(fac_list1)[z],"_",names(uni_list)[u],"_TB_Results.csv"),row.names=FALSE)
        
        write.csv(ret_fin_tb,paste(path_output,names(fac_list1)[z],"_",names(uni_list)[u],"_TB_PORTFOLIO.csv"),row.names=FALSE)
        
      }
      
      
      ret_full_cmbn = data.frame(cbind(ret_full_cmbn,tot_ret_full))
      colnames(ret_full_cmbn)[z+(u-1)*(length(fac_list1))+1]= paste(names(fac_list1)[z],"_",names(uni_list)[u])
      
      result_full_cmbn = data.frame(rbind(result_full_cmbn,result_full))
      
      ret_full_cmbn_L = data.frame(cbind(ret_full_cmbn_L,lng_ret_full))
      colnames(ret_full_cmbn_L)[z+(u-1)*(length(fac_list1))+1]= paste(names(fac_list1)[z],"_",names(uni_list)[u])
      
      ret_full_cmbn_S = data.frame(cbind(ret_full_cmbn_S, sht_ret_full))
      colnames(ret_full_cmbn_S)[z+(u-1)*(length(fac_list1))+1]= paste(names(fac_list1)[z],"_",names(uni_list)[u])
      
      if (topbot!=0){   
        ret_tb_cmbn = data.frame(cbind(ret_tb_cmbn,tot_ret_tb))
        colnames(ret_tb_cmbn)[z+(u-1)*(length(fac_list1))+1]= paste(names(fac_list1)[z],"_",names(uni_list)[u])
        
        result_tb_cmbn = data.frame(rbind(result_tb_cmbn,result_tb))
        
        ret_tb_cmbn_L = data.frame(cbind(ret_tb_cmbn_L,lng_ret_tb))
        colnames(ret_tb_cmbn_L)[z+(u-1)*(length(fac_list1))+1]= paste(names(fac_list1)[z],"_",names(uni_list)[u])
        
        ret_tb_cmbn_S = data.frame(cbind(ret_tb_cmbn_S, sht_ret_tb))
        colnames(ret_tb_cmbn_S)[z+(u-1)*(length(fac_list1))+1]= paste(names(fac_list1)[z],"_",names(uni_list)[u])
        
      }
      
    }
  }
}



write.csv(ret_full_cmbn,paste(path_output,"FULL_CMBND_RETURNS.csv"),row.names=FALSE)
write.csv(result_full_cmbn,paste(path_output,"RESULTS_FULL_CMBND.csv"))
write.csv(ret_full_cmbn_L,paste(path_output,"LONG_FULL_CMBND_RETURNS.csv"),row.names=FALSE)
write.csv(ret_full_cmbn_S,paste(path_output,"SHT_FULL_CMBND_RETURNS.csv"),row.names=FALSE)

write.csv(ret_tb_cmbn,paste(path_output,"TOP_BOT_CMBND_RETURNS.csv"),row.names=FALSE)
write.csv(result_tb_cmbn,paste(path_output,"RESULTS_TB_CMBND.csv"))
write.csv(ret_tb_cmbn_L,paste(path_output,"LONG_TOP_BOT_CMBND_RETURNS.csv"),row.names=FALSE)
write.csv(ret_tb_cmbn_S,paste(path_output,"SHT_TOP_BOT_CMBND_RETURNS.csv"),row.names=FALSE)
