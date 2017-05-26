# This is the code for creating return, given stock list and weights , every rebalancing date

##################################################### Libraries

library("xts")
library("zoo")
library("matrixStats")

############################ Inputs to the code
setwd("D:/LFT/Template/tester/R test data/full simulation")
path_input<-"D:/LFT/Template/tester/R test data/full simulation/FINAL FILES/INPUT/"
path_output<-"D:/LFT/Template/tester/R test data/full simulation/FINAL FILES/OUTPUT/BASIS ZSCORE/"

stck_lvl<-data.frame(read.csv(paste(path_input,"FUT DB/","RollAdjReturns.csv",sep="")))
stck_lvl$Date<-as.Date(stck_lvl$Date,"%d/%m/%Y")


bna<-1  # Keep this 1 if you need beta neutral
topbot<-2 # Make this 1 if you neeed top x and bottom x equay weighted
xnum<-10  # This is the number of stocks you need to select both on the top side and bottom side
dsort <-0 # For enabling double sort
dnum1<-25 # No. of stocks for primary factor
dnum2<-10 # no. of stocks for sec factor 

##############################################

tc<-(0.0004)                                # This is one side trnsaction cost 
cs=5 #min of 5 stocks should be there or else no trade
fa=1 #whether fixed allocation or not
############################################################ Factor inout, can be daily_monthly but in same order

#clafic<-data.frame(read.csv(paste(path_input,"sector neutral.csv",sep="")))[,-1]    # This means the sectors applicable stocks                   # This will differentiate between sectors or others
#clafic<-data.frame(read.csv(paste(path_input,"Sectors.csv",sep="")))[,-1]    # This means the sectors applicable stocks                   # This will differentiate between sectors or others

#n<-max(clafic)                                # This signifies the numbers of sectors etc

######################################################################################################################################################
raw_cash_close= data.frame(read.csv(paste(path_input,"CASH DB/","px_last.csv",sep="")))
raw_est_eps= data.frame(read.csv(paste(path_input,"ANALYST DB/","BEST_EPS.csv",sep="")))
list_PEST <- vector('list',1)

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
  
  list_PEST[[l]] <-rollfin      
}

names(list_PEST) <- list("PMN05")

#######################################################################################################################################################


fac_list = list_PEST

ret_full_cmbn = data.frame(stck_lvl$Date[c(-1,-2)])
colnames(ret_full_cmbn)= "DATE"

ret_tb_cmbn = ret_full_cmbn
ret_full_cmbn_L = ret_full_cmbn
ret_full_cmbn_S = ret_full_cmbn
ret_tb_cmbn_L = ret_tb_cmbn
ret_tb_cmbn_S= ret_tb_cmbn

stapp = data.frame(read.csv(paste(path_input,"UNIVERSE/","N50_TOP50_ANALYST.csv",sep="")))




fac_1<-fac_list[[1]] # factor file input

#stapp<-data.frame(read.csv(paste(uni_list[u,1],"/",uni_list[u,2],".csv",sep="")))


f_factor<-data.frame(fac_1)                                 # This will be the final clculate foactor to be used for ranking
f_factor<-((-1)*f_factor[,-1])

f_factor[stapp[,-1]==0]<-NA
#f_factor[f_factor<0.0000001 & f_factor>-0.0000001]<-NA
f_factor[is.na(f_factor)] =0

fin<- matrix(0,dim(f_factor)[1],dim(f_factor)[2]) # SIGNAL MATRIX
#pric[pric==0] <- NA
#clafic<-clafic[(lookback_period[li]+1):nrow(clafic),]
#pric[clafic==0]<-NA


fin[2,which(as.matrix(f_factor[1,]) > 1)]=1 # LONG ENTRY CONDITION/ SIGNAL LOOP
for(i in 3:nrow(fin)){
  fin[i,which(fin[i-1,]==0  & as.matrix(f_factor[i-1,])> 1)]=1
  
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


for(i in 2:nrow(f_factor)){ # Iterating through the rows 
  
  if(sum(entry_l[i-1,]==1)>0) # This is for checking exit criteria  
  {
    
    
    j<-sum(entry_l[i-1,]==1) # Long Exit
    
    while(j!=0){
      
      c<-which(entry_l[i-1,]==1)
        
      
      if(f_factor[i,c[j]]<0){ #exit criteria , factor/SL
        entry_l[i,c[j]]=0
        netp=netp-1
      }
      
      else{
        entry_l[i,c[j]]=1
      }
      
      j=j-1
    }
    
  }
  
  
  if((sum(fin[i,]==1)+netp)>=cs) # New postions matrix update
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
  if(sum(entry[i,]==1)<cs)
  {
    entry[i,]=0;
  }
  
}

entry_final = entry*f_factor
entry_final[entry_final ==0] = NA

rnk_top<-data.frame(t(apply(entry_final, 1, rank,na.last="keep",ties.method="first"))) 
rnk_top[is.na(rnk_top)] =0

for(i in 1:nrow(rnk_top)){
 
  rnk_top[i,which(rnk_top[i,] >xnum)] = 0
  rnk_top[i,which(rnk_top[i,] >0)] = 1
  
}


colnames(rnk_top) <- colnames(stapp[,-1])
newentry<-data.frame(DATE =dt,entry)
colnames(newentry) <- colnames(stapp)











