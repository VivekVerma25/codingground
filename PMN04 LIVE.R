# This is the code for creating return, given stock list and weights , every rebalancing date

##################################################### Libraries

library("xts")
library("zoo")
library("matrixStats")

############################ Inputs to the code
setwd("D:/LFT/Template/tester/R test data/full simulation")
path_input<-"D:/LFT/Template/tester/R test data/full simulation/FINAL FILES/INPUT/"
path_output<-"D:/LFT/Template/tester/R test data/full simulation/FINAL FILES/OUTPUT/PMN04 WEIGHTS/"

stck_lvl<-data.frame(read.csv(paste(path_input,"FUT DB/","global sec returns.csv",sep="")))
stck_lvl$Date<-as.Date(stck_lvl$Date,"%d/%m/%Y")


bna<-1  # Keep this 1 if you need beta neutral
xnum<-10  # This is the number of stocks you need to select both on the top side and bottom side
dsort <-0 # For enabling double sort
dnum1<-25 # No. of stocks for primary factor
dnum2<-10 # no. of stocks for sec factor 
NOS <-1 # No. of strategies
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
RAW_IN_PRICE= data.frame(read.csv(paste(path_input,"CASH DB/","global sec px_last delay0.csv",sep="")))

list_IN_P <- vector('list',4)
#list_DV <- vector('list',3)
#list_PDV <- vector('list',3)

lookback_period=c(60,120,180,240)

for(l in 1:length(lookback_period)){
  
  wq1<-RAW_IN_PRICE
  dt<-wq1[1:nrow(wq1),1]
  roll<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  rollfin <- data.frame(roll)
  rollfin<- wq1[,-1]/rollfin
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  rollfin<-data.frame(DATE=dt,rollfin)
  
  list_IN_P[[l]] <-rollfin      
}

#names(list_IN_PV) <- list("PMN01","PMN02","PMN03")
#names(list_IN_P) <- list("GSEC60","GSEC120","GSEC180","GSEC240")

#stapp = data.frame(read.csv(paste(path_input,"UNIVERSE/","global sec universe.csv",sep="")))

#uni_list = data.frame(read.csv(paste(path_input,"BASIS UNI.csv",sep="")))


#uni_list$dir = as.character(uni_list$dir)
#uni_list$files = as.character(uni_list$files)
#uni_row <-  dim(uni_list)[1]

#for(z in 1 : dim(fac_list)[1])

stapp = data.frame(read.csv(paste(path_input,"UNIVERSE/","global sec universe.csv",sep="")))

for(j in 1 : length(list_IN_P))
{
  
  fac_1<-list_IN_P[[j]] # factor file input
  
  #stapp<-data.frame(read.csv(paste(uni_list[u,1],"/",uni_list[u,2],".csv",sep="")))
  
  f_factor<-data.frame(fac_1)                                 # This will be the final clculate foactor to be used for ranking
  f_factor<-(f_factor[,-1])
  
  f_factor[stapp[,-1]==0]<-NA
  f_factor[f_factor<0.0000001 & f_factor>-0.0000001]<-NA
  
  rnk <- data.frame(t(apply(f_factor, 1, rank,na.last="keep",ties.method="max")))
  
  list_IN_P[[j]] =  data.frame(fac_1[,1],rnk)
  
}

list_full[[1]] = data.frame(fac_1[,1],(list_IN_P[[1]][,-1] + list_IN_P[[2]][,-1]+ list_IN_P[[3]][,-1] +list_IN_P[[4]][,-1])/4)

#######################################################################################################################################################
names(list_full) <- list("PMN04")
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

#######################################################################################################################################################

fac_list = list_full

for(z in 1 : length(fac_list))
{
  
  fac_1<-fac_list[[z]] # factor file input
  
  #stapp<-data.frame(read.csv(paste(uni_list[u,1],"/",uni_list[u,2],".csv",sep="")))
  
  
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
    
    #for (i in 1:1){
    
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
    
    
    #}
    
    
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
    WEIGHT_OP[z,] = wts_int_full[(dim(wts_int_full)[1]-1),]
    #write.csv(ret_fin_full, file = "ret_fin_full.csv")
    
    #write.csv(ret_fin_full,paste(path_output,fac_list[z,2],"_",uni_list[u,2],"_FULL.csv"),row.names=FALSE)
    #write.csv(ret_fin_full,paste(path_output,names(fac_list)[z],"_FULL.csv"),row.names=FALSE)
    ################################################################################################################################
    
    ################################################################################################################################

  }
#   ret_full_cmbn = data.frame(cbind(ret_full_cmbn,tot_ret_full))
#   colnames(ret_full_cmbn)[z+1]= names(fac_list)[z]
#   
#   ret_full_cmbn_L = data.frame(cbind(ret_full_cmbn_L,lng_ret_full))
#   colnames(ret_full_cmbn_L)[z+1]= names(fac_list)[z]
#   
#   ret_full_cmbn_S = data.frame(cbind(ret_full_cmbn_S, sht_ret_full))
#   colnames(ret_full_cmbn_S)[z+1]= names(fac_list)[z]
#   
#   if (topbot==2){   
#     ret_tb_cmbn = data.frame(cbind(ret_tb_cmbn,tot_ret_tb))
#     colnames(ret_tb_cmbn)[z+1]= names(fac_list)[z]
#     
#     ret_tb_cmbn_L = data.frame(cbind(ret_tb_cmbn_L,lng_ret_tb))
#     colnames(ret_tb_cmbn_L)[z+1]= names(fac_list)[z]
#     
#     ret_tb_cmbn_S = data.frame(cbind(ret_tb_cmbn_S, sht_ret_tb))
#     colnames(ret_tb_cmbn_S)[z+1]= names(fac_list)[z]
#     
#   }
  
}

FINAL_FILE = data.frame((cbind(DATE,WEIGHT_OP)))
rownames(FINAL_FILE) <- names(list_full)
#rownames(WEIGHT_OP) <- names(list_full)
#write.csv(t(WEIGHT_OP),paste(path_output,"MN ALLOC.csv"))
write.csv(t(FINAL_FILE),paste(path_output,"MN ALLOC 22-05-2017_MORNING.csv"))