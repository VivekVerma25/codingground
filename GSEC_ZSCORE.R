# This is the code for creating return, given stock list and weights , every rebalancing date

##################################################### Libraries

library("xts")
library("zoo")
library("matrixStats")

############################ Inputs to the code
setwd("D:/LFT/Template/tester/R test data/full simulation")
path_input<-"D:/LFT/Template/tester/R test data/full simulation/FINAL FILES/INPUT/"
path_output<-"D:/LFT/Template/tester/R test data/full simulation/FINAL FILES/OUTPUT/GSEC ZSCORE/"

stck_lvl<-data.frame(read.csv(paste(path_input,"FUT DB/","global sec returns.csv",sep="")))
stck_lvl$Date<-as.Date(stck_lvl$Date,"%d/%m/%Y")


bna<-1  # Keep this 1 if you need beta neutral
topbot<-3 # Make this 1 if you neeed top x and bottom x equay weighted
xnum<-20  # This is the number of stocks you need to select both on the top side and bottom side
dsort <-0 # For enabling double sort
dnum1<-25 # No. of stocks for primary factor
dnum2<-10 # no. of stocks for sec factor 

##############################################

tc<-(0.0004)                                # This is one side trnsaction cost 

############################################################ Factor inout, can be daily_monthly but in same order

#clafic<-data.frame(read.csv(paste(path_input,"sector neutral.csv",sep="")))[,-1]    # This means the sectors applicable stocks                   # This will differentiate between sectors or others
#clafic<-data.frame(read.csv(paste(path_input,"Sectors.csv",sep="")))[,-1]    # This means the sectors applicable stocks                   # This will differentiate between sectors or others

#n<-max(clafic)                                # This signifies the numbers of sectors etc

######################################################################################################################################################
RAW_IN_PRICE= data.frame(read.csv(paste(path_input,"CASH DB/","global sec px_last delay0.csv",sep="")))

list_IN_P <- vector('list',7)
#list_DV <- vector('list',3)
#list_PDV <- vector('list',3)

lookback_period=c(30,60,90,120,150,180,240)

for(l in 1:length(lookback_period)){
  
  wq1<-RAW_IN_PRICE
  dt<-wq1[1:nrow(wq1),1]
  m<-rollapply(wq1[,-1],lookback_period[l],mean,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  s<-rollapply(wq1[,-1],lookback_period[l],sd,na.rm =TRUE,fill=NA,align = "right",by.column=TRUE)
  roll= (wq1[,-1]-m)/s
  
  rollfin <- data.frame(roll)
  
  #rollfin[is.na(rollfin)] =0
  rollfin<-as.matrix(rollfin)
  rollfin[which(!is.finite(rollfin))] <- NA
  rollfin<-data.frame(DATE=dt,rollfin)
  
  list_IN_P[[l]] <-rollfin      
}

#names(list_IN_PV) <- list("PMN01","PMN02","PMN03")
names(list_IN_P) <- list("GSEC30","GSEC60","GSEC90","GSEC120","GSEC150","GSEC180","GSEC240")
#######################################################################################################################################################

fac_list = list_IN_P

ret_full_cmbn = data.frame(stck_lvl$Date[c(-1,-2)])
colnames(ret_full_cmbn)= "DATE"

ret_tb_cmbn = ret_full_cmbn
ret_full_cmbn_L = ret_full_cmbn
ret_full_cmbn_S = ret_full_cmbn
ret_tb_cmbn_L = ret_tb_cmbn
ret_tb_cmbn_S= ret_tb_cmbn

stapp = data.frame(read.csv(paste(path_input,"UNIVERSE/","global sec universe.csv",sep="")))

#uni_list = data.frame(read.csv(paste(path_input,"BASIS UNI.csv",sep="")))


#uni_list$dir = as.character(uni_list$dir)
#uni_list$files = as.character(uni_list$files)
#uni_row <-  dim(uni_list)[1]

#for(z in 1 : dim(fac_list)[1])
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
  #fac_clas<-rnk
  fac_clas<-f_factor
  
  #bbeta<-data.frame(read.csv(paste(path_input,"bbeta.csv",sep="")))            # Here input beta's for all the sectors
  
  if(dsort ==1){
    
    fac_2<-data.frame(read.csv(paste(path_input,"factor.csv",sep=""))) 
    
    f_factor2<-data.frame(fac_2)                                 # This will be the final clculate foactor to be used for ranking
    f_factor2<-f_factor2[,-1]
    f_factor2[stapp[,-1]==0]<-NA
    f_factor2[f_factor2==0]<-NA
    fac2_top<-f_factor2
    fac2_bot<-f_factor2
    
    
    fac2_top[fac1_rank <(dim(fac2_top)[2]-rowSums(is.na(fac2_top)))- dnum1+1]<- NA  
    
    fac2_rank_top<- data.frame(t(apply(fac2_top, 1, rank,na.last="keep",ties.method="max"))) 
    
    fac2_bot[fac1_rank >dnum1]<- NA  
    
    fac2_rank_bot<- data.frame(t(apply(fac2_bot, 1, rank,na.last="keep",ties.method="max")))
    
    
    fac2_rank_top1<-fac2_rank_top
    
    fac2_rank_bot1<-fac2_rank_bot 
    
    fac2_rank_top1[fac2_rank_top1 <(dim(fac2_rank_top1)[2]-rowSums(is.na(fac2_rank_top1)))- dnum2+1]<- NA
    
    
    fac2_rank_bot1[fac2_rank_bot1>dnum2]<- NA
    
    fac2_rank_top2<-fac2_rank_top1
    
    fac2_rank_bot2<-fac2_rank_bot1
    
    
    fac2_rank_top2[is.na(fac2_rank_top2)] <-0   #Final double sorted equal weights
    fac2_rank_top2[fac2_rank_top2 >0]<-0.1
    fac2_rank_bot2[is.na(fac2_rank_bot2)] <-0
    fac2_rank_bot2[fac2_rank_bot2 >0]<--0.1
    
    
    colnames(fac2_rank_top2)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
    colnames(fac2_rank_bot2)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
    
    
    topname<-matrix(0,dim(fac2_rank_top2)[1],dnum2)
    botname<-matrix(0,dim(fac2_rank_bot2)[1],dnum2)
    
    
    
    
    for (k in 1:dim(fac2_rank_top2)[1]){
      
      topname[k,]<-colnames(fac2_rank_top2[k,fac2_rank_top2[k,]>0])
      botname[k,]<-colnames(fac2_rank_bot2[k,fac2_rank_bot2[k,]<0])   
    }
    
    
    wts_ini_top_calc<-fac2_rank_top2[-1*dim(fac2_rank_top2)[1],]
    wts_ini_bot_calc<-fac2_rank_bot2[-1*dim(fac2_rank_bot2)[1],]
    
    wts_ini_full_tb <- (fac2_rank_top2 + fac2_rank_bot2)
    
    cst_tb<-data.frame(rowSums(abs(wts_ini_full_tb[-1,]-wts_ini_full_tb[-1*dim(wts_ini_full_tb)[1],])*tc))
    
    
    stck_lvl_calc<-stck_lvl[-1,-1]
    
    lng_ret_tb<-data.frame(rowSums(wts_ini_top_calc*stck_lvl_calc))
    sht_ret_tb<-data.frame(rowSums(wts_ini_bot_calc*stck_lvl_calc))
    
    
    lng_beta_tb<- matrix(1,dim(lng_ret_tb)[1],1)
    sht_beta_tb<- matrix(1,dim(sht_ret_tb)[1],1)
    
    
    if (bna==1){        # This will check if beta neutral needs to be done or not
      
      
      for(m in 251: dim(lng_beta_tb)[1])     # This will quantify the beta ratio between long and short in the portfolio
      {                                  
        
        #print(m)  
        
        lng_beta_tb[m,1]<- (cov(lng_ret_tb[(m-250):(m-1),1],stck_lvl[-1,2][(m-250):(m-1)])/var(stck_lvl[-1,2][(m-250):(m-1)]))
        sht_beta_tb[m,1]<- -(cov(sht_ret_tb[(m-250):(m-1),1],stck_lvl[-1,2][(m-250):(m-1)])/var(stck_lvl[-1,2][(m-250):(m-1)]))
      }
      
    } 
    
    
    tot_ret_tb<-data.frame(((lng_ret_tb+sht_ret_tb*(lng_beta_tb/sht_beta_tb))/2) - cst_tb)
    
    
    ret_fin_tb<-data.frame(cbind(stck_lvl$Returns[-1],tot_ret_tb,topname[-1,],botname[-1,]))
    
    #ret_fin_tb1<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sum))
    #ret_fin_tb2<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),mean))
    #ret_fin_tb3<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sd))
    
    write.csv(ret_fin_tb, file = "ret_fin_ds.csv")
    
  }
  
  
  
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
    
    #write.csv(ret_fin_full, file = "ret_fin_full.csv")
    
    #write.csv(ret_fin_full,paste(path_output,fac_list[z,2],"_",uni_list[u,2],"_FULL.csv"),row.names=FALSE)
    write.csv(ret_fin_full,paste(path_output,names(fac_list)[z],"_FULL.csv"),row.names=FALSE)
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
      
      
      #ret_fin_tb1<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sum))
      #ret_fin_tb2<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),mean))
      #ret_fin_tb3<- data.frame(aggregate(ret_fin_tb[,2],by=list((substr(ret_fin_tb[,1],1,4))),sd))
      
      #write.csv(ret_fin_tb, file = "ret_fin_tb.csv")
      #write.csv(ret_fin_tb,paste(path_output,fac_list[z,2],"_",uni_list[u,2],"_TOPBOT.csv"),row.names=FALSE)
      write.csv(ret_fin_tb,paste(path_output,names(fac_list)[z],"_TOPBOT.csv"),row.names=FALSE)
      
    }
  }
  ret_full_cmbn = data.frame(cbind(ret_full_cmbn,tot_ret_full))
  colnames(ret_full_cmbn)[z+1]= names(fac_list)[z]
  
  ret_full_cmbn_L = data.frame(cbind(ret_full_cmbn_L,lng_ret_full))
  colnames(ret_full_cmbn_L)[z+1]= names(fac_list)[z]
  
  ret_full_cmbn_S = data.frame(cbind(ret_full_cmbn_S, sht_ret_full))
  colnames(ret_full_cmbn_S)[z+1]= names(fac_list)[z]
  
  if (topbot==2){   
    ret_tb_cmbn = data.frame(cbind(ret_tb_cmbn,tot_ret_tb))
    colnames(ret_tb_cmbn)[z+1]= names(fac_list)[z]
    
    ret_tb_cmbn_L = data.frame(cbind(ret_tb_cmbn_L,lng_ret_tb))
    colnames(ret_tb_cmbn_L)[z+1]= names(fac_list)[z]
    
    ret_tb_cmbn_S = data.frame(cbind(ret_tb_cmbn_S, sht_ret_tb))
    colnames(ret_tb_cmbn_S)[z+1]= names(fac_list)[z]
    
  }
  
}


write.csv(ret_full_cmbn,paste(path_output,"FULL_CMBND_RETURNS.csv"),row.names=FALSE)
write.csv(ret_full_cmbn_L,paste(path_output,"LONG_FULL_CMBND_RETURNS.csv"),row.names=FALSE)
write.csv(ret_full_cmbn_S,paste(path_output,"SHT_FULL_CMBND_RETURNS.csv"),row.names=FALSE)

write.csv(ret_tb_cmbn,paste(path_output,"TOP_BOT_CMBND_RETURNS.csv"),row.names=FALSE)
write.csv(ret_tb_cmbn_L,paste(path_output,"LONG_TOP_BOT_CMBND_RETURNS.csv"),row.names=FALSE)
write.csv(ret_tb_cmbn_S,paste(path_output,"SHT_TOP_BOT_CMBND_RETURNS.csv"),row.names=FALSE)
