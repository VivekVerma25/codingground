# This is the code for creating return, given stock list and weights , every rebalancing date

##################################################### Libraries

library("xts")
library("zoo")

############################ Inputs to the code
setwd("D:/LFT/Template/tester/R test data/")
path_input<-"D:/LFT/Template/tester/R test data/"
path_output<-"D:/LFT/Template/tester/R test data/" 

stck_lvl<-data.frame(read.csv(paste(path_input,"Roll adjusted returns.csv",sep="")))

stck_lvl$Returns<-as.Date(stck_lvl$Returns,"%d/%m/%Y")

bna<-0  # Keep this 1 if you need beta neutral
topbot<-1 # Make this 1 if you neeed top x and bottom x equay weighted
xnum<-10  # This is the number of stocks you need to select both on the top side and bottom side
dsort <-1 # For enabling double sort
dnum1<-25 # No. of stocks for primary factor
dnum2<-10 # no. of stocks for sec factor

##############################################

tc<-(0)                                # This is the trnsaction cost 


# The rebalancing is still to be checked

#########################################################This part calculates rebalancing



rbl_month_lag<-3                             # This determines hpw many days after months start we rebalance


rbl<-data.frame(stck_lvl$Date)               # This will calculate the rebalance date

rbl<-cbind(rbl,matrix(1,dim(rbl)[1],1))      # This is if rebalancing daily   
rbl<-cbind(rbl,matrix(1,dim(rbl)[1],1))      # This is if rebalancing monthly


colnames(rbl)<-c("Date","RBL")

int_mon<-data.frame(months.Date(rbl$Date))             # This will figure out months
rbl_mon<-matrix(0,dim(rbl)[1]-1,1)                     # This initializes rebalance days
rbl_mon[int_mon[-1,]!=int_mon[-dim(int_mon)[1],]]<-1   # This will set that up as 1
rbl_mon<-rbind(1,rbl_mon)                              # This will set up the first date as the rebalance date


if(rbl_month_lag>0){
  
  rbl_mon<-rbind(matrix(0,rbl_month_lag,1),rbl_mon)
  rbl_mon<-data.frame(rbl_mon[-((dim(rbl_mon)[1]-rbl_month_lag+1):(dim(rbl_mon)[1])),])
  
}

############################################################ Rebalancing calculation ends


############################################################ Factor inout, can be daily_monthly but in same order

fac_1<-data.frame(read.csv(paste(path_input,"factor.csv",sep="")))  # factor file input
#fac_1$FACTOR<-as.Date(fac_1$FACTOR,"%d-%m-%Y")

#fac_2<-data.frame(read.csv(paste(path_input,"factor.csv",sep="")))  # factor file input
#fac_2$FACTOR<-as.Date(fac_2$FACTOR,"%d-%m-%Y")


stapp<-data.frame(read.csv(paste(path_input,"Universe.csv",sep="")))    # This means that days applicable stocks
#stapp$UNIVERSE<-as.Date(fac_1$UNIVERSE,"%d-%m-%Y")

f_factor<-data.frame(fac_1)                                 # This will be the final clculate foactor to be used for ranking
f_factor<-f_factor[,-1]
f_factor[stapp[,-1]==0]<-NA



#rnk<-data.frame(dim(f_factor)[2]-rowSums(is.na(f_factor))-t(apply(f_factor, 1, rank,na.last="keep",ties.method="max"))+1)
rnk <- data.frame(t(apply(f_factor, 1, rank,na.last="keep",ties.method="max")))
fac1_rank <-rnk

#rnk<-data.frame(dim(f_factor)[2]-1+1-t(apply(f_factor[,-1], 1, rank,na.last="keep",ties.method="max")))    # This will give rank as in Excel
# this will rank and keep na for those not in the universe


clafic<-data.frame(read.csv(paste(path_input,"Sectors.csv",sep="")))[,-1]    # This means the sectors applicable stocks                   # This will differentiate between sectors or others


n<-max(clafic)                                # This signifies the numbers of sectors etc

fac_clas<-rnk             # Change this based on the desired factor
fac_clas<-f_factor   
###### Note To make it flwxible with dates


#wts_ini<-rbind(wts_ini,matrix(clafic))                # This will combine the weights and classification
# To be seen what it does


bbeta<-data.frame(read.csv(paste(path_input,"bbeta.csv",sep="")))            # Here input beta's for all the sectors
#bbeta<-rbind(bbeta,clafic)                    # Betas with classificationm (or sectors)
#wts_ini<-matrix(0,sum(clafic[1,]==i),dim(rnk)[1])            # Initialisation of wts

if(dsort ==1){
  
  fac_2<-data.frame(read.csv(paste(path_input,"factor.csv",sep=""))) 
  
  f_factor2<-data.frame(fac_2)                                 # This will be the final clculate foactor to be used for ranking
  f_factor2<-f_factor2[,-1]
  f_factor2[stapp[,-1]==0]<-NA
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
  
  
  fac2_rank_top2[is.na(fac2_rank_top2)] <-0
  fac2_rank_top2[fac2_rank_top2 >0]<-0.1
  fac2_rank_bot2[is.na(fac2_rank_bot2)] <-0
  fac2_rank_bot2[fac2_rank_bot2 >0]<--0.1

  
  colnames(fac2_rank_top2)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
  colnames(fac2_rank_bot2)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
  
  
  topname<-matrix(0,dim(fac2_rank_top2)[1],xnum)
  botname<-matrix(0,dim(fac2_rank_bot2)[1],xnum)
  
  
  
  
  for (i in 1:dim(fac2_rank_top2)[1]){
    
    topname[i,]<-colnames(fac2_rank_top2[i,fac2_rank_top2[i,]>0])
    botname[i,]<-colnames(fac2_rank_bot2[i,fac2_rank_bot2[i,]<0])   
  }

  
  wts_ini_top_calc<-fac2_rank_top2[-1*dim(fac2_rank_top2)[1],]
  wts_ini_bot_calc<-fac2_rank_bot2[-1*dim(fac2_rank_bot2)[1],]
  
  cst_tb<-data.frame(rowSums(abs(wts_int_full[-1,]-wts_int_full[-1*dim(wts_int_full),])*tc))
  
  stck_lvl_calc<-stck_lvl[-1,-1]
  
  lng_ret_tb<-data.frame(rowSums(wts_ini_top_calc*stck_lvl_calc))
  sht_ret_tb<-data.frame(rowSums(wts_ini_bot_calc*stck_lvl_calc))
  
  tot_ret_tb<-data.frame(((lng_ret_tb+sht_ret_tb)/2) - cst_tb)
  
  ret_fin_tb<-data.frame(cbind(stck_lvl$Returns[-1],tot_ret_tb,topnames[-1*dim(topnames)[1],],botnames[-1*dim(botnames)[1],]))
  
  write.csv(ret_fin_tb, file = "ret_fin_tb.csv") 
  
}



if(dsort!=1){


for (i in 1:n){
  
  wts_ini<-matrix(0,sum(clafic[1,]==i),dim(rnk)[1])            # Initialisation of wts
  
  #fac_clas_sel<-fac_clas[1,fac_clas[2,]==i]   # This will select the particular sector
  fac_clas_sel<-fac_clas[,clafic[1,]==i]   # This will select the particular sector
  
  
  #######################################       Internally rank amongst sector again
  #fac_clas_sel_1<-data.frame(dim(fac_clas_sel)[2]-1+1-t(apply(fac_clas_sel, 1, rank,na.last="keep",ties.method="max")))    # This will give rank as in Excel
  
  fac_clas_sel_1<-fac_clas_sel
  
   
  ###################################################################### internal ranking ends
  
  
  
  ################################ Dollar Neutral amongst sectors
  
  fac_clas_sel_2<-(fac_clas_sel_1-rowSums(fac_clas_sel_1,na.rm=TRUE)/(dim(fac_clas_sel_1)[2]-rowSums(is.na(fac_clas_sel_1))))/rowSums(abs(fac_clas_sel_1-rowSums(fac_clas_sel_1,na.rm=TRUE)/(dim(fac_clas_sel_1)[2]-rowSums(is.na(fac_clas_sel_1)))),na.rm=TRUE)
                                  
                                 # /(dim(fac_clas_sel_1)[2]-sum(is.na(fac_clas_sel_1))))
    # The above will make the factor =factor -mean for a particular sector
  #matrix(1,1,sum(clafic[1,]==i)-1)
  
  #rowSums(fac_clas_sel_1,na.rm=TRUE)
  
  fac_clas_sel_2[is.na(fac_clas_sel_2)]<-0    # This will give 0 zero wts to the ones whic are nt active
  # LNo need for this, remove this and change the below adjusted for NA
  
  wts_ini<-data.frame(fac_clas_sel_2)
    #t(matrix((fac_clas_sel_2*2)/rowsum(abs(fac_clas_sel_2))))  # This will set up the weights
  
  
  #wts_ini<-t(matrix((fac_clas_sel_2*2)/sum(abs(fac_clas_sel_2)))[clafic[1,]==i])  # This will set up the weights
  
  
  #wts_ini[clafic[1,]<-t(matrix((fac_clas_sel_2*2)/sum(abs(fac_clas_sel_2))))  # This will set up the weights
  # Need to check again        
  #wts_ini<-t(data.frame(wts_ini)) 
  
  wts_ini_top<-matrix(0,dim(wts_ini)[1],dim(wts_ini)[2])
  wts_ini_bot<-matrix(0,dim(wts_ini)[1],dim(wts_ini)[2])
  ############################################################### This is for weights equal top and bottom
#  if (topbot<-1){
    
    #top_bot <- matrix(0,dim(stapp)[1],dim(stapp)[2]-1)   
    #rnk1<- data.frame(rnk[apply(rnk,1,order)])
     
  
    
#  }
  

  
  if (topbot==1){  # This will select the top x and bottom x stocks and equaly weigh the same
    
    
  
    rnk_top<-data.frame(t(apply(wts_ini, 1, rank,na.last="keep",ties.method="max"))) 
    #(dim(fac_clas_sel_2)[2]+1-rowSums(is.na(fac_clas_sel_1)))-(data.frame(t(apply(wts_ini, 1, rank,na.last="keep",ties.method="max"))))
    
    
    
    rnk_bot<-data.frame(t(apply(wts_ini, 1, rank,na.last="keep",ties.method="max"))) 
  
    
    ############ The below will calculate the top and bottom ranks
    
    topminc<-data.frame((apply(rnk_top,1,max,na.rm=TRUE)))
    topminc[topminc<=xnum]<-xnum     # This is to make sure that the total number is not less than selected numbers
    
    topminc_cbnd <- topminc-xnum +1
    
    for (j in 2:dim(wts_ini)[2] ){
      topminc_cbnd<-cbind(topminc_cbnd,topminc-xnum +1)
            
    }
      
          
    #topminc[topminc>=xnumary]<-xnumary
    
    #topminc<-matrix(topminc,,dim(wts_ini_top)[2])
    
    wts_ini_top[rnk_top>=topminc_cbnd]<-1/(xnum)
    wts_ini_top<-data.frame(wts_ini_top)
    
    
    #1/(topminc)
    
    
    
    botminc<-data.frame((apply(rnk_bot,1,max,na.rm=TRUE)))
   
    botminc[botminc<=xnum]<-xnum
    
    botminc[]<-xnum
    
    botminc_cbnd <- botminc
    
    for (j in 2:dim(wts_ini)[2] ){
      botminc_cbnd<-cbind(botminc_cbnd,botminc)
      
    }
    
    
    #botminc[botminc>=xnumary]<-xnumary
    
    #topminc<-matrix(topminc,,dim(wts_ini_top)[2])
    
    wts_ini_bot[rnk_bot<=botminc_cbnd]<-(-1/(xnum))
    
    wts_ini_bot<-data.frame(wts_ini_bot)
    
    
    colnames(wts_ini_bot)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
    colnames(wts_ini_top)<-colnames(stck_lvl)[-1]  # This will assign thhe proper stock name
    
    
    topname<-matrix(0,dim(wts_ini_top)[1],xnum)
    botname<-matrix(0,dim(wts_ini_bot)[1],xnum)
    
    
    
    
    for (i in 1:dim(wts_ini_bot)[1] ){
      
      topname[i,]<-colnames(wts_ini_top[i,wts_ini_top[i,]>0])
      botname[i,]<-colnames(wts_ini_bot[i,wts_ini_bot[i,]<0])
      
      
      
    }
    
    
    
    ##########################################################################
    
    
    
      #min(tst2,matrix(xnum,dim(data.frame((apply(rnk_top,1,max,na.rm=TRUE))))[1],1))
  }
  
  
  ###############################################################
  
  
  #wts_sel_int<-wts_ini[wts_ini[2,]==i]     # This is the internal wts for calculation
  wts_int_full<-data.frame((wts_ini))
  
  
  ################################## Beta Neutral amongst sectors
  wts_int_pos_full<-wts_int_full
  wts_int_neg_full<-wts_int_full
  
  wts_int_pos_full[wts_int_full<=0]<-0
  wts_int_neg_full[wts_int_full>=0]<-0
  
  
  

  
  
  
  
  
  
  if (bna==1){        # This will check if beta neutral needs to be done or not
  
  brtio<-data.frame((rowSums(wts_sel_int_pos*bbeta[,clafic[1,]==i][,-1]))/abs(rowSums(wts_sel_int_neg*bbeta[,clafic[1,]==i][,-1])))
                                             # This will quantify the beta ratio between long and short in the portfolio
  
  wts_sel_int[wts_sel_int>=0]<-(wts_sel_int[wts_sel_int>=0]/brtio)  # This will change the weights of long to make it beta neutral
  
  }
  
  
  ###############################################################
  
            # This will supply beta neutral weights to the main weights table
  
  
  

  
  
  
  
  
}

##############################################################################################################################
wts_int_pos_full_calc<-wts_int_pos_full[-1*dim(wts_int_pos_full)[1],]             #Full universe factor returns
wts_int_neg_full_calc<-wts_int_neg_full[-1*dim(wts_int_neg_full)[1],]

cst_full<-data.frame(rowSums(abs(wts_int_full[-1,]-wts_int_full[-dim(wts_int_full),])*tc))

stck_lvl_calc<-stck_lvl[-1,-1]

lng_ret_full<-data.frame(rowSums(wts_int_pos_full_calc*stck_lvl_calc))
sht_ret_full<-data.frame(rowSums(wts_int_neg_full_calc*stck_lvl_calc))

tot_ret_full<-data.frame(((lng_ret_full+sht_ret_full)/2)-cst_full)

ret_fin_full<-data.frame(cbind(stck_lvl$Returns[-1],tot_ret_full,wts_int_full[-1dim(wts_int_full)[1],]))

write.csv(ret_fin_full, file = "ret_fin_full.csv")
################################################################################################################################

################################################################################################################################
wts_ini_top_calc<-wts_ini_top[-1*dim(wts_ini_top)[1],]
wts_ini_bot_calc<-wts_ini_bot[-1*dim(wts_ini_bot)[1],]
cst_tb<-data.frame(rowSums(abs(wts_int_full[-1,]-wts_int_full[-1*dim(wts_int_full),])*tc))

stck_lvl_calc<-stck_lvl[-1,-1]

lng_ret_tb<-data.frame(rowSums(wts_ini_top_calc*stck_lvl_calc))
sht_ret_tb<-data.frame(rowSums(wts_ini_bot_calc*stck_lvl_calc))

tot_ret_tb<-data.frame(((lng_ret_tb+sht_ret_tb)/2) - cst_tb)

ret_fin_tb<-data.frame(cbind(stck_lvl$Returns[-1],tot_ret_tb,topnames[-1*dim(topnames)[1],],botnames[-1*dim(botnames)[1],]))

write.csv(ret_fin_tb, file = "ret_fin_tb.csv")

}







##################################################################################
#This is for top and bottom analysus only

# top_stocks<-matrix(0,dim(top_stocks)[1],xnum)
# bot_stocks<-matrix(0,dim(bot_stocks)[1],xnum)
# 
# wts_top_bot_ana<-matrix(0,dim(fnl_wts)[1],dim(fnl_wts)[2])
# 
# for (kk in 1:dim(fnl_wts)[2]){
#   
#   top_stocks<-colNames(fnl_wts[kk,][>0])
#   bot_stocks<-colNames(fnl_wts[kk,][<0])
#   
#   wts_top_bot_ana[rank(fnl_wts,1,ties.method="max")<=xnum]<-(1/xnum)
#   wts_top_bot_ana[rank(fnl_wts,0,ties.method="min")<=xnum]<-(1/xnum)
#   
#   
# }
# 
# wts_top_bot_ana # This will give you the weights that will analyse the top and bottom stocks
###################################################################################################


#wts_sel_int%*%stck_lvl



####################################### Other Code Inputs
#rbl_calc_lag<-1                             # This determines how many days after rebalance date the return is of new portfolio, for e.g 2 means we buy and sell next day and weights are applicable after 2nd days






#######################################################################################################################################################################################################
# ################################################################################################################################
# wts_int_pos_full_calc<-wts_int_pos_full[-1*dim(wts_int_pos_full)[1],]             #Full universe factor returns
# wts_int_neg_full_calc<-wts_int_neg_full[-1*dim(wts_int_neg_full)[1],]
# 
# #wts_sel_int_pos_calc<-wts_sel_int_pos
# #wts_sel_int_neg_calc<-wts_sel_int_neg
# cst_full<-data.frame(rowSums(abs(wts_int_full[-1,]-wts_int_full[-dim(wts_int_full),])*tc))
# 
# stck_lvl_calc<-stck_lvl[-1,-1]
# 
# lng_ret_full<-data.frame(rowSums(wts_int_pos_full_calc*stck_lvl_calc))
# sht_ret_full<-data.frame(rowSums(wts_int_neg_full_calc*stck_lvl_calc))
# 
# tot_ret_full<-data.frame(((lng_ret_full+sht_ret_full)/2)-cst_full)
# 
# ret_fin_full<-data.frame(cbind(stck_lvl$Returns[-1],tot_ret_full))
# 
# write.csv(ret_fin_full, file = "ret2.csv")
# ################################################################################################################################
# 
# ################################################################################################################################
# wts_ini_top_calc<-wts_ini_top[-1*dim(wts_ini_top)[1],]
# wts_ini_bot_calc<-wts_ini_bot[-1*dim(wts_ini_bot)[1],]
# 
# #wts_sel_int_pos_calc<-wts_sel_int_pos
# #wts_sel_int_neg_calc<-wts_sel_int_neg
# cst_tb<-data.frame(rowSums(abs(wts_int_full[-1,]-wts_int_full[-dim(wts_int_full),])*tc))
# 
# stck_lvl_calc<-stck_lvl[-1,-1]
# 
# lng_ret_tb<-data.frame(rowSums(wts_ini_top_calc*stck_lvl_calc))
# sht_ret_tb<-data.frame(rowSums(wts_ini_bot_calc*stck_lvl_calc))
# 
# tot_ret_tb<-data.frame(((lng_ret_tb+sht_ret_tb)/2) - cst_tb)
# 
# ret_fin_tb<-data.frame(cbind(stck_lvl$Returns[-1],tot_ret_tb))
# 
# 
# ############################################################ Return calculation starts
# ret_ser<-data.frame(rowSums(rbind(as.matrix(stocks_wts[-1:-2,-1]),matrix(0,2,dim(stocks_wts)[2]-1)) * as.matrix(stck_lvl[,-1])))
# cst<-data.frame(abs(as.matrix(stocks_wts[-1,-1])-as.matrix(stocks_wts[-dim(stocks_wts)[1],-1])))*matrix(tc,1,dim(ret_ser)[2])
# cst<-data.frame(rbind(as.matrix(cst),matrix(0,1,dim(cst)[2])))
# ret_ser_fn<-colSums(ret_ser)-colSums(cst)
# 
# #######################################################################################
# 
