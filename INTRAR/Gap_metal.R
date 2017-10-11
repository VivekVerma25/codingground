setwd("D:/Project/Excercise10_Gap_Strategy/NewExcercises_ALL/Metal_Prices")
library(xts)
library(zoo)
library(TTR)
library(data.table)
library("Rcpp")
library("data.table")
source("../InputFiles_ALL/R_Func_1.R")
sourceCpp("TWAP_metal.cpp")

case = c("NewData_PriceOnly_to_230930metalprices_retu")
#tst<- match(colnames(dta_stk_close),colnames(Volume))
#sel_Volume <- data.frame(Volume[,na.omit(tst)])
#tst1 <- match(colnames(sel_Volume),colnames(dta_stk_close))
#sel_dta_stk_close <- data.frame(dta_stk_close[,na.omit(tst1)])

dta_stk_close <-data.frame(read.csv("../InputFiles_ALL/Close_data_final_metals.csv"),check.names=FALSE)
dta_stk_close$Date<-as.POSIXct(paste(dta_stk_close$Date),"%Y-%m-%d %H:%M:%S",tz="GMT")

#dta_stk_close <- dta_stk_close[,c("Date","HINDALCO","HINDZINC","JSWSTEEL","JINDALSTEL","TATASTEEL"
#                                  ,"VEDL")]
dta_stk_high <- copy(dta_stk_close)
dta_stk_low <- copy(dta_stk_close)

Metals_Data <-data.frame(read.csv("Metals_Play_comm_data.csv"),check.names=FALSE)
Metals_Data$Date<-as.POSIXct(Metals_Data$Date,"%m/%d/%Y",tz="GMT")

common_date = merge(dta_stk_close[1],dta_stk_high[1],by=c("Date"))
common_date = merge(common_date,dta_stk_low[1],by=c("Date"))

dta_stk_close = merge(dta_stk_close,common_date,by=c("Date"))
dta_stk_high = merge(dta_stk_high,common_date,by=c("Date"))
dta_stk_low = merge(dta_stk_low,common_date,by=c("Date"))

common_date_only_date <- data.frame(unique(strftime(common_date$Date, format="%Y-%m-%d")))
colnames(common_date_only_date) <- c("Date")
common_date_only_date$Date<-as.POSIXct(common_date_only_date$Date,"%Y-%m-%d",tz="GMT")

Metals_Data <- merge(common_date_only_date,Metals_Data,by=c("Date"),all.x=T)
Metals_Data <- na.locf(Metals_Data)
Metals_Data <- na.locf(Metals_Data,fromLast = TRUE)

common_date$Date1 <- strftime(common_date$Date, format="%Y-%m-%d")
common_date = merge(common_date,Metals_Data,by.x=c("Date1"),by.y=c("Date"))

Metals_Data_final <- copy(common_date)
Metals_Data_final$Date1 <- NULL

############################################
results_op<-NULL
dt_chng<-as.matrix(endpoints(dta_stk_close$Date,on="days"))      # This will give the last rows of every day
dt_time=matrix(strftime(dta_stk_close$Date,"%H:%M:%OS",tz="GMT")) 

rslta<-ctw2(as.numeric(dta_stk_close[,9]),as.numeric(dta_stk_high[,9]),as.numeric(dta_stk_low[,9]),
            as.numeric(dt_mid_tilltime),as.numeric(lng_tk), as.numeric(sht_tk),as.numeric(dt_chng_tick),twapp,lexttk,
            sexttk,lonn,shtt,(0.0/100),as.numeric(lookback_mean_sd_price),as.numeric(factor_mean_sd_price))
rslta <- cbind(as.numeric(dta_stk_close[,9]),as.numeric(dta_stk_high[,9]),as.numeric(dta_stk_low[,9]),rslta)
date_tod <- data.table(dta_stk_close[,1])
rslta_dt <- data.table(rslta)
rslta_datetime <- cbind(date_tod,rslta_dt)
colnames(rslta_datetime) <- c("DateTime","Close","High","Low","MP","Ret","TWAP","Date_Chnage","Yest_high",
                              "Yest_Low","Yest_Close","Open","Today_hgh","Today_Low","Lookback_Mean","Lookback_SD",
                              "LongEntry_Cond","ShortEntry_Cond","TSLNUM","Entry_Price",
                              "Trsdty","volsum","Dailypricechan","price_change","TimeTillVol")
write.csv(rslta_datetime,"temporary_volume_new2.csv",row.names=F)

twapp<-10  # this is the ticks for twap

lookbackmeansdprice = c(10)
factormeansdprice = c(1.5)

final.datetime=strptime(as.character("1/2/2006 14:25:00"),"%m/%d/%Y %H:%M:%OS")
final.time=strftime(final.datetime,"%H:%M:%OS")

final.datetime1=strptime(as.character("1/2/2006 13:25:00"),"%m/%d/%Y %H:%M:%OS")
final.time1=strftime(final.datetime1,"%H:%M:%OS")
timeofday = c(final.time,final.time1)

lon = c(1)
sht = c(1)
Final_Results <- NULL

time_of_day = c(final.time1)
lonn <- c(1)
shtt <- c(1)
lookback_mean_sd_price <- c(10)
factor_mean_sd_price <- c(1.5)

for(time_of_day in timeofday)
{ 
  lng_tk<-matrix(0,dim(dt_time)[1]) # Initilising long ticks
  sht_tk<-matrix(0,dim(dt_time)[1]) # Initilising short ticks
  dt_chng_tick<-matrix(0,dim(dt_time)[1]) # Initilising short ticks
  
  if(time_of_day==final.time)
  { 
    lexttk<-4  # No. of ticks after day end . exit cond for long is true ... then twap will be initiated
    sexttk<-4  # This is no. of ticks before day end .. exit cond is true for short .. then twap will be initiated
  }
  if(time_of_day==final.time1)
  {
    lexttk<-16  # No. of ticks after day end . exit cond for long is true ... then twap will be initiated
    sexttk<-4  # This is no. of ticks before day end .. exit cond is true for short .. then twap will be initiated
    
  }
  dt_mid_time<-matrix(0,dim(dt_time)[1])  # This will be 1 when the time is as above for e.g 12:00
  dt_mid_time[dt_time==time_of_day]<-1
  
  dt_mid_tilltime<-matrix(0,dim(dt_time)[1]) 
  dt_mid_tilltime[dt_time<=time_of_day]<-1
  
  for ( i in 20:(dim(dt_time)[1]-20) ){
    
    #lng_tk[i]<-lng_tk[i-1]
    #sht_tk[i]<-sht_tk[i-1]
    
    if (!is.na(match(i+3,dt_chng))){
      lng_tk[i]<-0
      sht_tk[i]<-0
      
    }
    
    if (!is.na(match(i-1,dt_chng))){
      lng_tk[i]<-0
      sht_tk[i]<-0
      
    }
    
    if (!is.na(match(i-9,dt_chng))){
      lng_tk[i]<-0
      sht_tk[i]<-0
      
    }
    
    if (dt_mid_time[i]==1){
      lng_tk[i]<-1
      sht_tk[i]<-1
      
      
    }
    
    if (!is.na(match(i,dt_chng))){
      dt_chng_tick[i]<-1
      
    }
    
  }
for(lonn in lon)
{
  for(shtt in sht)
  {
    if(lonn==1 | shtt==1)
    {
    for(lookback_mean_sd_price in lookbackmeansdprice)
    {
      for(factor_mean_sd_price in factormeansdprice)
      {
      for ( j in 2:ncol(dta_stk_close)){
  #(NumericVector ltp,NumericVector ltph,NumericVector ltpl,NumericVector lng_tk , 
  #  NumericVector sht_tk , NumericVector dt_chng,int twapp, double tc)
  #rslta<-ctw2(as.numeric(dta_stk_close[,j]),as.numeric(dta_stk_high[,j]),as.numeric(dta_stk_low[,j]),as.numeric(lng_tk), as.numeric(sht_tk),as.numeric(dt_chng_tick),twapp,lexttk,sexttk,lon,sht,(0.0/100))
  rslta<-ctw2(as.numeric(dta_stk_close[,j]),as.numeric(dta_stk_high[,j]),as.numeric(dta_stk_low[,j]),
              as.numeric(dt_mid_tilltime),as.numeric(lng_tk), as.numeric(sht_tk),as.numeric(dt_chng_tick),twapp,lexttk,
              sexttk,lonn,shtt,(0.0/100),as.numeric(lookback_mean_sd_price),as.numeric(factor_mean_sd_price),as.numeric(Metals_Data_final$Close))
  
  #rslta <- cbind(as.numeric(dta_stk_close[,j]),as.numeric(dta_stk_high[,j]),as.numeric(dta_stk_low[,j]),rslta)
  rslta<-data.frame(rslta)
  #plot(cumsum(rslta[,2]))
  
  tv<-abs(rslta[-1,1]-rslta[-dim(rslta)[1],1])
  tv<-data.frame(tv)
  tv<-rbind(0,tv)
  tv<-data.frame(tv)
  
  if (j==2){
    ret_ser<-data.frame(rslta[,2])
    tv_ser<-tv
    
  }else{
    ret_ser<-data.frame(cbind(ret_ser,data.frame(rslta[,2])))
    tv_ser<-data.frame(cbind(tv_ser,tv)) 
    
  }
  print(j)
  
}

#ret_ser[abs(ret_ser)>=0.4]<-0  # This will adjust for splits etc

tv_ser<-data.frame(cbind((dta_stk_close$Date),tv_ser))
ret_ser<-data.frame(cbind((dta_stk_close$Date),data.frame(ret_ser)))

colnames(tv_ser)[1]<-"Date"
colnames(ret_ser)[1]<-"Date"

#dly_retn<-data.frame(aggregate(ret_ser,by=list((substr(ret_ser$Date,1,10))),sum))

dly_tv<-data.frame(aggregate(tv_ser[,2:dim(tv_ser)[2]],by=list((substr(tv_ser$Date,1,10))),sum))
dly_retn<-data.frame(aggregate(ret_ser[,2:dim(ret_ser)[2]],by=list((substr(ret_ser$Date,1,10))),sum))

#dly_retn[(dly_retn[,-1])>=0.4,-1]<-0

dly_retn<-data.frame(dly_retn)
setnames(dly_retn,"Group.1","Date")
setnames(dly_tv,"Group.1","Date")
colnames(dly_tv)[2:ncol(dly_tv)]<-colnames(dta_stk_close)[2:ncol(dly_tv)]
colnames(dly_retn)[2:ncol(dly_retn)]<-colnames(dta_stk_close)[2:ncol(dly_tv)]


dly_retn_date <- data.frame(dly_retn[1]$Date)
colnames(dly_retn_date) <- c("Date")
dly_retn_date$Date<-as.POSIXct(paste(dly_retn_date$Date),"%Y-%m-%d",tz="GMT")

all_liq_ret<-(dly_retn[,-1])/6
all_liq_tv<-(dly_tv[,-1])/6
#Gap_ret<-Gap_ret[-1:-66,]
#Gap_tv<-Gap_tv[-1:-66,]

all_liq_ret_ac<-all_liq_ret-all_liq_tv*(0.005/100)

all_perf <- NULL
dtrettv<-data.frame(cbind(data.frame(dly_retn[,1]),data.frame(rowSums(all_liq_ret_ac)),data.frame(rowSums(all_liq_tv))))
all_liq_perf<-data.table(ann_rslt(dtrettv),keep.rownames = TRUE)
all_liq_perf[,Type:="All_Liquid"]
all_perf = rbind(all_liq_perf)
all_perf[,Case:=case]
all_perf[,lookback_price:=lookback_mean_sd_price]
all_perf[,factor_price:=factor_mean_sd_price]
all_perf[,long:=lonn]
all_perf[,short:=shtt]
all_perf[,timeofday:=time_of_day]
Final_Results <- rbind(Final_Results,all_perf)
write.csv(Final_Results,paste0("Results/",case,"_Final_Results.csv"))

      }
    }
  }
}
    }
}
