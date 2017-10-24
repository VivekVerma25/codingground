#include <Rcpp.h>
using namespace Rcpp;
using namespace std;
#include<iostream>
#include <fstream>
#include <sstream>
#include <string> 
#include <Rcpp.h>
#include <algorithm>   
#include <iostream>     // std::cout
#include <vector>       // std::vector
#include <list>

using namespace Rcpp;

//Rcpp::plugins(cpp11)]

// std::reverse(myvector.begin(),myvector.end());    // IMPORTANT REVERSE
//std::sort(S.begin(), S.end()); // IMPORTANT SORT
// csum=std::accumulate(S.begin(), S.end(),0.00); // IMPORTANT ADDS
//S.erase (S.begin(),S.begin()+0);/ REMOVES THE FIRST ELEMTN
//NumericVector z=clone(x);

// [[Rcpp::export]]

NumericVector   which_cpp(NumericVector x, double y){
  // std::reverse(myvector.begin(),myvector.end());    // IMPORTANT REVERSE
  //std::sort(S.begin(), S.end()); // IMPORTANT REVERSE
  // csum=std::accumulate(S.begin(), S.end(),0.00); // IMPORTANT REVERSE
  //NumericVector z=clone(x);
  
  // NumericVector x1=cumsum(x);//+cumsum(y);
  //NumericVector x2=cumsum(y);
  //x2=x1+x2;
  NumericVector res(0);
  
  int n = x.size();
  
  for( int i=0;i<n;i++){// run a loop to find the first entry
    
    if(x[i]==y) res.push_back(i);
    //Rcout << "res[j]=" <<res[j] << std::endl;
    
  }
  
  return res;
}



// [[Rcpp::export]]


double  maxvalue(NumericVector S,  int startp, int endp){
  
  
  //Rcout << "n=" <<n << std::endl;
  double  mvalue=S[startp];
  for( int i=startp;i<=endp;i++){//
    if(S[i]>mvalue){
      //Rcout << "i=" <<i << std::endl;
      //Rcout << "S[i]=" <<S[i] << std::endl;
      mvalue=S[i];
    }
  }
  
  
  
  return mvalue;
}

// [[Rcpp::export]]


double  minvalue(NumericVector S,  int startp, int endp){
  
  
  //Rcout << "n=" <<n << std::endl;
  double  mvalue=S[startp];
  for( int i=startp;i<=endp;i++){//
    if(S[i]<mvalue){
      //Rcout << "i=" <<i << std::endl;
      //Rcout << "S[i]=" <<S[i] << std::endl;
      mvalue=S[i];
    }
  }
  
  return mvalue;
}
// [[Rcpp::export]]

double  meanvalue(NumericVector S){
  
  double plb = S.size();
  double meanp = 0;
  for (int mi=0;mi<plb;mi++){         // This loop will calculate mean
    meanp=S[mi]+meanp;
    
  }
  meanp=(meanp/plb);  
  
  return meanp;
}  
// [[Rcpp::export]]
double  sdvalue(NumericVector S, double meanp){
  
  double std = 0;
  double plb =S.size();
  for (int mi=0;mi<plb;mi++){         // This loop will calculate standard Deviation
    std=(S[mi]-meanp)*(S[mi]-meanp)+std;
    
  }
  std=sqrt(std/(plb-1)); 
  
  return std;
}  


// [[Rcpp::export]]

double  sumvalue(NumericVector S){
  
  int n = S.size();
  
  double  mvalue=0;
  for( int i=0;i<n;i++){//
    mvalue=mvalue+S[i];
    
  }
  
  return mvalue;
}


// [[Rcpp::export]]

double  avgvalue(NumericVector S){
  
  double n = S.size();
  
  double  mvalue=0;
  for( int i=0;i<n;i++){//
    mvalue=mvalue+S[i];
    
  }
  mvalue=mvalue/n;
  return mvalue;
}


// [[Rcpp::export]]


NumericVector  increasevector(  NumericVector S , NumericVector x){
  int n = x.size();
  double y=0;
  //Rcout << "n=" <<n << std::endl;
  if(n>0){
    for( int i=0;i<n;i++){//
      y=x[i];
      S.push_back(y);
    }
  } 
  //if(n==1){
  //y=x;
  //S.push_back(x);
  return S;
}



// [[Rcpp::export]]


NumericVector  subselectvector(  NumericVector S , int startpoint, int endpoint){
  
  NumericVector Snew(0);
  double y=0;
  
  //Snew[0]=S[startpoint];
  for( int i=startpoint;i<= endpoint;i++){//
    y=S[i];
    Snew.push_back(y);
    
  }
  
  return Snew;
  
}

// [[Rcpp::export]]


NumericVector  subselectmatrix(  NumericMatrix S , int startpoint, int endpoint, int ccselect){
  //int	newlength=endpoint-startpoint;
  NumericVector Snew(0);
  double y=0;
  
  //Snew[0]=S[startpoint];
  for( int i=startpoint;i<= endpoint;i++){//
    y=S(i,ccselect);
    Snew.push_back(y);
    
  }
  
  return Snew;
  
}

// [[Rcpp::export]]


NumericMatrix  yearlytable(NumericMatrix Scpp, NumericVector ep_years){
  int n = ep_years.size();
  
  //	double s;
  
  int cntr=0;
  int startp=0;
  int endp=0;
  NumericMatrix resultmatrix(7,n-1);
  
  for( int i=0;i<n-1;i++){//
    double tv=0;
    double s=0;
    double minmtm=1000000000;
    double cummtm=0;
    double maxmtm=0;
    int notrades=0;
    startp=ep_years[i];
    endp=ep_years[i+1];
    NumericVector yearlymtm(endp-startp+100);
    //	Rcout << "i=" <<i << std::endl;
    //	Rcout << "startp=" <<startp << std::endl;
    //	Rcout << "endp=" <<endp << std::endl;
    //Rcout << "endp-startp+1=" <<endp-startp+1 << std::endl;
    int cntr1=0;
    for( int j=startp;j<endp;j++){//
      if(j>0){
        if(Scpp(j,2)==1 && Scpp(j-1,2)==0) notrades=notrades+1;
        if(Scpp(j,2)==-1 && Scpp(j-1,2)==0) notrades=notrades+1;
        if(Scpp(j,2)==1 && Scpp(j-1,2)==-1) notrades=notrades+1;
        if(Scpp(j,2)==-1 && Scpp(j-1,2)==1) notrades=notrades+1;
      }
      cummtm=cummtm+Scpp(j,0);
      tv=tv+Scpp(j,1);
      s=s+abs(Scpp(j,2));
      maxmtm=max(maxmtm,cummtm);
      minmtm=min(minmtm,cummtm-maxmtm);
      yearlymtm[cntr1]=Scpp(j,0);
      cntr1=cntr1+1;
      //if(i>0) break;
      //Rcout << "j=" <<j << std::endl;
      //Rcout << "cntr1=" <<cntr1 << std::endl;
      
    }
    //if(i>0) break;
    resultmatrix(0,cntr)=cummtm;//total return
    resultmatrix(1,cntr)=(sd(yearlymtm)*sqrt(250*75));//total return
    resultmatrix(2,cntr)=cummtm/(sd(yearlymtm)*sqrt(250*75));//total return
    resultmatrix(3,cntr)=(s/(endp-startp))*100;//total intime
    resultmatrix(4,cntr)=(cummtm/tv);//total mtm/tv
    resultmatrix(5,cntr)=minmtm;//total mtm/tv
    resultmatrix(6,cntr)=tv/2;//total mtm/tv
    cntr=cntr+1;
  }
  
  
  
  return resultmatrix;
}



// [[Rcpp::export]]
NumericVector ccretarrayabs(NumericVector p, NumericVector inc, int plb, int ptc){ // lb is lookback and tc is time cumulation
  int n = p.size();//size of data
  
  
  NumericVector pb(plb);        // This will initialise the price bucket
  
  double retsm=0;
  
  int k=0;
  int cnt=0;
  int sp=0;
  
  
  for( int j=0;j<(plb*ptc*2);j++){ // This loop will will calculate bucket of price
    
    if (inc(n-1-j)!=1 ){
      
      
      retsm=((p(n-1-j)/p(n-1-j-1))-1) +retsm;
      //Rcout << "rersm=" << retsm << std::endl;
      //Rcout << "k=" << retsm << std::endl;
      k=k+1;
      // This if will make sure that return are summed till time count
      
      
      if (k==ptc) {
        cnt=cnt+1;
        pb(plb-cnt)=abs(retsm);
        //Rcout << "rersm=" << retsm << std::endl;
        k=0;
        retsm=0;
        
        
      }
    }
    
    if (cnt==plb ) break;
  }
  
  
  return pb;
}


// [[Rcpp::export]]


NumericMatrix ctw2(NumericVector ltp,NumericVector ltph,NumericVector ltpl,
                   NumericVector lng_tk , NumericVector sht_tk , 
                   NumericVector dt_chng,int twapp,int lexttk, int sexttk,int lon, int sht, double tc,
                   int lookback_mean_sd_price, double factor_mean_sd_price){
  
  // ltp means the tick close data
  //ltph means ticks high
  // ltpl means tick long
  // lng_tk should be 1 for ticks you wanna enter long
  // sht_tk should be 1 for ticks you wanna enter short
  //dt_chng should be 1 for last tick of the day
  //twapp ... this will give no. of ticks for twap
  //lexttk.. no. of ticks after date change you wanna initiate  exit
  //sexttk.. no of ticks before date change you wann initiate twap exit
  //lon . when 1 means long entry possible
  // sht when 1 means hort entry possibe
  //tc is one side tran cost
  //lookback for calculating the mean and sd
  
  
  int n=ltp.size();

  int lbk=1;
  int lbk2=70;
  double rto_lvl=(0.5);
  double rto=0;
  double retsd=0;
  double retmean=0;
  double volsd=0;
  double volmean=0;
  double retsm=0;
  double retsm_num=0;
  double volsm=0;
  double volsm_num=0;
  
  
  
  double opn=0;  // this is todays open price
  double th=0;   // This is todays high price
  double tl=0;   // this is todays low price
  double yh=0;  // This is yesterdays high price 
  double yc=0;  // This is yesterdays close price 
  double yl=0;  // This is yesterdays low price 
  double twp_prc=0;
  double yo = 0; 
  double yc_metals=0;
  //double tsll=0.033;
  //double tsls=0.033;
  
  //double tgtl=0.05;
  //double stpl=0.01;
  // double tgts=0.1;
  //double stps=0.01;
  
  
  
  
  //Rcout << "tgtl first=" << (tgtl)<< std::endl;
  
  //bool entcond=false;
  //bool extcond=false;
  bool longentrycond=false;
  bool shortentrycond =false;
  bool longexitcond=false;
  bool shortexitcond=false;
  
  //double meanp=0;
  //double std=0;
  //double meanv=0;
  //double stdv=0;
  double ep=1; // this is initiated to 1 o avoid division by 0 errorss
  double tslnum=0;
  //double tslnum_p=0;
  double ep_p=0;
  double ibs_today = 0;
  //double facibs=0;
  //double facibs2=0;
  //double facopibs=0;
  
  //double cls_avg=0;
  //double lbk_h=0;
  //int k=0;
  //int l=0;
  //int cnt=0;
  //int cntv=0;
  int barsinceentry=0;
  int trdstdy=0;
  int dtcnt=0;
  //int lkback=36;
  double factor_change = 0;
  double price_change  = 0;
  std::vector<double> pb(20);      // This will initialise the price bucket
  std::vector<double> vb(20);      // This will initialise the price bucket
  
  
  NumericMatrix op(n,21);
  
  
  NumericVector fac1(n);
  NumericVector fac2(n);
  NumericVector fac3(n);
  NumericVector fac4(n);
  NumericVector fac5(n);
  NumericVector fac6(n);
  NumericVector dlyyestcls(n);
  NumericVector dlyyesthigh(n);
  NumericVector dlyyestlow(n);
  NumericVector dlyyestrange(n);
  NumericVector dlypricechange(n);
  NumericVector subvector_sel(n);
  NumericVector subvector_sel_price(n);
  NumericVector Gap_dir(n);
  NumericVector Day_dir(n);
  NumericVector volume_day(n);
  NumericVector lookback_mean(n);
  NumericVector lookback_sd(n);
  NumericVector lookback_mean_price(n);
  NumericVector lookback_sd_price(n);
  NumericVector mp(n);
  NumericVector ret(n);
  NumericVector twp_prc_lst(n); // This will calculate the twap price used for every tick
  
  for( int j=(100);j<n-(20);j++){
    twp_prc=0;
    for ( int ii=(1);ii<=twapp;ii++){
      
      twp_prc=twp_prc+(ltp(j+ii)+ltph(j+ii)+ltpl(j+ii))/3;
      
      // ltp(j+ii)
      
    }
    twp_prc=twp_prc/twapp;    // This will calculate the 
    twp_prc_lst(j)=twp_prc;
  }
  
  
  for( int j=(100);j<n-(20);j++){
    
    mp(j)=mp(j-1);

    if (dt_chng(j-1)==1){  // This will start once the date changes
      
      dtcnt=dtcnt+1;
      volume_day(dtcnt) = volsm; 
      dlypricechange(dtcnt) = price_change;
      volsm = 0;
      price_change = 0;
      trdstdy=0;
      yh=th;  // This is yesterdays high price 
      yc=ltp(j-1);  // This is yesterdays close price 
      yl=tl;  // This is yesterdays low price
      yo = opn;
      opn=ltp(j);  // this is todays open price
      th=ltph(j);   // This is todays high price
      tl=ltpl(j);   // this is todays low price
   
      dlyyestcls(dtcnt)= yc;
      dlyyesthigh(dtcnt) = yh;
      dlyyestlow(dtcnt) = yl;
      
    if((dtcnt-lookback_mean_sd_price)<=0)
      {
        subvector_sel_price = subselectvector(dlyyestcls,0,dtcnt);
      }
      else
      {
        subvector_sel_price = subselectvector(dlyyestcls,dtcnt-lookback_mean_sd_price,dtcnt);
      }
      
      lookback_mean_price(dtcnt) = meanvalue(subvector_sel_price);
      lookback_sd_price(dtcnt) = sdvalue(subvector_sel_price,lookback_mean_price(dtcnt));
      
    }

      price_change = (ltp(j)-opn)*100/opn;

      th=max(th,ltph(j));
      tl=min(tl,ltpl(j));

    longentrycond = (ltp(j))>(lookback_mean_price(dtcnt)+ factor_mean_sd_price*lookback_sd_price(dtcnt)) && 
      lng_tk(j)==1 && lon==1 && trdstdy<=1;
    
    shortentrycond= (ltp(j))<(lookback_mean_price(dtcnt) - factor_mean_sd_price*lookback_sd_price(dtcnt))
    && sht_tk(j)==1 && sht==1 && trdstdy<=1;  
    
    if ((longentrycond))
    { 
      mp(j)=1;
    }
    if ( (shortentrycond)){ 
      mp(j)=(-1);
        }
    
    if(mp(j)!=0 && mp(j)!=mp(j-1)){
      
      trdstdy=trdstdy+1;
      
    }
    
   ep_p=ep;    // This wil lextend the entry price
    
     if (mp(j)!=mp(j-1)) {
      
      barsinceentry=0;
      ep=twp_prc_lst(j); // This will calculate entry price
      
    }
    
    
    
    if (mp(j)!=0 && mp(j)==mp(j-1)) barsinceentry=barsinceentry+1;
    
    // Entry ends
    
    // Trailing Stop Loss calcultion
    
    //tslnum_p=tslnum; // This will give the previous ones its level
    
    if(mp(j)!=mp(j-1)){ // If there is a change in the mp
      
      if (mp(j)==1){
        tslnum=max(ep,ltp(j));
      }
      
      
      if (mp(j)==(-1)){
        tslnum=min(ep,ltp(j));
      }
      
      
    }
    
    
    if(mp(j)==mp(j-1)){ // If there is no change in the mp
      
      if (mp(j)==1){
        tslnum=max(tslnum,ltp(j));
      }
      
      
      if (mp(j)==(-1)){
        tslnum=min(tslnum,ltp(j));
      }
      
      
    }

    longexitcond=((mp(j-1)==(1)) && dt_chng(j+lexttk)==1) ;
//    longexitcond=((mp(j-1)==(1)) && ltp(j)<=0.97*tslnum) ;
    
    // shortexitcond=((mp(j-1)==(-1)) && dt_chng(j+sexttk)==1) ;
    shortexitcond=((mp(j-1)==(-1)) && dt_chng(j+lexttk)==1) ;
    
    if (longexitcond && !shortentrycond) mp(j)=0;  // This is only time based entry
    if (shortexitcond && !longentrycond) mp(j)=0;

    ret(j)=((twp_prc_lst(j)-twp_prc_lst(j-1))/ep_p)*(mp(j-1))-abs(mp(j)-mp(j-1))*tc;    // This will encapsulate all conditions for entry and exits etc etc etc
    
    // This will provide the market position and returns to the output file op
    
    op(j,0)=mp(j);  
    op(j,1)=ret(j);
    op(j,2) = twp_prc_lst(j);
    op(j,3) = dt_chng(j);
    op(j,4) = yh;
    op(j,5) = yl;
    op(j,6) = yc;
    op(j,7) = opn;
    op(j,8) = th;
    op(j,9) = tl;
    op(j,10) = lookback_mean_price(dtcnt);
    op(j,11) = lookback_sd_price(dtcnt);
    op(j,12) = longentrycond;
    op(j,13) = shortentrycond;
    op(j,14)=tslnum;
    op(j,15)=ep;
    op(j,16)=trdstdy;
    op(j,17)= volsm;
    op(j,18)=  dlypricechange(dtcnt);
    op(j,19) = price_change;
    op(j,20) = shortentrycond;
    //Rcout << "res[j]=" <<res[j] << std::endl;
}    
    return op;

}