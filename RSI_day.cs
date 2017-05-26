using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class RSI_day : BasicStrategy
    {
        public object TradeStartTime = 9.5;
        public object TradeEndTime = 14.5;
        public object TradeSquareOff = 15.4;
        public object RSILength = 2;
        public object Thresh = 90.0;
        public object LONG_SMA = 200;
        public object SHORT_SMA = 5;
        
        public object PriceCutoff = 20;
        public object LC = 100;
        public object SC = 100;


        public RSI_day(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double thresh = Convert.ToDouble(Thresh);
            
            int tmaP = Convert.ToInt32(RSILength);
            int lbk1 = Convert.ToInt32(LONG_SMA);
            int lbk2 = Convert.ToInt32(SHORT_SMA);
            int lc = Convert.ToInt32(LC);
            int sc = Convert.ToInt32(SC);
            double pco = Convert.ToDouble(PriceCutoff);

            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime = DateTime.FromOADate(Convert.ToDouble(TradeEndTime) / 24.0).TimeOfDay;

            TimeSpan TrdSqOff = DateTime.FromOADate(Convert.ToDouble(TradeSquareOff) / 24.0).TimeOfDay;

            for (int i = 0; i < numSec; i++)
            {

                double[] ltp = data.InputData[i].Prices;
                                
                double[] sig = new double[ltp.Length];
                double[] np = new double[ltp.Length];

                List<double> Move1 = new List<double>();
                List<double> Move2 = new List<double>();
                List<double> Move3 = new List<double>();

                double[] series1 = new double[0];
                double[] newseries1 = new double[0];

                double[] series2 = new double[0];
                double[] newseries2 = new double[0];

                double[] series3 = new double[0];
                double[] newseries3 = new double[0];

                double up = 0.0000000001;
                double down = 0.000000001;
                double RS = 0;
                double RSI = 0;


                int longctr = 0;
                int shortctr = 0;
                int flag = 1;


                for (int j = 1; j < (ltp.Length - 1); j++)
                {
                   

                    
                    
                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j - 1].Date)
                    {
                        Move1.Add(ltp[j - 1]);
                        longctr = 0;
                        shortctr = 0;
                                                
                        
                       
                        if(Move1.Count()>tmaP && flag==0)
                       {
                           series1 = Move1.ToArray();
                           newseries1 = newseries1 = UF.GetRange(series1, series1.Length - tmaP, series1.Length - 1);

                           if (newseries1[tmaP - 1] > newseries1[tmaP - 2])
                               up = (up * (tmaP - 1) + (newseries1[tmaP - 1] - newseries1[tmaP - 2]))/tmaP;
                           else if (newseries1[tmaP - 1] < newseries1[tmaP - 2])
                               down = (down* (tmaP - 1) + (-newseries1[tmaP - 1] + newseries1[tmaP - 2])) / tmaP;
                           else if(newseries1[tmaP - 1] == newseries1[tmaP - 2])
                           {
                               up = up * (tmaP - 1) / tmaP;
                               down = down * (tmaP - 1) / tmaP;
                           }
                           RS = up / down;
                           RSI = 100 - 100 / (1 + RS);

                       }
                        
                        
                        
                        
                        if(Move1.Count()>tmaP && flag==1)
                        {

                            series1 = Move1.ToArray();
                            newseries1= UF.GetRange(series1, series1.Length - tmaP, series1.Length - 1);

                            for(int k=0; k<tmaP-1 ; k++)
                            {
                                if(newseries1[tmaP-1-k] > newseries1[tmaP-1-k-1])
                                {
                                    up = up+ newseries1[tmaP - 1 - k] - newseries1[tmaP - 1 - k - 1];
                                }

                                else if (newseries1[tmaP - 1 - k] < newseries1[tmaP - 1 - k - 1])
                                {
                                    down = down+ (-newseries1[tmaP - 1 - k] + newseries1[tmaP - 1 - k - 1]);
                                }

                            }

                            up = up / tmaP;
                            down = down / tmaP;
                            RS = up / down;
                            RSI = 100 - 100 / (1 + RS);
                            flag = 0;
                        }
                       
                        if(Move1.Count() > lbk1)
                        {
                            series2 = Move1.ToArray();
                            newseries2 = UF.GetRange(series2, series2.Length - lbk1, series2.Length - 1);


                        }

                        if (Move1.Count() > lbk2)
                        {
                            series3 = Move1.ToArray();
                            newseries3 = UF.GetRange(series3, series3.Length - lbk2, series3.Length - 1);
                        }
                     
                    }




                    if (Move1.Count() > lbk1 && Move1.Count() > lbk2 && Move1.Count() > tmaP)
                    {

                        if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime)
                        {
                            if (RSI >= thresh && ltp[j] < newseries2.Average() && np[j - 1] != -1 && shortctr <= sc)
                            {
                                sig[j] = -2;
                                np[j] = -1;
                                shortctr++;
                            }

                            if (RSI <= 100 - thresh && ltp[j] > newseries2.Average() && np[j - 1] != 1 && longctr <= lc)
                            {
                                sig[j] = 2;
                                np[j] = 1;
                                longctr++;
                            }
                        }
                    }


                    /*if ((np[j - 1] == 1 && ltp[j] > newseries3.Average()) || (np[j - 1] == -1 && ltp[j] < newseries3.Average()))
                    {
                        sig[j] = -np[j - 1];
                        np[j] = 0;
                    }*/

                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdSqOff && np[j - 1] != 0)
                    {
                        sig[j] = -np[j - 1];
                        np[j] = 0;
                    }

                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j + 1].Date)
                    {
                        sig[j] = -np[j - 1];
                        np[j] = 0;
                    }

                    if (sig[j] == 0)
                        np[j] = np[j - 1];

                }

                base.CalculateNetPosition(data, sig, i, 1.5, -1.5, -0.5, 0.5);

                //FileWrite opt1 = new FileWrite("bar.csv");
                //opt1.DataWriteOneVar(bar);
                //FileWrite opt2 = new FileWrite("uptick.csv");
                //opt2.DataWriteOneVar(uptick);
                //FileWrite opt3 = new FileWrite("downtick.csv");
                //opt3.DataWriteOneVar(downtick);
                //FileWrite opt4 = new FileWrite("RS.csv");
                //opt4.DataWriteOneVar(RS);
                //FileWrite opt5 = new FileWrite("RSI.csv");
                //opt5.DataWriteOneVar(RSI);
                //FileWrite opt6 = new FileWrite("sig.csv");
                //opt6.DataWriteOneVar(sig);
                //FileWrite opt7 = new FileWrite("np.csv");
                //opt7.DataWriteOneVar(np);
            }

            base.RunStrategyBase(data);

        }

    }
}