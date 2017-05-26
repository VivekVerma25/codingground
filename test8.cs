using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class test8 : BasicStrategy
    {
        public object TradeStartTime = 9.5;
        public object TradeEndTime = 12;
        public object TradeSquareOffTime = 17.5;
        public object RefStartTime = 9;
        public object RefEndTime = 17.75;
        public object Lookback = 90;
        public object Lookback2 = 5;
        public object Lookback3 = 12;
        //public object Lookback3 = 3;
        //public object Mult = 2.5;
        //public object EC = 0.9;
        public object Mode = "A"; //"A", "L", "S"
        //public object SigmaDifferenceL = 0.4;
        //public object SigmaDifferenceS = 0.4;
        public object SigmaLevel1 = 0;
        public object SigmaLevel2 = 0;
        public object ExitTime1 = 1000;
        public object ExitTime2 = 1000;
        public object LongCount = 1;
        public object ShortCount = 1;
        public object NoTrade = 0;
        //public object returns = 0.000;
        

        public test8(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            int lbk = Convert.ToInt32(Lookback);
            int lbk2 = Convert.ToInt32(Lookback2);
            int lbk3 = Convert.ToInt32(Lookback3);
            //double mult = Convert.ToDouble(Mult);
            //double ec = Convert.ToDouble(EC);
            string mode = Convert.ToString(Mode);
            // double sigdiffL = Convert.ToDouble(SigmaDifferenceL);
            //double sigdiffS = Convert.ToDouble(SigmaDifferenceS);
            double siglevel1 = Convert.ToDouble(SigmaLevel1);
            double siglevel2 = Convert.ToDouble(SigmaLevel2);
            double et1 = Convert.ToDouble(ExitTime1);
            double et2 = Convert.ToDouble(ExitTime2);
            //double ret = Convert.ToDouble(returns);
            int LC = Convert.ToInt32(LongCount);
            int NT = Convert.ToInt32(NoTrade);
            int SC = Convert.ToInt32(ShortCount);

            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime = DateTime.FromOADate(Convert.ToDouble(TradeEndTime) / 24.0).TimeOfDay;
            TimeSpan TrdSquareOffTime = DateTime.FromOADate(Convert.ToDouble(TradeSquareOffTime) / 24.0).TimeOfDay;


            TimeSpan DataRefStartTime = DateTime.FromOADate(Convert.ToDouble(RefStartTime) / 24.0).TimeOfDay;
            TimeSpan DataRefEndTime = DateTime.FromOADate(Convert.ToDouble(RefEndTime) / 24.0).TimeOfDay;

            //double metric1 = 9999999999; 
            //double metric2 = 9999999999;


            for (int i = 0; i < numSec; i++)
            {
                int len = data.InputData[i].Dates.Length;
                double[] ltp_stock = data.InputData[i].Prices;
                double[] ltp_sec = data.InputData[i].Extra1;

                double[] sig = new double[len];
                double[] np = new double[len];

                //List<double> Move1 = new List<double>();
                //List<double> Move2 = new List<double>();

                double[] Zscore1 = new double[len];
                double[] Zscore2 = new double[len];
                double[] TIT = new double[len];

                double[] series1 = new double[0];
                double[] series2 = new double[0];

                double[] newseries1 = new double[0];
                double[] newseries2 = new double[0];

                double avg1 = 0;
                double avg2 = 0;
                double std1 = 0;
                double std2 = 0;
                int z1_max_i = 0;
                int z2_max_i = 0;
                int flagS = 0;
                int flagL = 0;
                int zzz = 0;
                double lw = 0;
                double hi = 0;
                //double longlevel = -999999999;
                //double shortlevel = 999999999;

                //double high = 999999999;
                //double low = -999999999;

                int longtrades = 0;
                int shorttrades = 0;
                int timeintradeS = 0;
                int timeintradeL = 0;

                List<double> close = new List<double>();
                List<double> open = new List<double>();
                List<double> ret = new List<double>();
                List<double> low = new List<double>();
                List<double> high = new List<double>();

                //double entryflag = 0;
                int tt=0;
                for (int timestep = 1; timestep < (len); timestep++)
                {

                    if (np[timestep - 1] == 1)
                        timeintradeL++;

                    if (np[timestep - 1] == -1)
                        timeintradeS++;

                    if (data.InputData[i].Dates[timestep].Date != data.InputData[i].Dates[timestep - 1].Date)
                    {
                        //entryflag = 0;

                        tt=0;
                        if (low.Count() > 0)
                            lw = low.Min();
                        if (high.Count() > 0)
                            hi = high.Max();



                        if (close.Count() > 0)
                        {
                            ret.Add(Math.Abs(Math.Log(ltp_stock[timestep - 1] / close.Last())));

                        }


                        open.Add(ltp_stock[timestep]);
                        close.Add(ltp_stock[timestep - 1]);
                        low = new List<double>();
                        low.Add(ltp_stock[timestep]);

                        high = new List<double>();
                        high.Add(ltp_stock[timestep]);

                        flagS = 0;
                        flagL = 0;
                        timeintradeS = 0;
                        timeintradeL = 0;
                        shorttrades = 0;
                        longtrades = 0;


                        if (ret.Count() > lbk && close.Count() > lbk2)
                        {
                            series1 = ret.ToArray();
                            newseries1 = UF.GetRange(series1, series1.Length - lbk, series1.Length - 1);

                            series2 = close.ToArray();
                            newseries2 = UF.GetRange(series2, series2.Length - lbk2, series2.Length - 1);

                            avg1 = newseries1.Average();
                            avg2 = newseries2.Average();
                            std1 = UF.StandardDeviation(newseries1);
                            //std2 = UF.StandardDeviation(newseries2);
                            
                        }

                    }

                    if (series1.Length > lbk && data.InputData[i].Dates[timestep].Date == data.InputData[i].Dates[timestep - 1].Date)
                    {
                        low.Add(ltp_stock[timestep]);
                        high.Add(ltp_stock[timestep]);
                        

                        if (data.InputData[i].Dates[timestep].TimeOfDay >= DataRefStartTime && data.InputData[i].Dates[timestep].TimeOfDay <= DataRefEndTime)
                        {

                            if (series2.Length > lbk2)
                            {



                                if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[timestep].TimeOfDay < TrdEntryEndTime)
                                {
                                    tt++;
                                    /*if (entryflag == 0)
                                    {
                                        open.Add(ltp_stock[timestep]);
                                        entryflag++;
                                    }*/

                                    
                                    
                                    
                                        double z1 = (Math.Abs(Math.Log(ltp_stock[timestep] / lw)) - avg1) / std1;
                                        double z2 = (Math.Abs(Math.Log(ltp_stock[timestep] / hi)) - avg1) / std1;


                                        Zscore1[timestep] = z1;
                                        Zscore2[timestep] = z2;

                                    
                                    if (z1 >= siglevel1 && np[timestep - 1] != -1 && (mode == "A" || mode == "S") && shorttrades < SC && flagS <= 0 && ltp_stock[timestep] > 20.0 && tt<lbk3)
                                    {

                                        if (ltp_stock[timestep] < avg2)
                                        {

                                            sig[timestep] = -2;
                                            np[timestep] = -1;
                                            timeintradeS = 0;
                                            shorttrades++;

                                        }

                                    }




                                    if (z2 >= siglevel2 && np[timestep - 1] != 1 && (mode == "A" || mode == "L") && longtrades < LC && flagL <= 0 && ltp_stock[timestep] > 20.0 && tt<lbk3)
                                    {

                                        if (ltp_stock[timestep] > avg2)
                                        {

                                            sig[timestep] = +2;
                                            np[timestep] = +1;
                                            timeintradeL = 0;
                                            longtrades++;

                                        }

                                    }



                                    /*if (z1[z1_max_i] >= siglevel1 && np[timestep - 1] != -1 && metric <= sigdiffS && (mode == "A" || mode == "S") && shorttrades < SC && flag <= 0)
                                    {
                                        sig[timestep] = -2;
                                        np[timestep] = -1;
                                        timeintrade = 0;
                                        shorttrades++;
                                    }*/




                                }
                            }

                        }
                    }

                    //TIT[timestep] = timeintrade;
                    if (timeintradeS >= et1 && np[timestep - 1] == -1)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        timeintradeS = 0;
                    }


                    if (timeintradeL >= et2 && np[timestep - 1] == +1)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        timeintradeL = 0;
                    }


                    if (np[timestep] == 0 && np[timestep - 1] == 1)
                    {
                        flagL = NT;
                    }
                    else
                        flagL--;

                    if (np[timestep] == 0 && np[timestep - 1] == -1)
                    {
                        flagS = NT;
                    }
                    else
                        flagS--;






                    if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdSquareOffTime && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        timeintradeL = 0;
                        timeintradeS = 0;
                    }

                    if (sig[timestep] == 0)
                        np[timestep] = np[timestep - 1];



                }


                /*FileWrite opt1 = new FileWrite("sig.csv");
                opt1.DataWriteOneVar(sig);
                FileWrite opt2 = new FileWrite("np.csv");
                opt2.DataWriteOneVar(np);
                FileWrite opt5 = new FileWrite("TimeInTrade.csv");
                opt5.DataWriteOneVar(TIT);

                FileWrite opt3 = new FileWrite("zcore1.csv");
                opt3.DataWriteOneVar(Zscore1);
                FileWrite opt4 = new FileWrite("zcore2.csv");
                opt4.DataWriteOneVar(Zscore2);*/

                base.CalculateNetPosition(data, sig, i, 1.5, -1.5, -0.5, 0.5);
            }

            base.RunStrategyBase(data);
        }
    }
}

