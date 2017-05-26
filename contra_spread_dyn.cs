using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class contra_spread_dyn : BasicStrategy
    {
        public object TradeStartTime = 9.75;
        public object TradeEndTime = 14.75;
        public object TradeSquareOffTime = 15;
        public object RefStartTime = 9.75;
        public object RefEndTime = 15;
        public object Lookback = 25;
        public object Lookback2 = 1500;
        public object Lookback3 = 1500;
        //public object Percentile = 0.1;
        //public object Mult = 2.5;
        //public object EC = 0.9;
        public object Mode = "L"; //"A", "L", "S"
        //public object SigmaRatio = 0.4;

        public object SigmaLevel1 = 2;
        public object SigmaLevel2 = 2;
        public object ExitTime1 = 15;
        public object ExitTime2 = 30;
        //public object LongCount = 1000;
        //public object ShortCount = 1000;
        public object NoTrade = 15;
        //public object returns = 0.000;

        public contra_spread_dyn(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            int lbk = Convert.ToInt32(Lookback);
            int lbk2 = Convert.ToInt32(Lookback2);
            int lbk3 = Convert.ToInt32(Lookback3);
            //double per = Convert.ToDouble(Percentile);
            //double mult = Convert.ToDouble(Mult);
            //double ec = Convert.ToDouble(EC);
            string mode = Convert.ToString(Mode);
            //double sigdiffL = Convert.ToDouble(SigmaRatio);
            //double sigdiffS = Convert.ToDouble(SigmaDifferenceS);
            double siglevel1 = Convert.ToDouble(SigmaLevel1);
            double siglevel2 = Convert.ToDouble(SigmaLevel2);
            double et1 = Convert.ToDouble(ExitTime1);
            double et2 = Convert.ToDouble(ExitTime2);
            //double ret = Convert.ToDouble(returns);
            //int LC = Convert.ToInt32(LongCount);
            int NT = Convert.ToInt32(NoTrade);
            //int SC = Convert.ToInt32(ShortCount);

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

                List<double> Move1 = new List<double>();
                List<double> Move2 = new List<double>();
                //List<double> sp = new List<double>();



                double[] Zscore1 = new double[len];
                double[] Zscore2 = new double[len];
                double[] TIT = new double[len];

                double[] series1 = new double[0];
                double[] series2 = new double[0];
                //double[] series3 = new double[0];

                double[] newseries1 = new double[0];
                double[] newseries2 = new double[0];
                //double[] newseries3 = new double[0];

                double avg1 = 0;
                double avg2 = 0;
                double std1 = 0;
                double std2 = 0;
                int z1_max_i = 0;
                int z2_max_i = 0;
                int flag = 0;
                int zzz = 0;
                double xx = 0;
                //double longlevel = -999999999;
                //double shortlevel = 999999999;

                //double high = 999999999;
                //double low = -999999999;

                int longtrades = 0;
                //int shorttrades = 0;
                int timeintrade = 0;

                for (int timestep = 1; timestep < (len); timestep++)
                {

                    if (np[timestep - 1] != 0)
                        timeintrade++;

                    if (data.InputData[i].Dates[timestep].Date != data.InputData[i].Dates[timestep - 1].Date)
                    {
                        flag = 0;
                        timeintrade = 0;
                        //shorttrades = 0;
                        longtrades = 0;
                        if (Move1.Count() > lbk2)
                        {
                            series1 = Move1.ToArray();
                            newseries1 = UF.GetRange(series1, series1.Length - lbk2, series1.Length - 1);

                            //series2 = Move2.ToArray();
                            //newseries2 = UF.GetRange(series2, series2.Length - lbk2, series2.Length - 1);

                            avg1 = newseries1.Average();
                            //avg2 = newseries2.Average();
                            std1 = UF.StandardDeviation(newseries1);
                            //std2 = UF.StandardDeviation(newseries2);

                        }


                        /* if (Move2.Count() > lbk3)
                         {
                             //series1 = Move1.ToArray();
                             //newseries1 = UF.GetRange(series1, series1.Length - lbk2, series1.Length - 1);

                             series2 = Move2.ToArray();
                             newseries2 = UF.GetRange(series2, series2.Length - lbk3, series2.Length - 1);

                             //avg1 = newseries1.Average();
                             avg2 = newseries2.Average();
                             //std1 = UF.StandardDeviation(newseries1);
                             std2 = UF.StandardDeviation(newseries2);

                         }*/



                    }

                    if (timestep - lbk > 0 && data.InputData[i].Dates[timestep].Date == data.InputData[i].Dates[timestep - lbk].Date)
                    {


                        if (data.InputData[i].Dates[timestep].TimeOfDay >= DataRefStartTime && data.InputData[i].Dates[timestep].TimeOfDay <= DataRefEndTime)
                        {

                            /* if (Move2.Count() > lbk3)
                             {
                                 //series1 = Move1.ToArray();
                                 //newseries1 = UF.GetRange(series1, series1.Length - lbk2, series1.Length - 1);

                                 series2 = Move2.ToArray();
                                 newseries2 = UF.GetRange(series2, series2.Length - lbk3, series2.Length - 1);

                                 //avg1 = newseries1.Average();
                                 avg2 = newseries2.Average();
                                 //std1 = UF.StandardDeviation(newseries1);
                                 std2 = UF.StandardDeviation(newseries2);

                             }*/


                            double[] currentmove1 = new double[lbk/5];
                                                                                 
                            
                            for (int j = 0; j < lbk/5; j++)
                            {
                                currentmove1[j] = Math.Log(ltp_stock[timestep] / ltp_stock[timestep - 5*(j + 1)]);
                                
                            }



                            double currentmove2 = (ltp_sec[timestep] / ltp_stock[timestep]);


                            Move1.Add(Math.Abs(currentmove1[(lbk/5) - 1]));
                            Move2.Add(Math.Abs(currentmove2));




                            if (series1.Length > lbk2)
                            {
                                double[] z1 = new double[lbk/5];


                                for (int j = 0; j < lbk/5; j++)
                                {
                                    z1[j] = (Math.Abs(currentmove1[j]) - avg1) / std1;
                                    
                                }



                                //Zscore1[timestep] = z1[lbk-1];
                                //Zscore2[timestep] = z2[lbk-1];


                                for (int j = 0; j < lbk / 5; j++)
                                {
                                    if (z1[j] == z1.Max())
                                    {
                                        z1_max_i = j;

                                    }

                                 }
                                //double metric = z2 / z1;

                                Zscore1[timestep] = z1[z1_max_i];






                                if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[timestep].TimeOfDay < TrdEntryEndTime)
                                {
                                    //Move.Add(currentmove);




                                    /*if (currentmove1[z1_max_i] < 0)
                                    {
                                        double[] z3 = new double[lbk / 5];

                                        for (int j = 2; j < lbk / 5; j++)
                                        {

                                            z3[j] = Math.Abs(z2[j]) / z1[j];
                                            z3[0] = 9999999999;
                                            z3[1] = 9999999999;
                                        }*/


                                    /*double w1 = 0;
                                    double ss = 0;
                                    for (int j = 0; j < lbk - z1_max_i - 1; j++)
                                    {
                                        w1 = w1 + (lbk - z1_max_i - 1 - j) * z3[j];

                                        ss = ss + j + 1;
                                    }

                                    w1 = w1 / ss;*/








                                    if (z1[z1_max_i] >= siglevel1 && np[timestep - 1] != 1 && (mode == "A" || mode == "L") && flag <= 0 && currentmove1[z1_max_i] < 0.0 && ltp_stock[timestep] >= 20.0)
                                    {


                                        if (Move2.Count() > lbk3)
                                        {
                                            //series1 = Move1.ToArray();
                                            //newseries1 = UF.GetRange(series1, series1.Length - lbk2, series1.Length - 1);

                                            series2 = Move2.ToArray();
                                            newseries2 = UF.GetRange(series2, series2.Length - lbk3 - 1, series2.Length - 2);

                                            //avg1 = newseries1.Average();
                                            avg2 = newseries2.Average();
                                            //std1 = UF.StandardDeviation(newseries1);
                                            std2 = UF.StandardDeviation(newseries2);

                                        }

                                        double z2 = (Math.Abs(currentmove2) - avg2) / std2;
                                        Zscore2[timestep] = z2;

                                        if (z2 >= siglevel2)
                                        {

                                            sig[timestep] = +2;
                                            np[timestep] = +1;
                                            timeintrade = 0;
                                            longtrades++;
                                            zzz = z1_max_i;
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

                    TIT[timestep] = timeintrade;

                    if (((timeintrade >= 5 *1.5*(zzz + 1) && timeintrade >= et1) || timeintrade >= et2) && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        timeintrade = 0;
                    }

                    if (np[timestep] == 0 && np[timestep - 1] == 1)
                    {
                        flag = NT;
                    }
                    else
                        flag--;




                    if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdSquareOffTime && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        timeintrade = 0;
                    }

                    if (sig[timestep] == 0)
                        np[timestep] = np[timestep - 1];



                }


                //FileWrite opt1 = new FileWrite("sig.csv");
                // opt1.DataWriteOneVar(sig);
                //FileWrite opt2 = new FileWrite("np.csv");
                //opt2.DataWriteOneVar(np);
                //FileWrite opt5 = new FileWrite("TimeInTrade.csv");
                //opt5.DataWriteOneVar(TIT);

                //FileWrite opt3 = new FileWrite("zcore1.csv");
                //opt3.DataWriteOneVar(Zscore1);
                //FileWrite opt4 = new FileWrite("zcore2.csv");
                //opt4.DataWriteOneVar(Zscore2);

                base.CalculateNetPosition(data, sig, i, 1.5, -1.5, -0.5, 0.5);
            }

            base.RunStrategyBase(data);
        }
    }
}

