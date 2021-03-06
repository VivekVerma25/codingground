﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class stvssec_gap : BasicStrategy
    {
        public object TradeStartTime = 9.75;
        public object TradeEndTime = 14.75;
        public object TradeSquareOffTime = 15;
        public object RefStartTime = 9.75;
        public object RefEndTime = 15;
        public object Lookback = 5;
        public object Lookback2 = 300;
        public object GapLookback = 40;
        //public object Mult = 2.5;
        //public object EC = 0.9;
        public object Mode = "L"; //"A", "L", "S"
        public object SigmaDifferenceL = 0.4;
        public object SigmaDifferenceS = 0.4;
        public object Gap1 = 0.9;
        public object Gap2 = 0.5;
        public object SigmaLevel1 = 2;
        public object SigmaLevel2 = 1;
        public object ExitTime1 = 3;
        public object ExitTime2 = 6;
        public object LongCount = 100;
        public object ShortCount = 100;
        public object NoTrade = 3;
        //public object returns = 0.000;

        public stvssec_gap(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            int lbk = Convert.ToInt32(Lookback);
            int lbk2 = Convert.ToInt32(Lookback2);
            int lbk3 = Convert.ToInt32(GapLookback);
            //double mult = Convert.ToDouble(Mult);
            //double ec = Convert.ToDouble(EC);
            string mode = Convert.ToString(Mode);
            double sigdiffL = Convert.ToDouble(SigmaDifferenceL);
            double sigdiffS = Convert.ToDouble(SigmaDifferenceS);
            double siglevel1 = Convert.ToDouble(SigmaLevel1);
            double siglevel2 = Convert.ToDouble(SigmaLevel2);
            double et1 = Convert.ToDouble(ExitTime1);
            double et2 = Convert.ToDouble(ExitTime2);
            //double ret = Convert.ToDouble(returns);
            int LC = Convert.ToInt32(LongCount);
            int NT = Convert.ToInt32(NoTrade);
            int SC = Convert.ToInt32(ShortCount);
            double G1 = Convert.ToDouble(Gap1);
            double G2 = Convert.ToDouble(Gap2);

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

                List<double> close1 = new List<double>();
                List<double> close2 = new List<double>();

                List<double> gap1 = new List<double>();
                List<double> gap2 = new List<double>();

                double[] Zscore1 = new double[len];
                double[] Zscore2 = new double[len];

                double[] series1 = new double[0];
                double[] series2 = new double[0];

                double[] newseries1 = new double[0];
                double[] newseries2 = new double[0];

                double PL1 = 0;
                double PL2 = 0;

                double gg1 = 0;
                double gg2 = 0;

                double avg1 = 0;
                double avg2 = 0;
                double std1 = 0;
                double std2 = 0;
                int z1_max_i = 0;
                int z2_max_i = 0;
                int flag = 0;
                //double longlevel = -999999999;
                //double shortlevel = 999999999;

                //double high = 999999999;
                //double low = -999999999;

                int longtrades = 0;
                int shorttrades = 0;
                double timeintrade = 0;


                for (int timestep = 1; timestep < (len); timestep++)
                {

                    if (np[timestep - 1] != 0)
                        timeintrade++;

                    if (data.InputData[i].Dates[timestep].Date != data.InputData[i].Dates[timestep - 1].Date)
                    {
                        flag = 0;
                        timeintrade = 0;
                        shorttrades = 0;
                        longtrades = 0;
                        close1 = new List<double>();
                        close2 = new List<double>();
                        close1.Add(ltp_stock[timestep]);
                        close2.Add(ltp_sec[timestep]);

                        gg1 = ((ltp_stock[timestep] - ltp_stock[timestep - 1]) / ltp_stock[timestep - 1]);
                        gg2 = ((ltp_sec[timestep] - ltp_sec[timestep - 1]) / ltp_sec[timestep - 1]);

                        gap1.Add(Math.Abs(gg1));
                        gap2.Add(Math.Abs(gg2));


                        if (Move1.Count() > lbk2 && Move2.Count() > lbk2)
                        {
                            series1 = Move1.ToArray();
                            newseries1 = UF.GetRange(series1, series1.Length - lbk2, series1.Length - 1);

                            series2 = Move2.ToArray();
                            newseries2 = UF.GetRange(series2, series2.Length - lbk2, series2.Length - 1);

                            avg1 = newseries1.Average();
                            avg2 = newseries2.Average();
                            std1 = UF.StandardDeviation(newseries1);
                            std2 = UF.StandardDeviation(newseries2);

                        }

                        if (gap1.Count() > (lbk3+1) && gap2.Count() > (lbk3+1))
                        {
                            double[] series3 = gap1.ToArray();
                            double[] newseries3 = UF.GetRange(series3, series3.Length - lbk3-1, series3.Length - 2);

                            double[] series4 = gap2.ToArray();
                            double[] newseries4 = UF.GetRange(series4, series4.Length - lbk3-1, series4.Length - 2);

                            PL1 = UF.Percentile(newseries3, G1);
                            PL2 = UF.Percentile(newseries4, G2);
                            
                            
                        }

                    }


                    if (data.InputData[i].Dates[timestep].Date == data.InputData[i].Dates[timestep - 1].Date)
                    {
                        close1.Add(ltp_stock[timestep]);
                        close2.Add(ltp_sec[timestep]);
                    }





                    if (timestep - lbk > 0 && data.InputData[i].Dates[timestep].Date == data.InputData[i].Dates[timestep - lbk].Date)
                    {
                        double[] currentmove1 = new double[lbk];
                        double[] currentmove2 = new double[lbk];




                        for (int j = 0; j < lbk; j++)
                        {
                            currentmove1[j] = Math.Log(ltp_stock[timestep] / ltp_stock[timestep - j - 1]);
                            currentmove2[j] = Math.Log(ltp_sec[timestep] / ltp_sec[timestep - j - 1]);
                        }



                        if (data.InputData[i].Dates[timestep].TimeOfDay >= DataRefStartTime && data.InputData[i].Dates[timestep].TimeOfDay <= DataRefEndTime)
                        {
                            Move1.Add(Math.Abs(currentmove1[lbk - 1]));
                            Move2.Add(Math.Abs(currentmove2[lbk - 1]));
                        }



                        if (series1.Length > lbk2 && series2.Length > lbk2)
                        {

                            double[] z1 = new double[lbk];
                            double[] z2 = new double[lbk];


                            for (int j = 0; j < lbk; j++)
                            {
                                z1[j] = (Math.Abs(currentmove1[j]) - avg1) / std1;
                                z2[j] = (Math.Abs(currentmove2[j]) - avg2) / std2;

                            }



                            //Zscore1[timestep] = z1[lbk-1];
                            //Zscore2[timestep] = z2[lbk-1];

                            for (int j = 0; j < lbk; j++)
                            {
                                if (z1[j] == z1.Max())
                                {
                                    z1_max_i = j;

                                }

                                if (z2[j] == z2.Max())
                                {
                                    z2_max_i = j;

                                }

                            }




                            double metric = z2[z1_max_i] / z1[z1_max_i];



                            if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[timestep].TimeOfDay < TrdEntryEndTime)
                            {
                                //Move.Add(currentmove);

                                if (currentmove1[z1_max_i] < 0)
                                {
                                    double[] z3 = new double[lbk];

                                    for (int j = 0; j < lbk; j++)
                                    {

                                        z3[j] = Math.Abs(z2[j]) / z1[j];

                                    }


                                    /*double w1 = 0;
                                    double ss = 0;
                                    for (int j = 0; j < lbk - z1_max_i - 1; j++)
                                    {
                                        w1 = w1 + (lbk - z1_max_i - 1 - j) * z3[j];

                                        ss = ss + j + 1;
                                    }

                                    w1 = w1 / ss;*/

                                    double lowratio1 = (ltp_stock[timestep] - close1.Min()) / (close1.Max() - close1.Min());
                                    double lowratio2 = (ltp_sec[timestep] - close2.Min()) / (close2.Max() - close2.Min());
                                    int yyy = 0;
                                    if (gg1 >= PL1)
                                    { yyy = 1; }
                                    else
                                    { yyy = 0; }


                                        if (z1[z1_max_i] >= siglevel1 && metric <= z3.Min() && np[timestep - 1] != 1 && metric <= sigdiffL && (mode == "A" || mode == "L") && longtrades < LC && flag <= 0 && yyy==1)
                                        {
                                            sig[timestep] = +2;
                                            np[timestep] = +1;
                                            timeintrade = 0;
                                            longtrades++;

                                        }

                                }


                                if (currentmove1[z1_max_i] > 0)
                                {
                                    double[] z3 = new double[lbk];

                                    for (int j = 0; j < lbk; j++)
                                    {

                                        z3[j] = Math.Abs(z2[j]) / z1[j];

                                    }

                                    if (z1[z1_max_i] >= siglevel1 && np[timestep - 1] != -1 && metric <= sigdiffS && (mode == "A" || mode == "S") && shorttrades < SC && flag <= 0)
                                    {
                                        sig[timestep] = -2;
                                        np[timestep] = -1;
                                        timeintrade = 0;
                                        shorttrades++;
                                    }

                                }


                            }
                        }

                    }


                    if ((timeintrade >= Math.Max((z1_max_i + 1), et1) || timeintrade >= et2) && np[timestep - 1] != 0)
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
                //opt1.DataWriteOneVar(sig);
                //FileWrite opt2 = new FileWrite("np.csv");
                //opt2.DataWriteOneVar(np);
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

