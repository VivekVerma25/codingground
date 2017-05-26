using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class test3 : BasicStrategy
    {
        public object TradeStartTime = 9.75;
        public object TradeEndTime = 14.75;
        public object TradeSquareOffTime = 15;
        public object RefStartTime = 9.75;
        public object RefEndTime = 15;
        public object Lookback = 12;
        public object Lookback2 = 1200;
        //public object Mult = 2.5;
        //public object EC = 0.9;
        public object Mode = "L"; //"A", "L", "S"
        public object SigmaDifferenceL = 0.6;
        public object SigmaDifferenceS = 0.75;
        public object SigmaLevel1 = 2;
        public object SigmaLevel2 = 3;
        public object ExitTime = 6;
        public object LongCount = 1;

        public object returns = 0.000;

        public test3(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            int lbk = Convert.ToInt32(Lookback);
            int lbk2 = Convert.ToInt32(Lookback2);
            //double mult = Convert.ToDouble(Mult);
            //double ec = Convert.ToDouble(EC);
            string mode = Convert.ToString(Mode);
            double sigdiffL = Convert.ToDouble(SigmaDifferenceL);
            double sigdiffS = Convert.ToDouble(SigmaDifferenceS);
            double siglevel1 = Convert.ToDouble(SigmaLevel1);
            double siglevel2 = Convert.ToDouble(SigmaLevel2);
            double et = Convert.ToDouble(ExitTime);
            double ret = Convert.ToDouble(returns);
            int LC = Convert.ToInt32(LongCount);


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

                double[] Zscore1 = new double[len];
                double[] Zscore2 = new double[len];

                double[] series1 = new double[0];
                double[] series2 = new double[0];

                double[] newseries1 = new double[0];
                double[] newseries2 = new double[0];

                double avg1 = 0;
                double avg2 = 0;
                double std1 = 0;
                double std2 = 0;
                //int z1_min_i = 0;
                //int z2_min_i = 0;

                //double longlevel = -999999999;
                //double shortlevel = 999999999;

                //double high = 999999999;
                //double low = -999999999;

                int longtrades = 0;

                double timeintrade = 0;

                for (int timestep = 1; timestep < (len); timestep++)
                {
                    if (np[timestep - 1] != 0)
                        timeintrade++;

                    if (data.InputData[i].Dates[timestep].Date != data.InputData[i].Dates[timestep - 1].Date)
                    {
                        timeintrade = 0;
                        longtrades = 0;

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

                    }

                    if (timestep - lbk > 0 && data.InputData[i].Dates[timestep].Date == data.InputData[i].Dates[timestep - lbk].Date)
                    {
                        double currentmove1 = Math.Log(ltp_stock[timestep] / ltp_stock[timestep - lbk]);
                        double currentmove2 = Math.Log(ltp_sec[timestep] / ltp_sec[timestep - lbk]);



                        if (data.InputData[i].Dates[timestep].TimeOfDay >= DataRefStartTime && data.InputData[i].Dates[timestep].TimeOfDay <= DataRefEndTime)
                        {
                            Move1.Add(currentmove1);
                            Move2.Add(currentmove2);
                        }


                        if (series1.Length > lbk2 && series2.Length > lbk2)
                        {

                            double z1 = (currentmove1 - avg1)/std1;
                            Zscore1[timestep] = z1;

                            double z2 = (currentmove2 - avg2)/std2;
                            Zscore2[timestep] = z2;
                            
                            double z1_avg = 0;
                            double z2_avg = 0;

                            for(int j=0; j<4; j++)
                            {
                                z1_avg += (4-j)*Zscore1[timestep-j];
                                z2_avg += (4 - j) * Zscore2[timestep - j];
                            }



                            double metric = z2_avg / z1_avg;

                            
                            

                            if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[timestep].TimeOfDay < TrdEntryEndTime)
                            {
                                //Move.Add(currentmove);

                                if (z1_avg <= -siglevel1 && np[timestep - 1] != 1 && metric <= sigdiffL && (mode == "A" || mode == "L") && longtrades < LC)
                                {
                                    sig[timestep] = +2;
                                    np[timestep] = +1;
                                    timeintrade = 0;
                                    longtrades++;



                                }

                                if (z1_avg >= siglevel2 && np[timestep - 1] != -1 && metric <= sigdiffS && (mode == "A" || mode == "S"))
                                {
                                    sig[timestep] = -2;
                                    np[timestep] = -1;
                                    timeintrade = 0;

                                }


                            }
                        }

                    }


                    if (timeintrade >=  et && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        timeintrade = 0;
                    }

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

