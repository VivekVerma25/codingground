using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class ADRatio_new3 : BasicStrategy
    {
        public object TradeStartTime = 10;
        public object TradeEndTime = 15;
        public object TradeSquareOff = 15.4;
        public object ADMult = -100;
        public object ADSqMult = -100;
        public object DIST_THRESH = 0.1;
        public object ADCutoffLONG = -100;
        public object ADCutoffSHORT = 100;
        public object Lookback = 6;
        public object Lag = 0;
        public object Fwd = 0;
        public object LONGFlag = true;
        public object SHORTFlag = true;

        public ADRatio_new3(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double adm = Convert.ToDouble(ADMult);
            double dthresh = Convert.ToDouble(DIST_THRESH);
            double adsqm = Convert.ToDouble(ADSqMult);
            double adcl = Convert.ToDouble(ADCutoffLONG);
            double adcs = Convert.ToDouble(ADCutoffSHORT);
            int lag = Convert.ToInt32(Lag);
            int fwd = Convert.ToInt32(Fwd);
            int lbk = Convert.ToInt32(Lookback);
            Boolean longflag = Convert.ToBoolean(LONGFlag);
            Boolean shortflag = Convert.ToBoolean(SHORTFlag);

            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime = DateTime.FromOADate(Convert.ToDouble(TradeEndTime) / 24.0).TimeOfDay;

            TimeSpan TrdSqOff = DateTime.FromOADate(Convert.ToDouble(TradeSquareOff) / 24.0).TimeOfDay;

            for (int i = 0; i < numSec; i++)
            {
                double[] ltp = data.InputData[i].Prices;
                double[] ad = data.InputData[i].Extra1;

                double[] sig = new double[ltp.Length];
                double[] np = new double[ltp.Length];

                double openad = 0;
                double openad2 = 0;
                double timecounter = 0;
                double diff2 = -100;
                double dist1 = 0;
                double dist2 = 10000000000000;
                double flag = 0;
                double timeintrade = 0;
                for (int j = (lag + 1); j < (ltp.Length - 1); j++)
                {
                  if (np[j - 1] != 0)
                      timeintrade++;

                    timecounter++;

                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j - 1].Date)
                    {
                        openad = (ad[j] + ad[j + 1] + ad[j + 2]) / 3;
                        openad2 = openad;
                        timecounter = 0;
                        dist1 = 0;
                        dist2 = 10000000000000;
                        timeintrade = 0;
                    }

                    if (timeintrade == 12)
                     {
                         dist2 = 0;
                         flag = 0;
                     }

                    double diff1 = ad[j - lag] - openad;
                    double diff3 = ad[j - lag] - openad2;
                    dist2 = dist2 + Math.Abs(ad[j] - ad[j-1]);
                    if (timecounter > 2)
                    {
                        dist1 += Math.Abs(ad[j] - ad[j-1]);
                    }

                    if (timecounter > lbk)
                    {
                        diff2 = ad[j - lag] - ad[j - lag - lbk];
                        /*for (int t = 0; t < lbk; t++)
                        {
                            dist2 += Math.Abs(ad[j - (t)] - ad[j - (1 + t)]);
                        }*/


                    }
                    double currentad = ad[j - lag];

                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime)
                    {
                        if ((diff1 / dist1) > dthresh && longflag == true)
                        {
                            sig[j] = +2;
                            np[j] = +1;
                            openad2 = ad[j];
                            flag = 1;
                            timeintrade = 0;
                        }

                        if ((diff1 / dist1) < -dthresh && shortflag == true)
                        {
                            sig[j] = -2;
                            np[j] = -1;
                            openad2 = ad[j];
                            flag = 1;
                            timeintrade = 0;
                        }
                    }

                    if ((np[j - 1] == 1 && (diff3 / dist2) < adsqm) || (np[j - 1] == -1 && (diff3 / dist2) > -adsqm))
                    {
                        sig[j] = -np[j - 1];
                        np[j] = 0;
                    }

                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdSqOff && np[j - 1] != 0)
                    {
                        sig[j] = -np[j - 1];
                        np[j] = 0;
                        timeintrade = 0;
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

            }

            base.RunStrategyBase(data);

        }

    }
}