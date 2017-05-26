﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class AD_range_dyn2 : BasicStrategy
    {
        public object TradeStartTime = 10;
        public object TradeEndTime1 = 12.5;
        public object TradeEndTime2 = 14.5;
        public object TradeSquareOff = 15.4;
        public object ADMult = 1;
        public object ADSqMult = 100;
        public object ADCutoffLONG = -100;
        public object ADCutoffSHORT = 100;
        public object Lookback1 = 6;
        public object Lookback2 = 10;
        public object Lag = 0;
        public object Fwd = 0;
        public object LONGFlag = true;
        public object SHORTFlag = true;

        public AD_range_dyn2(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double adm = Convert.ToDouble(ADMult);
            double adsqm = Convert.ToDouble(ADSqMult);
            double adcl = Convert.ToDouble(ADCutoffLONG);
            double adcs = Convert.ToDouble(ADCutoffSHORT);
            int lag = Convert.ToInt32(Lag);
            int fwd = Convert.ToInt32(Fwd);
            int lbk1 = Convert.ToInt32(Lookback1);
            int lbk2 = Convert.ToInt32(Lookback2);
            Boolean longflag = Convert.ToBoolean(LONGFlag);
            Boolean shortflag = Convert.ToBoolean(SHORTFlag);

            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime1 = DateTime.FromOADate(Convert.ToDouble(TradeEndTime1) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime2 = DateTime.FromOADate(Convert.ToDouble(TradeEndTime2) / 24.0).TimeOfDay;
            TimeSpan TrdSqOff = DateTime.FromOADate(Convert.ToDouble(TradeSquareOff) / 24.0).TimeOfDay;

            for (int i = 0; i < numSec; i++)
            {
                double[] ltp = data.InputData[i].Prices;
                double[] ad = data.InputData[i].Extra1;

                double[] sig = new double[ltp.Length];
                double[] np = new double[ltp.Length];

                double openad1 = 0;
                double openad2 = 0;
                double timecounter = 0;
                double diff2 = -100;
                int flag = 0;

                List<double> Move1 = new List<double>();
                double[] series1 = new double[0];
                double[] newseries1 = new double[0];

                List<double> Move2 = new List<double>();
                double[] series2 = new double[0];
                double[] newseries2 = new double[0];

                for (int j = (lag + 1); j < (ltp.Length - 1); j++)
                {
                    timecounter++;

                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j - 1].Date)
                    {
                        openad1 = (ad[j] + ad[j + 1] + ad[j + 2]) / 3;
                        timecounter = 0;
                        flag = 0;

                        series1 = Move1.ToArray();
                        //newseries1 = UF.GetRange(series1, series1.Length - lbk2, series1.Length - 1);


                        Move2.Add((series1.Max() - series1.Min()));
                        Move1 = new List<double>();

                        if (Move2.Count() >= lbk2)
                        {
                            series2 = Move2.ToArray();
                            newseries2 = UF.GetRange(series2, series2.Length - lbk2, series2.Length - 1);
                        }



                    }
                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryEndTime1 && flag==0)
                    {
                        openad1 = (ad[j] + ad[j-1] + ad[j -2]) / 3;
                        flag = 1;
                        timecounter = 6;
                    }


                    double diff1 = ad[j - lag] - openad1;
                    Move1.Add(ad[j]);
                    if (timecounter > lbk1)
                    {
                        diff2 = ad[j - lag] - ad[j - lag - lbk1];

                    }
                    double currentad = ad[j - lag];

                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime1)
                    {
                        if (series2.Length >= lbk2)
                        {
                            if (diff1 > Math.Min(Math.Max(adm * (timecounter / 75) * newseries2.Average(), 0.1), 0.5) && longflag == true)
                            {
                                sig[j] = +2;
                                np[j] = +1;
                            }

                            if (diff1 < -Math.Min(Math.Max(adm * (timecounter / 75) * newseries2.Average(), 0.1), 0.5) && shortflag == true)
                            {
                                sig[j] = -2;
                                np[j] = -1;
                            }
                        }
                    }


                    if (data.InputData[i].Dates[j].TimeOfDay > TrdEntryEndTime1 && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime2)
                    {
                        if (series2.Length >= lbk2)
                        {
                            if (diff1 > Math.Min(Math.Max(adm * (timecounter / 75) * newseries2.Average(), 0.1), 0.5) && longflag == true)
                            {
                                sig[j] = +2;
                                np[j] = +1;
                            }

                            if (diff1 < -Math.Min(Math.Max(adm * (timecounter / 75) * newseries2.Average(), 0.1), 0.5) && shortflag == true)
                            {
                                sig[j] = -2;
                                np[j] = -1;
                            }
                        }
                    }

                    if ((np[j - 1] == 1 && diff1 < -Math.Min(Math.Max(adm * newseries2.Average() / 10, 0.1), adm / 10)) || (np[j - 1] == -1 && diff1 > Math.Min(Math.Max(adm * newseries2.Average() / 10, 0.1), adm / 10)))
                    {
                        sig[j] = -np[j - 1];
                        np[j] = 0;
                    }

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

            }

            base.RunStrategyBase(data);

        }

    }
}