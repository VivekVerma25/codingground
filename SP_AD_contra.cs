﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class SP_AD_contra : BasicStrategy
    {
        public object TradeStartTime = 10;
        public object TradeEndTime = 15;
        public object TradeSquareOff = 15.4;
        public object ADMult = 0.1;
        public object ADSqMult = 0;
        public object ADCutoffLONG = -100;
        public object ADCutoffSHORT = 100;
        public object Lag = 0;
        public object Fwd = 0;
        public object LONGFlag = true;
        public object SHORTFlag = true;

        public SP_AD_contra(string stratName, double alloc, double cost, double timeStep)
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
                double timecounter = 0;

                for (int j = (lag + 1); j < (ltp.Length - 1); j++)
                {
                    timecounter++;

                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j - 1].Date)
                    {
                        openad = ad[j + fwd];
                        timecounter = 0;
                    }

                    double diff = ad[j] - openad;
                    double currentad = ad[j];

                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime)
                    {
                        if (diff > Math.Min(Math.Max(adm * timecounter / 75, 0.1), 0.5) && shortflag == true)
                        {
                            sig[j + lag] = -2;
                            np[j + lag] = -1;
                        }

                        if (diff < Math.Max(Math.Min(-adm * timecounter / 75, -0.1), -0.5) && longflag == true)
                        {
                            sig[j + lag] = +2;
                            np[j + lag] = +1;
                        }
                    }

                    if ((np[j - 1] == 1 && diff > adsqm) || (np[j - 1] == -1 && diff < -adsqm))
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