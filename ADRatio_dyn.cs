﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class ADRatio_dyn : BasicStrategy
    {
        public object TradeStartTime = 10;
        public object TradeEndTime = 15;
        public object TradeSquareOff = 15.4;
        public object ADMult1 = 0.1;
        public object ADMult2 = 0.0;
        public object ADSqMult = 0;
        public object ADCutoffLONG = -100;
        public object ADCutoffSHORT = 100;
        public object Lookback = 6;
        public object Lag = 0;
        public object Fwd = 0;
        public object LONGFlag = true;
        public object SHORTFlag = true;

        public ADRatio_dyn(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double adm1 = Convert.ToDouble(ADMult1);
            double adm2 = Convert.ToDouble(ADMult2);
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

                for (int j = (lag + 1); j < (ltp.Length - 1); j++)
                {

                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j - 1].Date)
                        openad = ad[j + fwd];

                    double diff1 = (ad[j - lag] - openad);
                    
                    double currentad = ad[j - lag];

                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime)
                    {
                        double diff2 = (ad[j - lag] - ad[j - lag - lbk]);
                        if (diff1 > adm1 && diff2 >= adm2 && currentad >= adcl && longflag == true)
                        {
                            sig[j] = +2;
                            np[j] = +1;
                        }

                        if (diff1 < -adm1 && diff2 <= adm2 && currentad <= adcs && shortflag == true)
                        {
                            sig[j] = -2;
                            np[j] = -1;
                        }
                    }

                    if ((np[j - 1] == 1 && diff1 < adsqm) || (np[j - 1] == -1 && diff1 > -adsqm))
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