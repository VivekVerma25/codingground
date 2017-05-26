﻿using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class ADRATIOVivekStaticTime : BasicStrategy
    {
        public object TradeStartTime = 10.5;
        public object TradeSquareOff = 15.25;
        public object ADMult = 0.05;
        public object ADLim = 0.5;
        public object ADSqMult =-100;
        public object Lag = 1;
        public object Fwd = 0;
        public object LONGFlag = true;
        public object SHORTFlag = true;

        public ADRATIOVivekStaticTime(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double adm = Convert.ToDouble(ADMult);
            double alm = Convert.ToDouble(ADLim);
            double adsqm = Convert.ToDouble(ADSqMult);
            int lag = Convert.ToInt32(Lag);
            int fwd = Convert.ToInt32(Fwd);
            Boolean longflag = Convert.ToBoolean(LONGFlag);
            Boolean shortflag = Convert.ToBoolean(SHORTFlag);

            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;

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

                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j - 1].Date)
                    {
                        openad = (ad[j + fwd] + ad[j + fwd + 1] + ad[j + fwd + 2]) / 3;
                        timecounter = 0;
                    }

                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime)
                        timecounter++;


                    double diff = ad[j - lag] - openad;
                    double currentad = ad[j - lag];

                    if (timecounter == 1)
                    {
                        if (diff > adm && longflag == true && openad <=alm)
                        {
                            sig[j] = +2;
                            np[j] = +1;
                        }

                        if (diff < -adm && shortflag == true && openad >= -alm)
                        {
                            sig[j] = -2;
                            np[j] = -1;
                        }
                    }

                    if ((np[j - 1] == 1 && diff < adsqm) || (np[j - 1] == -1 && diff > -adsqm))
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