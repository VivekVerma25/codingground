using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class ADRATIOVivekDynamicTime : BasicStrategy
    {
        public object TradeStartTime = 10.5;
        public object TradeEndTime = 14;
        public object TradeSquareOff = 15.4;
        public object ADMult = 0.0;
        public object ADSqMult = -100;
        public object Lookback = 6;
        public object Lag = 0;
        public object LONGFlag = true;
        public object SHORTFlag = true;

        public ADRATIOVivekDynamicTime(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double adm = Convert.ToDouble(ADMult);
            double adsqm = Convert.ToDouble(ADSqMult);
            int lbk = Convert.ToInt32(Lookback);
            int lag = Convert.ToInt32(Lag);
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

                for (int j = (lbk + lag + 1); j < (ltp.Length - 1); j++)
                {
                    double diff = ad[j - lag] - ad[j - lag - lbk];
                    double currentad = ad[j - lag];

                    DateTime datenow = data.InputData[i].Dates[j - lag].Date;
                    DateTime datebefore = data.InputData[i].Dates[j - lag - lbk].Date;

                    if (datenow == datebefore)
                    {

                        if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime)
                        {
                            if (diff > adm && longflag == true)
                            {
                                sig[j] = +2;
                                np[j] = +1;
                            }

                            if (diff < -adm && shortflag == true)
                            {
                                sig[j] = -2;
                                np[j] = -1;
                            }
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