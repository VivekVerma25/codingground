using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class ADXStrategy : BasicStrategy
    {
        public object TradeStartTime = 9.5;
        public object TradeEndTime = 14;
        public object TradeExitTime = 15;
        public object Thresh = 25.0;
        public object DIPeriod = 14.0;
        public object ADXPeriod = 8.0;

        public ADXStrategy(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            int DIperiod = Convert.ToInt32(DIPeriod);
            int ADXperiod = Convert.ToInt32(ADXPeriod);
            double thresh = Convert.ToDouble(Thresh);
            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime = DateTime.FromOADate(Convert.ToDouble(TradeEndTime) / 24.0).TimeOfDay;
            TimeSpan TrdExitTime = DateTime.FromOADate(Convert.ToDouble(TradeExitTime) / 24.0).TimeOfDay;

            for (int i = 0; i < numSec; i++)
            {

                double[] nifty = data.InputData[i].OHLC.close;
                double[] niftyhigh = data.InputData[i].OHLC.high;
                double[] niftylow = data.InputData[i].OHLC.low;
                double[] niftyopen = data.InputData[i].OHLC.open;
                double[] volume = data.InputData[i].OHLC.volume;
                double[] tr = new double[nifty.Length];
                double[] atr = new double[nifty.Length];
                double[] upmov = new double[nifty.Length];
                double[] downmov = new double[nifty.Length];
                double[] DMup = new double[nifty.Length];
                double[] DMdown = new double[nifty.Length];
                double[] DIup = new double[nifty.Length];
                double[] DIdown = new double[nifty.Length];
                double[] DIndex = new double[nifty.Length];
                double[] AverageDIndex = new double[nifty.Length];
                double[] sig = new double[nifty.Length];
                double[] np = new double[nifty.Length];

                for (int j = 500; j < nifty.Length; j++)
                {
                    tr[j] = Math.Max(niftyhigh[j] - niftylow[j], Math.Max(Math.Abs(nifty[j] - niftyhigh[j - 1]),Math.Abs(nifty[j] - niftylow[j - 1])));
                    upmov[j] = niftyhigh[j] - niftyhigh[j-1];
                    downmov[j] = niftylow[j - 1] - niftylow[j];

                    if (upmov[j] > downmov[j] && upmov[j] > 0)
                    {
                        DMup[j] = upmov[j];
                        DMdown[j] = 0;
                    }
                    else if (downmov[j] > upmov[j] && downmov[j] > 0)
                    {
                        DMup[j] = 0;
                        DMdown[j] = downmov[j];
                    }

                    else
                    {
                        DMup[j] = 0;
                        DMdown[j] = 0;
                    }

                    atr[j] = 100 * UF.GetRange(DMup, j - DIperiod, j).Sum();
                    DIup[j] = 100 * UF.GetRange(DMup, j - DIperiod, j).Sum();
                    DIdown[j] = 100 * UF.GetRange(DMdown, j - DIperiod, j).Sum();

                    DIndex[j]=Math.Abs(DIup[j]-DIdown[j])/(DIup[j]+DIdown[j]);

                    AverageDIndex[j] = 100 * UF.GetRange(DIndex, j - ADXperiod, j).Average();

                    if (data.InputData[i].Dates[j].TimeOfDay <= TrdEntryStartTime)
                    {
                        sig[j] = 0;
                        np[j] = 0;
                    }
                    else if (data.InputData[i].Dates[j].TimeOfDay > TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay < TrdEntryEndTime)
                    {
                        if (DIup[j] > DIdown[j] && AverageDIndex[j] > thresh)
                        {
                            sig[j] = 2;
                            np[j] = 1;
                        }
                        else if (DIup[j] < DIdown[j] && AverageDIndex[j] > thresh)
                        {
                            sig[j] = -2;
                            np[j] = -1;
                        }
                        else
                            np[j] = np[j - 1];
                    }
                    else if (data.InputData[i].Dates[j].TimeOfDay < TrdExitTime)
                        np[j] = np[j - 1];
                    else
                    {
                        np[j] = 0;
                        sig[j] = -np[j - 1];
                    }
                        

                      
                }

                base.CalculateNetPosition(data, sig, i, 1.5, -1.5, -0.5, 0.5);
         }

         base.RunStrategyBase(data);        

        }

    }
}