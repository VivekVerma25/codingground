using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class IPA1 : BasicStrategy
    {
        public object TradeStartTime = 9.00;
        public object TradeEndTime = 9.1;
        public object TradeSquareOffTime = 15;
        public object Threshold = 1;
        public object Lookback = 30;
        
        public IPA1(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double th = Convert.ToDouble(Threshold);
            int lbk = Convert.ToInt32(Lookback);
            
            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime = DateTime.FromOADate(Convert.ToDouble(TradeEndTime) / 24.0).TimeOfDay;
            TimeSpan TrdSquareOffTime = DateTime.FromOADate(Convert.ToDouble(TradeSquareOffTime) / 24.0).TimeOfDay;


            for (int i = 0; i < numSec; i++)
            {
                int len = data.InputData[i].Dates.Length;

                double[] ltp = data.InputData[i].Prices;
                //double[] ltp = data.InputData[i].OHLC.close;
                
                double[] sig = new double[len];
                double[] np = new double[len];

                List<double> Gap = new List<double>();
                
                double high = 99999999999;
                double low = -99999999999;

                double latestparam = 0;

                for (int timestep = 1; timestep < len; timestep++)
                {

                    if (data.InputData[i].Dates[timestep].Date != data.InputData[i].Dates[timestep - 1].Date)
                    {
                        int ctr = timestep - 1;

                        double currentOpen = ltp[timestep];
                        double previousClose = ltp[ctr];

                        Gap.Add(Math.Log(currentOpen / previousClose));
                        
                        double[] newRange = new double[10];

                        newRange = Gap.ToArray();
                        
                        if (newRange.Length > (lbk + 1))
                        {
                            double[] series = UF.GetRange(newRange, newRange.Length - lbk - 2, newRange.Length - 2);

                            high = series.Average() + (th * UF.StandardDeviation(series));
                            low = series.Average() - (th * UF.StandardDeviation(series));
                        }

                        latestparam = newRange[newRange.Length - 1];

                    }

                    if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdSquareOffTime && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                    }

                    if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[timestep].TimeOfDay <= TrdEntryEndTime)
                    {
                        if (latestparam > high && np[timestep - 1] != 1)
                        {
                            sig[timestep] = +2;
                            np[timestep] = +1;
                        }

                        if (latestparam < low && np[timestep - 1] != -1)
                        {
                            sig[timestep] = -2;
                            np[timestep] = -1;
                        }

                    }

                    if (sig[timestep] == 0)
                        np[timestep] = np[timestep - 1];

                }

                base.CalculateNetPosition(data, sig, i, 1.5, -1.5, -0.5, 0.5);
            }

            base.RunStrategyBase(data);
        }
    }
}

