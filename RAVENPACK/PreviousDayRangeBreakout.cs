using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class PreviousDayRangeBreakout : BasicStrategy
    {
        public object TradeStartTime = 9.5;
        public object TradeSquareOffTime = 15;
        public object TradeEndTime = 14;
        public object RangePercent = 0.70;
        public object MinRange = 0;
        public object StopLossPercent = 0.25;
        public object MinStopLoss = 0.0025;
        public object AbsStopLoss = 0.0050;
        public object StopLossFlag = 0;
        public object LSMultiplier = 1;
        
        public PreviousDayRangeBreakout(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double rperc = Convert.ToDouble(RangePercent);
            double minr = Convert.ToDouble(MinRange);
            double slperc = Convert.ToDouble(StopLossPercent);
            double minsl = Convert.ToDouble(MinStopLoss);
            double abssl = Convert.ToDouble(AbsStopLoss);
            double slflag = Convert.ToDouble(StopLossFlag);
            double lsm = Convert.ToDouble(LSMultiplier);
            
            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime = DateTime.FromOADate(Convert.ToDouble(TradeEndTime) / 24.0).TimeOfDay;
            TimeSpan TrdSquareOffTime = DateTime.FromOADate(Convert.ToDouble(TradeSquareOffTime) / 24.0).TimeOfDay;

            for (int i = 0; i < numSec; i++)
            {
                int len = data.InputData[i].Dates.Length;

                double[] ltp = data.InputData[i].Prices;
                double[] highs = data.InputData[i].OHLC.high;
                double[] lows = data.InputData[i].OHLC.low;
                
                double[] sig = new double[len];
                double[] np = new double[len];
                
                double currclose = 99999999999;

                double longlevel = 99999999999;
                double shortlevel = -9999999999;

                double tradenumLong = 0;
                double tradenumShort = 0;

                List<double> h = new List<double>();
                List<double> l = new List<double>();

                double sl = 9999;
                double enpx = 0;

                for (int timestep = 1; timestep < len; timestep++)
                {
                    h.Add(highs[timestep]);
                    l.Add(lows[timestep]);

                    if (data.InputData[i].Dates[timestep].Date != data.InputData[i].Dates[timestep - 1].Date)
                    {
                        longlevel = 99999999999;
                        shortlevel = -9999999999;
                        enpx = 0;

                        currclose = ltp[timestep - 1];
                        tradenumLong = 0;
                        tradenumShort = 0;
                        np[timestep - 1] = 0;

                        double prevrange = (h.Max() - l.Min()) / h.Max();

                        longlevel = currclose * (1 + rperc * Math.Max(prevrange, minr));
                        shortlevel = currclose * (1 - (lsm * rperc * Math.Max(prevrange, minr)));

                        if (slflag == 1)
                            sl = abssl;
                        if (slflag == 0)
                            sl = Math.Max(slperc * Math.Max(prevrange, minr), minsl);

                        h.Clear();
                        l.Clear();

                        h.Add(highs[timestep]);
                        l.Add(lows[timestep]);
                    }

                    if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdSquareOffTime && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        enpx = 0;
                    }

                    double currret = 0;

                    if(enpx != 0)
                        currret = ltp[timestep]/enpx - 1;

                    if ((np[timestep - 1] == 1 && currret <= -sl) || (np[timestep - 1] == -1 && currret >= sl))
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        enpx = 0;
                    }

                    // Trade Initiation

                    if (data.InputData[i].Dates[timestep].TimeOfDay > TrdEntryStartTime && data.InputData[i].Dates[timestep].TimeOfDay < TrdEntryEndTime && ltp[timestep] > 10)
                    {
                        if (ltp[timestep] >= longlevel && np[timestep - 1] != 1 && tradenumLong == 0)
                        {
                            sig[timestep] = +2;
                            np[timestep] = +1;
                            tradenumLong++;
                            enpx = ltp[timestep];
                        }

                        if (ltp[timestep] <= shortlevel && np[timestep - 1] != -1 && tradenumShort == 0)
                        {
                            sig[timestep] = -2;
                            np[timestep] = -1;
                            tradenumShort++;
                            enpx = ltp[timestep];
                        }

                        if (h.Count() > 0 && l.Count() > 0)
                        {
                            if (ltp[timestep] >= longlevel && ltp[timestep] >= h.Max() && np[timestep - 1] != 1 && tradenumLong == 1)
                            {
                                sig[timestep] = +2;
                                np[timestep] = +1;
                                tradenumLong++;
                                enpx = ltp[timestep];
                            }

                            if (ltp[timestep] <= shortlevel && ltp[timestep] <= l.Min() && np[timestep - 1] != -1 && tradenumShort == 1)
                            {
                                sig[timestep] = -2;
                                np[timestep] = -1;
                                tradenumShort++;
                                enpx = ltp[timestep];
                            }
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

