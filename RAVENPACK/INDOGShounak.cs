using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class INDOGShounak : BasicStrategy
    {
        public object StartTime = "09:20";
        public object EndTime = "12:00";
        public object TradeSquareOffTime = "15:00";
        public object BODTime = "9:10";
        public object EODTime = "15:00";
        public object gapThreshold = 0.005;
        
        public INDOGShounak(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            TimeSpan start = TimeSpan.Parse(Convert.ToString(StartTime));
            TimeSpan end = TimeSpan.Parse(Convert.ToString(EndTime));
            int numSec = data.InputData.Count;
            double gth = Convert.ToDouble(gapThreshold);
            double prevclose = 0;
            
            TimeSpan TrdSquareOffTime = TimeSpan.Parse(Convert.ToString(TradeSquareOffTime));
            TimeSpan openTime = TimeSpan.Parse(Convert.ToString(BODTime));
            TimeSpan closeTime = TimeSpan.Parse(Convert.ToString(EODTime));


            for (int i = 0; i < numSec; i++)
            {
                int len = data.InputData[i].Dates.Length;

                double[] ltp = data.InputData[i].Prices;
                
                double[] high = data.InputData[i].OHLC.high;
                double[] low = data.InputData[i].OHLC.low;

                double[] sig = new double[len];
                double[] np = new double[len];

                double ticks = 0;
                double gap = 0;
                double trade = 0;
                double daytrdcount = 0;

                double maxP = 99999999999;
                double minP = -99999999999;

                for (int timestep = 1; timestep < len; timestep++)
                {
                    ticks++;


                    if (data.InputData[i].Dates[timestep].Date != data.InputData[i].Dates[timestep - 1].Date)
                    {
                        ticks = 0;
                        trade = 0;
                        gap = prevclose == 0 ? 0 : Math.Log(ltp[timestep] / prevclose);
                        daytrdcount = 0;
                        np[timestep - 1] = 0;
                    }

                    if ((data.InputData[i].Dates[timestep].TimeOfDay >= closeTime && data.InputData[i].Dates[timestep - 1].TimeOfDay < closeTime))
                    {
                        prevclose = ltp[timestep];
                    }

                    if ((data.InputData[i].Dates[timestep - 1].TimeOfDay < closeTime && data.InputData[i].Dates[timestep - 1].Date != data.InputData[i].Dates[timestep].Date))
                    {
                        prevclose = ltp[timestep - 1];
                    }

                    if ((data.InputData[i].Dates[timestep].TimeOfDay >= openTime && data.InputData[i].Dates[timestep - 1].TimeOfDay < openTime) ||
                        (data.InputData[i].Dates[timestep].TimeOfDay >= openTime && data.InputData[i].Dates[timestep - 1].Date != data.InputData[i].Dates[timestep].Date))
                    {
                        gap = prevclose == 0 ? 0 : Math.Log(ltp[timestep] / prevclose);
                    }


                    if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdSquareOffTime && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                    }

                    if (data.InputData[i].Dates[timestep].TimeOfDay >= start && data.InputData[i].Dates[timestep].TimeOfDay <= end)
                    {
                        if (data.InputData[i].Dates[timestep].TimeOfDay >= start && data.InputData[i].Dates[timestep - 1].TimeOfDay < start)
                        {
                            int ctr = timestep;

                            List<double> highPrices = new List<double>();
                            List<double> lowPrices = new List<double>();

                            while (data.InputData[i].Dates[ctr].Date == data.InputData[i].Dates[ctr - 1].Date && ctr > 1)
                            {
                                highPrices.Add(high[ctr]);
                                lowPrices.Add(low[ctr]);
                                ctr--;
                            }
                            highPrices.Add(high[ctr]);
                            lowPrices.Add(low[ctr]);

                            maxP = highPrices.Max();
                            minP = lowPrices.Min();
                        }

                        if (gap >= gth && ltp[timestep] > maxP)
                            trade = 1;
                        if (gap <= -gth && ltp[timestep] < minP)
                            trade = -1;

                        if (trade == 1 && np[timestep - 1] != +1 && daytrdcount == 0)
                        {
                            sig[timestep] = +2;
                            np[timestep] = +1;
                            daytrdcount = 1;
                        }

                        if (trade == -1 && np[timestep - 1] != -1 && daytrdcount == 0)
                        {
                            sig[timestep] = -2;
                            np[timestep] = -1;
                            daytrdcount = 1;
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





















