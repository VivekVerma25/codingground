using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;
using System.Reflection;

namespace StrategyCollection
{
    public class IdxOGDynamic_ticks : BasicStrategy
    {
        public object StartTimeTicks = 4;
        public object EndTimeTicks = 12;
        public object TradeSquareOffTicks = 3;
        public object BODTimeTicks = 0;
        public object EODTimeTicks = 3;
        public object gapThreshold = 0.005;
        public object NumdaysAverage = 10;
        public object PercentileCutoff = 0.5;
        public object MaxHoldTicks = 100;
        private double Alloc = 0;


        public IdxOGDynamic_ticks(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {
            
        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;

            int start = Convert.ToInt32(StartTimeTicks) + 1;
            int end = Convert.ToInt32(EndTimeTicks) + 1;

            double gth = Convert.ToDouble(gapThreshold);
            double prevclose = 0;

            int TrdSquareOffTime = Convert.ToInt32(TradeSquareOffTicks) + 1;
            int openTime = Convert.ToInt32(BODTimeTicks);
            int closeTime = Convert.ToInt32(EODTimeTicks);

            int avglbk = Convert.ToInt32(NumdaysAverage);
            double pc = Convert.ToDouble(PercentileCutoff);
            int mhticks = Convert.ToInt32(MaxHoldTicks);

            for (int i = 0; i < numSec; i++)
            {
                DateTime[] Date = data.InputData[i].Dates;
                long len = Date.Length;

                double[] ltp = data.InputData[i].Prices;

                double[] high = data.InputData[i].OHLC.high;
                double[] low = data.InputData[i].OHLC.low;

                double[] sig = new double[len];
                double[] np = new double[len];

                double ticks = 0;
                double gap = 0;
                double avggap = 0;
                double trade = 0;
                double daytrdcount = 0;

                double maxP = 99999999999;
                double minP = -99999999999;
                double dayhigh = double.MaxValue;
                double daylow = double.MinValue;
                double pdr = 0;
                int dur = 0;

                List<double> gaplist = new List<double>();

                for (int timestep = 1; timestep < len - end; timestep++)
                {
                    ticks++;
                    dur++;

                    if (data.InputData[i].Dates[timestep].Date != data.InputData[i].Dates[timestep - 1].Date)
                    {
                        ticks = 0;
                        trade = 0;
                        gap = prevclose == 0 ? 0 : Math.Log(ltp[timestep] / prevclose);
                        daytrdcount = 0;
                        np[timestep - 1] = 0;
                        pdr = daylow != 0 ? (dayhigh - daylow) * 2 / (dayhigh + daylow) : 0;
                        dayhigh = high[timestep];
                        daylow = low[timestep];
                        dur = 0;
                    }
                    else
                    {
                        dayhigh = Math.Max(dayhigh, high[timestep]);
                        daylow = Math.Min(daylow, low[timestep]);
                    }

                    if ((Date[timestep + closeTime].Date != Date[timestep].Date && Date[timestep - 1 + closeTime].Date == Date[timestep - 1].Date))
                    {
                        prevclose = ltp[timestep];
                    }

                    if ((closeTime == 0 && data.InputData[i].Dates[timestep - 1].Date != data.InputData[i].Dates[timestep].Date))
                    {
                        prevclose = ltp[timestep - 1];
                    }

                    if ((ticks == openTime))
                    {
                        gap = prevclose == 0 ? 0 : Math.Log(ltp[timestep] / prevclose);

                        if (gaplist.Count > avglbk)
                            avggap = UF.Percentile(gaplist.Skip(gaplist.Count - avglbk).ToArray(), pc);

                        //if (pdr != 0)
                        //    gaplist.Add(Math.Abs(gap) / pdr);
                        // else gaplist.Add(0);

                        gaplist.Add(Math.Abs(gap));
                    }


                    if (Date[timestep + TrdSquareOffTime].Date != Date[timestep].Date && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                    }

                    if (dur > mhticks && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        dur = 0;
                    }



                    if (ticks >= start && Date[timestep + end].Date == Date[timestep].Date)
                    {
                        if (ticks == start)
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

                        if (gap >= Math.Max(avggap, gth) && ltp[timestep] > maxP)
                            trade = 1;
                        if (gap <= -Math.Max(avggap, gth) && ltp[timestep] < minP)
                            trade = -1;

                        if (trade == 1 && np[timestep - 1] != +1 && daytrdcount == 0)
                        {
                            sig[timestep] = +2;
                            np[timestep] = +1;
                            daytrdcount = 1;
                            dur = 0;
                        }

                        if (trade == -1 && np[timestep - 1] != -1 && daytrdcount == 0)
                        {
                            sig[timestep] = -2;
                            np[timestep] = -1;
                            daytrdcount = 1;
                            dur = 0;
                        }
                    }

                    if (sig[timestep] == 0)
                        np[timestep] = np[timestep - 1];


                }
                base.CalculateNetPosition(data, sig, i, 1.5, -1.5, -0.5, 0.5);

                //base.CalculateNetPosition(data.InputDataArray[i], data.SecName[i], sig, allocation, i, 1.5, -1.5, -0.5, 0.5);
            }

            base.RunStrategyBase(data);
        }
    }
}





















