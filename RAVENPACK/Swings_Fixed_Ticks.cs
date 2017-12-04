using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;
using System.Reflection;

namespace StrategyCollection
{
    public class Swings_Fixed_Ticks : BasicStrategy
    {
        public object TradeStartTicks = 6;
        public object TradeEndTicks = 6;
        public object TradeSquareOffTicks = 3;
        public object SwingSize = 0.005;
        public object EntryMultiplier = 0.0;
        public object MaxSwingMult = 3;
        public object UseHighLow = 0;
        public object MaxSwingTrades = 10;
        public object MaxHoldTicks = 500;
        private double Alloc = 0;



        public Swings_Fixed_Ticks(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {
            Alloc = alloc;
        }




        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double ss = Convert.ToDouble(SwingSize);
            double mult = Convert.ToDouble(EntryMultiplier);
            double maxswmult = Convert.ToDouble(MaxSwingMult);
            bool usehl = Convert.ToBoolean(Convert.ToDouble(UseHighLow));
            int mst = Convert.ToInt32(MaxSwingTrades);

            int TrdEntryStartTime = Convert.ToInt32(TradeStartTicks) - 1;
            int TrdEntryEndTime = Convert.ToInt32(TradeEndTicks) + 1;
            int TrdSquareOffTime = Convert.ToInt32(TradeSquareOffTicks) + 1;

            int mhticks = Convert.ToInt32(MaxHoldTicks);

            for (int i = 0; i < numSec; i++)
            {

                DateTime[] Date = data.InputData[i].Dates;
                long len = Date.Length;

                double[] ltp = data.InputData[i].Prices;
                double[] highpx = data.InputData[i].OHLC.high;
                double[] lowpx = data.InputData[i].OHLC.low;

                double[] sig = new double[len];
                double[] np = new double[len];

                double max = 0;
                double min = double.MaxValue;
                //double[] swinghighlist = new double[len];
                //double[] swinglowlist = new double[len];
                double swinghigh = double.MaxValue;
                double swinglow = 0;
                bool newhigh = true;
                bool newlow = true;
                double lastswingsize = 0, currentswingtrd = 0;
                double dayopen = 0, dayhigh = 0, daylow = double.MaxValue, gap = 0;
                bool longdayflag = false, shortdayflag = false;
                int ticks = 0;
                int dur = 0;

                for (int timestep = 1; timestep < (len - TrdEntryEndTime); timestep++)
                {
                    ticks++;
                    dur++;


                    # region Swing Construction

                    if (timestep == 1)
                    {
                        if (usehl)
                        {
                            max = highpx[timestep];
                            min = lowpx[timestep];
                        }
                        else
                        {
                            max = ltp[timestep];
                            min = ltp[timestep];
                        }

                        swinghigh = double.MaxValue;
                        swinglow = 0;
                        //swinghigh[timestep] = double.MaxValue;
                        //swinglow[timestep] = 0;
                    }
                    else
                    {
                        if (newhigh && ltp[timestep] <= max * (1 - ss))
                        {
                            //swinghigh[timestep] = max;
                            //swinglow[timestep] = swinglow[timestep - 1];
                            swinghigh = max;
                            if (usehl)
                            {
                                max = highpx[timestep];
                                min = lowpx[timestep];
                            }
                            else
                            {
                                max = ltp[timestep];
                                min = ltp[timestep];
                            }
                            newhigh = false;
                            newlow = true;
                            currentswingtrd = 0;
                            shortdayflag = false;
                        }
                        else if (newlow && ltp[timestep] >= min * (1 + ss))
                        {
                            //swinglow[timestep] = min;
                            //swinghigh[timestep] = swinghigh[timestep - 1];
                            swinglow = min;
                            if (usehl)
                            {
                                max = highpx[timestep];
                                min = lowpx[timestep];
                            }
                            else
                            {
                                max = ltp[timestep];
                                min = ltp[timestep];
                            }
                            newhigh = true;
                            newlow = false;
                            currentswingtrd = 0;
                            longdayflag = false;
                        }
                        else
                        {
                            if (usehl)
                            {
                                max = Math.Max(highpx[timestep], max);
                                min = Math.Min(lowpx[timestep], min);
                            }
                            else
                            {
                                max = Math.Max(ltp[timestep], max);
                                min = Math.Min(ltp[timestep], min);
                            }
                            //swinghigh[timestep] = swinghigh[timestep - 1];
                            //swinglow[timestep] = swinglow[timestep - 1];
                        }

                    }

                    # endregion

                    if (swinglow != 0 && swinghigh < double.MaxValue / 2)
                    {
                        lastswingsize = Math.Abs((swinghigh - swinglow) * 2 / (swinghigh + swinglow));
                    }
                    else lastswingsize = double.MaxValue;

                    if (Date[timestep].Date != Date[timestep - 1].Date)
                    {
                        dayopen = ltp[timestep];
                        dayhigh = usehl ? highpx[timestep] : ltp[timestep];
                        daylow = usehl ? lowpx[timestep] : ltp[timestep];
                        gap = dayopen / ltp[timestep - 1] - 1;
                        shortdayflag = false;
                        longdayflag = false;
                        np[timestep - 1] = 0;
                        ticks = 0;
                        dur = 0;                     }
                    else
                    {
                        dayhigh = Math.Max(dayhigh, usehl ? highpx[timestep] : ltp[timestep]);
                        daylow = Math.Min(daylow, usehl ? lowpx[timestep] : ltp[timestep]);
                    }

                    if (ticks < TrdEntryStartTime)
                    {
                        if (ltp[timestep] > swinglow * (1 + mult * lastswingsize) && newhigh && lastswingsize <= maxswmult * ss && currentswingtrd == 0)
                        {
                            longdayflag = true;
                        }
                        else if (ltp[timestep] < swinghigh * (1 - mult * lastswingsize) && newlow && lastswingsize <= maxswmult * ss && currentswingtrd == 0)
                        {
                            shortdayflag = true;
                        }
                    }

                    if (ticks >= TrdEntryStartTime && data.InputData[i].Dates[timestep + TrdEntryEndTime].Date == data.InputData[i].Dates[timestep].Date)
                    {
                        if (ltp[timestep] > swinglow * (1 + mult * lastswingsize) && newhigh && lastswingsize <= maxswmult * ss && currentswingtrd == 0)
                        {
                            sig[timestep] = 2;
                            np[timestep] = 1;
                            currentswingtrd++;
                            dur = 0;
                        }
                        else if (ltp[timestep] < swinghigh * (1 - mult * lastswingsize) && newlow && lastswingsize <= maxswmult * ss && currentswingtrd == 0)
                        {
                            sig[timestep] = -2;
                            np[timestep] = -1;
                            currentswingtrd++;
                            dur = 0;
                        }
                        else if (ltp[timestep] > swinghigh && newhigh && lastswingsize <= maxswmult * ss && currentswingtrd <= mst && np[timestep - 1] != 1)
                        {
                            sig[timestep] = +2;
                            np[timestep] = +1;
                            currentswingtrd++;
                            dur = 0;
                        }
                        else if (ltp[timestep] < swinglow && newlow && lastswingsize <= maxswmult * ss && currentswingtrd <= mst && np[timestep - 1] != -1)
                        {
                            sig[timestep] = -2;
                            np[timestep] = -1;
                            currentswingtrd++;
                            dur = 0;
                        }
                    }



                    if (data.InputData[i].Dates[timestep + TrdSquareOffTime].Date != data.InputData[i].Dates[timestep].Date && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        dur = 0;
                    }

                    if (dur > mhticks && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
                        dur = 0;
                    }

                    if (sig[timestep] == 0)
                        np[timestep] = np[timestep - 1];

                    //swinghighlist[timestep] = swinghigh;
                    //swinglowlist[timestep] = swinglow;

                }

                //FileWrite fr = new FileWrite("swingsdata.csv");
                //for (int k = 0; k < len; k++)
                //{
                //    fr.WriteLineAppend(Date[k] + "," + ltp[k] + "," + swinghighlist[k] + "," + swinglowlist[k]);
                //}

                base.CalculateNetPosition(data, sig, i, 1.5, -1.5, -0.5, 0.5);
            }

            base.RunStrategyBase(data);
        }
    }
}


