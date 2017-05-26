using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class RSI2 : BasicStrategy
    {
        public object TradeStartTime = 9.5;
        public object TradeEndTime = 12;
        public object TradeSquareOff = 14.75;
        public object RSILength = 30;
        public object Thresh = 30.0;
        public object SqOff = 0;
        public object PriceCutoff = 20;

        public RSI2(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double thresh = Convert.ToDouble(Thresh);
            double so = Convert.ToDouble(SqOff);
            int tmaP = Convert.ToInt32(RSILength);
            double pco = Convert.ToDouble(PriceCutoff);

            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime = DateTime.FromOADate(Convert.ToDouble(TradeEndTime) / 24.0).TimeOfDay;

            TimeSpan TrdSqOff = DateTime.FromOADate(Convert.ToDouble(TradeSquareOff) / 24.0).TimeOfDay;

            for (int i = 0; i < numSec; i++)
            {

                double[] ltp = data.InputData[i].Prices;

                double[] bar = new double[ltp.Length];
                double[] uptick = new double[ltp.Length];
                double[] downtick = new double[ltp.Length];
                double[] upexpavg = new double[ltp.Length];
                double[] downexpavg = new double[ltp.Length];
                double[] RS = new double[ltp.Length];
                double[] RSI = new double[ltp.Length];
                double[] sig = new double[ltp.Length];
                double[] np = new double[ltp.Length];

                int longctr = 0;
                int shortctr = 0;
                int flag = 1;
            

                for (int j = 1; j < ltp.Length; j++)
                {
                    //bar[j] = ltp[j]/ltp[j - 1] - 1;

                    bar[j] = ltp[j] - ltp[j - 1];

                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdSqOff && np[j - 1] != 0)
                    {
                        sig[j] = -np[j - 1];
                        np[j] = 0;
                    }

                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j - 1].Date)
                    {
                        longctr = 0;
                        shortctr = 0;
                        flag = 1;

                    }

                    if (bar[j] > 0)
                    {
                        uptick[j] = bar[j];
                        downtick[j] = 0;
                    }

                    else if (bar[j] < 0)
                    {
                        uptick[j] = 0;
                        downtick[j] = -bar[j];
                    }

                    else
                    {
                        uptick[j] = 0;
                        downtick[j] = 0;
                    }


                    if (j > tmaP)
                    {

                        if (flag == 1)
                        {
                            upexpavg[j] = UF.GetRange(uptick, j - tmaP + 1, j).Average();


                            downexpavg[j] = UF.GetRange(downtick, j - tmaP + 1, j).Average();
                            RS[j] = upexpavg[j] / downexpavg[j];
                            RSI[j] = 100 - (100 / (1 + RS[j]));
                            flag = 0;
                        }


                        else
                        {
                            upexpavg[j] = ((upexpavg[j - 1]*(tmaP-1)+uptick[j])/tmaP);
                            
                            downexpavg[j] = ((downexpavg[j - 1] * (tmaP - 1) + downtick[j]) / tmaP);
                            RS[j] = upexpavg[j] / downexpavg[j];
                            RSI[j] = 100 - (100 / (1 + RS[j]));
                        }



                        if (data.InputData[i].Dates[j].TimeOfDay < TrdSqOff && np[j - 1] != 0)
                        {
                            if ((np[j - 1] == 1 && RSI[j] <= so) || (np[j - 1] == -1 && RSI[j] >= 100 - so))
                            {
                                sig[j] = -np[j - 1];
                                np[j] = 0;
                            }
                        }



                        if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && ltp[j] >= pco && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime && RSI[j] <= thresh && np[j - 1] != -1 && shortctr <= 100)
                        {
                            sig[j] = -2;
                            np[j] = -1;
                            shortctr++;
                        }

                        if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && ltp[j] >= pco && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime && RSI[j] >= 100 - thresh && np[j - 1] != 1 && longctr <= 100)
                        {
                            sig[j] = 2;
                            np[j] = 1;
                            longctr++;
                        }
                    }

                    if (sig[j] == 0)
                        np[j] = np[j - 1];

                }

                base.CalculateNetPosition(data, sig, i, 1.5, -1.5, -0.5, 0.5);

                //FileWrite opt1 = new FileWrite("bar.csv");
                //opt1.DataWriteOneVar(bar);
                //FileWrite opt2 = new FileWrite("uptick.csv");
                //opt2.DataWriteOneVar(uptick);
                //FileWrite opt3 = new FileWrite("downtick.csv");
                //opt3.DataWriteOneVar(downtick);
                //FileWrite opt4 = new FileWrite("RS.csv");
                //opt4.DataWriteOneVar(RS);
                //FileWrite opt5 = new FileWrite("RSI.csv");
                //opt5.DataWriteOneVar(RSI);
                //FileWrite opt6 = new FileWrite("sig.csv");
                //opt6.DataWriteOneVar(sig);
                //FileWrite opt7 = new FileWrite("np.csv");
                //opt7.DataWriteOneVar(np);
            }

            base.RunStrategyBase(data);

        }

    }
}