using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class RSI_AD_lag : BasicStrategy
    {
        public object TradeStartTime = 9.25;
        public object TradeEndTime = 12;
        public object TradeSquareOff = 14.75;
        public object RSILength = 30;
        public object Thresh = 30.0;
        public object SqOff = 0;
        public object Lag = 0;
        public object PriceCutoff = -1000000;

        public RSI_AD_lag(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double thresh = Convert.ToDouble(Thresh);
            double so = Convert.ToDouble(SqOff);
            int tmaP = Convert.ToInt32(RSILength);
            int lag = Convert.ToInt32(Lag);
            double pco = Convert.ToDouble(PriceCutoff);

            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime = DateTime.FromOADate(Convert.ToDouble(TradeEndTime) / 24.0).TimeOfDay;

            TimeSpan TrdSqOff = DateTime.FromOADate(Convert.ToDouble(TradeSquareOff) / 24.0).TimeOfDay;

            for (int i = 0; i < numSec; i++)
            {

                double[] nifty = data.InputData[i].Extra1;

                double[] bar = new double[nifty.Length];
                double[] uptick = new double[nifty.Length];
                double[] downtick = new double[nifty.Length];
                double[] upexpavg = new double[nifty.Length];
                double[] downexpavg = new double[nifty.Length];
                double[] RS = new double[nifty.Length];
                double[] RSI = new double[nifty.Length];
                double[] sig = new double[nifty.Length];
                double[] np = new double[nifty.Length];

                int longctr = 0;
                int shortctr = 0;


                for (int j = (1+lag); j < (nifty.Length); j++)
                {
                    //bar[j] = nifty[j]/nifty[j - 1] - 1;

                    bar[j] = nifty[j-lag] - nifty[j - 1-lag];

                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdSqOff && np[j - 1] != 0)
                    {
                        sig[j] = -np[j - 1];
                        np[j] = 0;
                    }

                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j - 1].Date)
                    {
                        longctr = 0;
                        shortctr = 0;
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

                        upexpavg[j] = UF.GetRange(uptick, j - tmaP + 1, j).Average();
                        downexpavg[j] = UF.GetRange(downtick, j - tmaP + 1, j).Average();
                        RS[j] = upexpavg[j] / downexpavg[j];
                        RSI[j] = 100 - (100 / (1 + RS[j]));

                        if (data.InputData[i].Dates[j].TimeOfDay < TrdSqOff && np[j - 1] != 0)
                        {
                            if ((np[j - 1] == 1 && RSI[j] <= so) || (np[j - 1] == -1 && RSI[j] >= 100 - so))
                            {
                                sig[j] = -np[j - 1];
                                np[j] = 0;
                            }
                        }



                        if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime && RSI[j] <= thresh && np[j - 1] != -1 && shortctr <= 100)
                        {
                            sig[j] = -2;
                            np[j] = -1;
                            shortctr++;
                        }

                        if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime && RSI[j] >= 100 - thresh && np[j - 1] != 1 && longctr <= 100)
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