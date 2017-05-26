using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class ADRatio_VariableTime : BasicStrategy
    {
        public object TradeStartTime = 9.5;
        public object TradeEndTime1 = 10.5;
        public object TradeEndTime2 = 12;
        public object TradeSquareOff = 15.4;
        public object ADMult = 1;
        public object ADSqMult = -0.1;
        public object GAP = -100;
        public object ADCutoffLONG = -100;
        public object ADCutoffSHORT = 100;
        public object Lookback1 = 5;
        public object Lookback2 = 6;
        public object Lag = 0;
        public object Fwd = 0;
        public object LONGFlag = true;
        public object SHORTFlag = true;

        public ADRatio_VariableTime(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double adm = Convert.ToDouble(ADMult);
            double adsqm = Convert.ToDouble(ADSqMult);
            int lbk1 = Convert.ToInt32(Lookback1);
            int lbk2 = Convert.ToInt32(Lookback2);
            double adcl = Convert.ToDouble(ADCutoffLONG);
            double gap = Convert.ToDouble(GAP);
            double adcs = Convert.ToDouble(ADCutoffSHORT);
            int lag = Convert.ToInt32(Lag);
            int fwd = Convert.ToInt32(Fwd);
            Boolean longflag = Convert.ToBoolean(LONGFlag);
            Boolean shortflag = Convert.ToBoolean(SHORTFlag);

            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime1 = DateTime.FromOADate(Convert.ToDouble(TradeEndTime1) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime2 = DateTime.FromOADate(Convert.ToDouble(TradeEndTime2) / 24.0).TimeOfDay;

            TimeSpan TrdSqOff = DateTime.FromOADate(Convert.ToDouble(TradeSquareOff) / 24.0).TimeOfDay;

            for (int i = 0; i < numSec; i++)
            {
                double[] ltp = data.InputData[i].Prices;
                double[] ad = data.InputData[i].Extra1;

                double[] sig = new double[ltp.Length];
                double[] np = new double[ltp.Length];
                double timecounter = 0;
                double diff2 = -50;

                double openad = 0;
                double move = 0;
                List<double> Move1 = new List<double>();
                double[] series1 = new double[0];
                double[] newseries1 = new double[0];
                
                for (int j = (lag + 1); j < (ltp.Length - 1); j++)
                {
                    timecounter++;

                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j - 1].Date)
                    {
                        openad = (ad[j + fwd] + ad[j + fwd + 1] + ad[j + fwd + 2]) / 3;
                        move = (openad - ad[j - 1]);
                        Move1.Add(Math.Abs(move));

                        if (Move1.Count() >= lbk1)
                        {
                            series1 = Move1.ToArray();
                            newseries1 = UF.GetRange(series1, series1.Length - lbk1, series1.Length - 1);
                        }

                    }
                    if (timecounter > lbk2)
                    {
                        diff2 = ad[j - lag] - ad[j - lag - lbk2];

                    }
                    double diff = ad[j - lag] - openad;
                    double currentad = ad[j - lag];
                    if (series1.Length >= lbk1)
                    {
                        if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime1)
                        {




                            if (diff > (adm+Math.Max(0.1, 0.25*newseries1.Average())) && longflag == true && move < gap)
                            {
                                sig[j] = +2;
                                np[j] = +1;
                            }

                            if (diff < -(adm+Math.Max(0.1, 0.25 * newseries1.Average())) && shortflag == true && move > -gap)
                            {
                                sig[j] = -2;
                                np[j] = -1;
                            }

                        }
                    }
                    /*if (data.InputData[i].Dates[j].TimeOfDay > TrdEntryEndTime1 && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime2)
                    {




                        if (diff > adm * 0.333 && longflag == true)
                        {
                            sig[j] = +2;
                            np[j] = +1;
                        }

                        if (diff < -adm *0.333 && shortflag == true)
                        {
                            sig[j] = -2;
                            np[j] = -1;
                        }
                    }*/



                    if ((np[j - 1] == 1 && diff2 < adsqm) || (np[j - 1] == -1 && diff2 > -adsqm))
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