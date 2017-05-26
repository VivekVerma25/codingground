using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class Vivek_NSEAD : BasicStrategy
    {
        public object TradeStartTime = 9.78;
        public object TradeEndTime = 15.5;
        public object TradeSquareOff = 15.5;
        public object ADMult = 0.2;
        public object ADSqMult = 0.2;
        public object Lookback = 2;
        public object Lag = 0;
        public object AdLim1 = 0.05;
        public object AdLim2 = 0.05;
        public object LONGFlag = true;
        public object SHORTFlag = true;

        public Vivek_NSEAD(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            double adm = Convert.ToDouble(ADMult);
            double adsqm = Convert.ToDouble(ADSqMult);
            double adl1 = Convert.ToDouble(AdLim1);
            double adl2 = Convert.ToDouble(AdLim2);
            int lag = Convert.ToInt32(Lag);
            int lbk = Convert.ToInt32(Lookback);
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

                double openad = 0;
                double openad2 = 0;
                double timecounter = 0;

                List<double> Move1 = new List<double>();
                double[] series1 = new double[0];
                double[] newseries1 = new double[0];

                List<double> Move2 = new List<double>();
                double[] series2 = new double[0];
                double[] newseries2 = new double[0];

                for (int j = (lag + 1); j < (ltp.Length - 1); j++)
                {
                    timecounter++;

                    if (data.InputData[i].Dates[j].Date != data.InputData[i].Dates[j - 1].Date)
                    {
                        openad = (ad[j] + ad[j + 1] + ad[j + 2]) / 3;
                        timecounter = 0;
                        series1 = Move1.ToArray();

                        Move2.Add((series1.Max() - series1.Min()));

                        Move1 = new List<double>();

                        if (Move2.Count() >= lbk)
                        {
                            series2 = Move2.ToArray();
                            newseries2 = UF.GetRange(series2, series2.Length - lbk, series2.Length - 1);
                        }



                    }

                    double diff1 = ad[j - lag] - openad;
                    double diff3 = ad[j - lag] - openad2;

                    Move1.Add(ad[j]);

                    double currentad = ad[j - lag];

                    if (data.InputData[i].Dates[j].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[j].TimeOfDay <= TrdEntryEndTime)
                    {
                        
                        
                            if (diff1 > Math.Min(Math.Max(adm * (timecounter / 75) * 0.5351412885993357, adl1), adl2) && longflag == true)
                            {
                                sig[j] = +2;
                                np[j] = +1;
                                openad2 = ad[j];

                            }

                            if (diff1 < -Math.Min(Math.Max(adm * (timecounter / 75) * 0.5351412885993357, adl1), adl2) && shortflag == true)
                            {
                                sig[j] = -2;
                                np[j] = -1;
                                openad2 = ad[j];
                            }
                        
                    }

                    if ((np[j - 1] == 1 && diff3 <= -Math.Min(Math.Max(adsqm * 0.5351412885993357, adl1), adl2)) || (np[j - 1] == -1 && diff3 >= Math.Min(Math.Max(adsqm * 0.5351412885993357, adl1), adl2)))
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