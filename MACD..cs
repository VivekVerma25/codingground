using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class MACD : BasicStrategy
    {
        public object TMALength = 100;
        //public object LTTMALength = 500;
        public object StdevBand = 1.0;

        public object StartTime1 = 9.5;
        public object EndTime1 = 14.5;
        public object ExitTime = 15.25;

        public MACD(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            
            int tmaP = Convert.ToInt32(TMALength);
            //int tmaLT = Convert.ToInt32(LTTMALength);
            double pband = Convert.ToDouble(StdevBand);

            TimeSpan startTime1 = DateTime.FromOADate(Convert.ToDouble(StartTime1) / 24.0).TimeOfDay;
            TimeSpan endTime1 = DateTime.FromOADate(Convert.ToDouble(EndTime1) / 24.0).TimeOfDay;
            TimeSpan exitTime = DateTime.FromOADate(Convert.ToDouble(ExitTime) / 24.0).TimeOfDay;


            for (int i = 0; i < numSec; i++)
            {
                double[] ltp = data.InputData[i].Prices;

                //double[] tma = Technicals.MovAvg(ltp, tmaP);
                List<double[]> bands = Technicals.BollingerBand(ltp, tmaP, pband);
                
                double[] tma = bands[0];
                double[] uband = bands[1];
                double[] lband = bands[2];


                //List<double[]> temp = new List<double[]>();
                //temp.Add(nifty);
                //temp.Add(tmafilter);
                //FileWrite.Write(temp, "dump.csv", false, false);

                double[] sig = new double[tma.Length];
                double[] np = new double[tma.Length];

                for (int j = tmaP; j < tma.Length; j++)
                {

                    if (data.InputData[i].Dates[j].TimeOfDay <= startTime1)
                        np[j] = 0;
                    else if (data.InputData[i].Dates[j].TimeOfDay > startTime1
                        && data.InputData[i].Dates[j].TimeOfDay < endTime1)
                    {
                        if (ltp[j] > uband[j]
                            && ltp[j - 1] < uband[j - 1])
                        {
                            sig[j] = 2;
                            np[j] = 1;
                        }

                        else if (ltp[j] < lband[j]
                            && ltp[j - 1] > lband[j - 1])
                        {
                            sig[j] = -2;
                            np[j] = -1;
                        }
                        else np[j] = np[j - 1];
                    }
                    
                    else if (data.InputData[i].Dates[j].TimeOfDay < exitTime)
                        np[j] = np[j - 1];
                    else
                    {
                        if (np[j - 1] != 0)
                            sig[j] = np[j - 1] > 0 ? -1 : 1;

                        np[j] = 0;
                    }



                }

                base.CalculateNetPosition(data, sig, i, 1.5, -1.5, -0.5, 0.5);
            }

            base.RunStrategyBase(data);
        }
    }
}
