using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class TMAwithStdevBand : BasicStrategy
    {
        public object TMALength = 100;
        //public object LTTMALength = 500;
        public object StdevBand_Entry = 3.0;
        public object StdevBand_Exit = 1.0;

        public object StartTime1 = 9.5;
        public object EndTime1 = 14.5;
        public object ExitTime = 15.25;

        public TMAwithStdevBand(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;

            int tmaP = Convert.ToInt32(TMALength);
            //int tmaLT = Convert.ToInt32(LTTMALength);
            double pband1 = Convert.ToDouble(StdevBand_Entry);
            double pband2 = Convert.ToDouble(StdevBand_Exit);
            TimeSpan startTime1 = DateTime.FromOADate(Convert.ToDouble(StartTime1) / 24.0).TimeOfDay;
            TimeSpan endTime1 = DateTime.FromOADate(Convert.ToDouble(EndTime1) / 24.0).TimeOfDay;
            TimeSpan exitTime = DateTime.FromOADate(Convert.ToDouble(ExitTime) / 24.0).TimeOfDay;
            

            for (int i = 0; i < numSec; i++)
            {
                double[] ltp = data.InputData[i].Prices;

                //double[] tma = Technicals.MovAvg(ltp, tmaP);
                List<double[]> bands1 = Technicals.BollingerBand(ltp, tmaP, pband1);
                List<double[]> bands2 = Technicals.BollingerBand(ltp, tmaP, pband2);
                
                double[] tma1 = bands1[0];
                double[] uband1 = bands1[1];
                double[] lband1 = bands1[2];

                double[] tma2= bands2[0];
                double[] uband2 = bands2[1];
                double[] lband2 = bands2[2];
                
                

                //List<double[]> temp = new List<double[]>();
                //temp.Add(nifty);
                //temp.Add(tmafilter);
                //FileWrite.Write(temp, "dump.csv", false, false);

                double[] sig = new double[tma1.Length];
                double[] np = new double[tma1.Length];

                for (int j = tmaP; j < tma1.Length; j++)
                {

                    if (data.InputData[i].Dates[j].TimeOfDay <= startTime1)
                        np[j] = 0;
                    else if (data.InputData[i].Dates[j].TimeOfDay > startTime1
                        && data.InputData[i].Dates[j].TimeOfDay < endTime1)
                    {
                        if (ltp[j] > uband1[j]
                            && ltp[j - 1] < uband1[j - 1])
                        {
                            sig[j] = 2;
                            np[j] = 1;
                        }

                        else if (ltp[j] < lband1[j]
                            && ltp[j - 1] > lband1[j - 1])
                        {
                            sig[j] = -2;
                            np[j] = -1;
                        }
                        else np[j] = np[j - 1];
                    }
                    else if((np[j - 1] == 1 && ltp[j] < uband2[j]
                            && ltp[j - 1] > uband2[j - 1]) || (np[j - 1] == -1 && ltp[j] > lband2[j]
                            && ltp[j - 1] < lband2[j - 1]))
                    {
                        sig[j] = -np[j - 1];
                        np[j] = 0;
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
