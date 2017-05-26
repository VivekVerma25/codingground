using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using CommonLib;

namespace StrategyCollection
{
    public class MovePerUnitTime : BasicStrategy
    {
        public object TradeStartTime = 11;
        public object TradeEndTime = 11.25;
        public object TradeSquareOffTime = 15.25;
        public object RefTime = 9.5;
        public object Lookback = 3;
        public object Lookback2 = 600;
        public object Mult = 1;
        public object Mode = "A"; //"A", "L", "S"
        public object EC = 1;

        public MovePerUnitTime(string stratName, double alloc, double cost, double timeStep)
            : base(stratName, alloc, cost, timeStep)
        {

        }

        public override void RunStrategy(StrategyData data)
        {
            int numSec = data.InputData.Count;
            int lbk = Convert.ToInt32(Lookback);
            int lbk2 = Convert.ToInt32(Lookback2);
            double mult = Convert.ToDouble(Mult);
            double ec = Convert.ToDouble(EC);
            string mode = Convert.ToString(Mode);
            
            TimeSpan TrdEntryStartTime = DateTime.FromOADate(Convert.ToDouble(TradeStartTime) / 24.0).TimeOfDay;
            TimeSpan TrdEntryEndTime = DateTime.FromOADate(Convert.ToDouble(TradeEndTime) / 24.0).TimeOfDay;
            TimeSpan TrdSquareOffTime = DateTime.FromOADate(Convert.ToDouble(TradeSquareOffTime) / 24.0).TimeOfDay;

            for (int i = 0; i < numSec; i++)
            {
                int len = data.InputData[i].Dates.Length;
                double[] ltp = data.InputData[i].Prices;
                double[] vol = data.InputData[i].Extra1;

                double[] sig = new double[len];
                double[] np = new double[len];

                double tradecountLONG = 0;
                double tradecountSHORT = 0;

                List<double> Move = new List<double>();

                double longlevel = -999999999;
                double shortlevel = 999999999;

                double high = 999999999;
                double low = -999999999;

                for (int timestep = 1; timestep < (len); timestep++)
                {
                    if (data.InputData[i].Dates[timestep].Date != data.InputData[i].Dates[timestep - 1].Date)
                    {
                        tradecountLONG = 0;
                        tradecountSHORT = 0;

                        longlevel = -999999999;
                        shortlevel = 999999999;

                        if(Move.Count() > lbk2)
                        {
                            double[] series = Move.ToArray();
                            double[] newseries = UF.GetRange(series, series.Length - lbk2, series.Length - 1);

                            longlevel = newseries.Average() - (mult * UF.StandardDeviation(newseries));
                            shortlevel = newseries.Average() + (mult * UF.StandardDeviation(newseries));

                            int ctr = timestep - 1;

                            List<double> Pr = new List<double>();

                            while (data.InputData[i].Dates[ctr].Date == data.InputData[i].Dates[ctr - 1].Date && ctr > 0)
                            {
                                Pr.Add(ltp[ctr]);
                                ctr--;
                            }

                            high = Pr.Max();
                            low = Pr.Min();
                        }

                    }

                    if (timestep - lbk > 0 && data.InputData[i].Dates[timestep].Date == data.InputData[i].Dates[timestep - lbk].Date)
                    {
                        double currentmove = Math.Log(ltp[timestep] / ltp[timestep - lbk]) / lbk;

                        double metric = (high - ltp[timestep]) / (high - low);

                        if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdEntryStartTime && data.InputData[i].Dates[timestep].TimeOfDay < TrdEntryEndTime)
                        {
                            Move.Add(currentmove);

                            if (currentmove <= longlevel && np[timestep - 1] != 1 && tradecountLONG == 0 && metric >= ec && (mode == "A" || mode == "L"))
                            {
                                sig[timestep] = +2;
                                np[timestep] = +1;
                                tradecountLONG++;

                            }

                            if (currentmove >= shortlevel && np[timestep - 1] != -1 && tradecountSHORT == 0 && metric <= 1 - ec && (mode == "A" || mode == "S"))
                            {
                                sig[timestep] = -2;
                                np[timestep] = -1;
                                tradecountSHORT++;

                            }


                        }
                            
                    }

                    if (data.InputData[i].Dates[timestep].TimeOfDay >= TrdSquareOffTime && np[timestep - 1] != 0)
                    {
                        sig[timestep] = -np[timestep - 1];
                        np[timestep] = 0;
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

