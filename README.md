# quantInvest
A quant investing tool built in R. A nascent version can be found at https://dhruvnigam.shinyapps.io/quantinvest/. 

## Understanding you MF portfolio quantitatively
In investing, it is essential too be able to look at the performance of your portfolio as a whole. At the time of writing this, there are no popular tools *I could find* available for a retail investor to be able to do that. The main objectives for this project are to enable investors to- 

1. analyze historic performance of their mutual fund portfolios by looking at key performance metrics and assess if their currect portfolio is fit to achieve their goals
3. use modern portfolio theory, employed by professional money managers, to come up with an optimal portfolio to match their return expectations and risk tolerance

## Historic portfolio performance 
This tool is mean to simulated your investment strategy using past data. Although pas performance can be misleading in predicting future returns, it is still informative to look at long run charectreistics like 
* average return over a benchmark - although this is often misleading - consistent and significant outperformance or relative to index can be very useful
* volatility, which quantifies the variability of returns you can expect from the portfolio
* average drawdown - gives you an idea of how long an average your invesment can go down from a peak, before recovering.
* Shape ratio - a measure incorporating both return and volatility 

The investment strategy itself can be one of two popular ones-

* Lumpsum investment - putting in all the money in the chosen funds at once
* Systematic Investment Plan - putting in a fixed amonth of money every month in the choden funds

An additional decision that has to be taken w.r.t the invetment strategy is regarding periodically rebalancing the pprtfolio i.e. perdiodically shuffling your invesments to make sure that your relativa allocations stick to a pre-decided profile (Eg. 60% Equity, 40% Debt) . The choices available regardin rebalancing are - 

* Monthly
* Yearly
* Never
