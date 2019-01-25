source("funs.R")
library(rvest)
library(PortfolioAnalytics)
portCodes = c("107745" , "119205" , "118539")
weights = c(0.2,0.2,0.6)

schemeCodes = getAllSchemeMFCodes()

portDF = schemeCodes[schemeCodes$`Scheme Code` %in% portCodes,]

allNAV = list()

for( i in 1:(length(portCodes))){
  dataTemp = getHistNAV(mfcode = portDF$Code[i],scmCode = portDF$`Scheme Code`[i],startDate = as.Date("1995-01-01"), endDate = Sys.Date())
  allNAV = c(allNAV , list(dataTemp))  
}

names(allNAV) = portCodes

allNAVZoo = do.call("merge",lapply( allNAV , function(x) zoo(x = x$nav,order.by = x$date) ))

returnZoo = Return.calculate(allNAVZoo)

pf_rebal <- Return.portfolio(returnZoo, weights = weights, rebalance_on = "months", verbose = TRUE )

plot(pf_rebal$returns)
