# source("funs.R")
# library(rvest)
# library(PortfolioAnalytics)
# 
# #### Define Portfolio ####
# portCodes = c("107745" , "119205" , "118539")
# weights = c(0.2,0.2,0.6)


#### Historic Portfolio Performance ####
getHistoricPerf <- function(mfNames , weights){
schemeCodes = getAllSchemeMFCodes()
portDF = schemeCodes[schemeCodes$`Scheme Name` %in% mfNames,]
allNAV = list()

for( i in 1:(length(mfNames))){
  dataTemp = getHistNAV(mfcode = portDF$Code[i],scmCode = portDF$`Scheme Code`[i],startDate = as.Date("1995-01-01"), endDate = Sys.Date())
  allNAV = c(allNAV , list(dataTemp))  
}

names(allNAV) = mfNames
allNAVZoo = do.call("merge",lapply( allNAV , function(x) zoo(x = x$nav,order.by = x$date) ))
returnZoo = Return.calculate(allNAVZoo)

returnZoo = returnZoo[max(apply(returnZoo ,2, function(x) ( min( which(!is.na(x)) ) ))) : nrow(returnZoo),]

pf_rebal <- Return.portfolio(returnZoo, weights = weights, rebalance_on = "months", verbose = TRUE )

mean_return = mean(pf_rebal$returns , na.rm = T)
sd_return = sd(pf_rebal$returns , na.rm = T)

plot_historic = plot(cumprod(1 + pf_rebal$returns) ,main = "Historic Performance")

return(list(mean_return,sd_return,plot_historic))
}

# #### Portfolio Projections ####
# n = 5 # Holding period in years
# 
# nDays = (1:(n*252))
# nYears = nDays/252
# projectedAvgRet = (1 +mean_return)^nDays
# projecctedSd = sqrt((1 + (sd_return)^2)^nDays - 1)
# 
# predFrame = data.frame(years = nYears , return = projectedAvgRet , yhigh = projectedAvgRet + projecctedSd, ylow = projectedAvgRet - projecctedSd)
# 
# pl = ggplot(predFrame , aes(x = years , y = return)) + geom_line() + 
#   geom_ribbon(data = predFrame , aes(ymin = ylow , ymax = yhigh) , alpha = 0.3)
# 
# 

