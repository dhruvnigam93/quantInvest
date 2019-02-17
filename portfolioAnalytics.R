#### Historic Portfolio Performance ####
getHistoricPerf <- function(mfNames , weights, schemeCodes){
  
  portDF = merge(data.frame(mfNames) , schemeCodes , by.x = "mfNames" , by.y = "Scheme Name")
  print(paste("Getting data from scheme Codes" , paste(portDF$`Scheme Code`, collapse = ",")))
  returnZoo <- getHistoricReturns(portDF)
  pf_rebal <- Return.portfolio(returnZoo, weights = weights, rebalance_on = "months", verbose = TRUE )
  
  mean_return = mean(pf_rebal$returns , na.rm = T)
  sd_return = sd(pf_rebal$returns , na.rm = T)
  
  plot_historic = plot(cumprod(1 + pf_rebal$returns) ,main = "Historic Performance")
  
  return(list(mean_return = mean_return,sd_return = sd_return,plot_historic = plot_historic , pfRetrns =  merge(returnZoo , pf_rebal$returns)))
}

getHistoricReturns <- function(mfDf){
  allNAV = list()
  for( i in 1:(nrow(mfDf))){
    dataTemp = getHistNAV(mfcode = mfDf$Code[i],scmCode = mfDf$`Scheme Code`[i],startDate = as.Date("1995-01-01"), endDate = Sys.Date())
    allNAV = c(allNAV , list(dataTemp))  
  }
  
  names(allNAV) = mfDf$`Scheme Code`
  allNAVZoo = do.call("merge",lapply( allNAV , function(x) zoo(x = x$nav,order.by = x$date) ))
  returnZoo = Return.calculate(allNAVZoo)
  if(length(allNAV)!=1){
  returnZoo = returnZoo[max(apply(returnZoo ,2, function(x) ( min( which(!is.na(x)) ) ))) : nrow(returnZoo),]
  }
  return(returnZoo)
  
}

getSIPreturns <- function(returnZoo){
  monthStarts = as.Date(seq((as.yearmon(min(attributes(returnZoo)$index)) + 1/12), (as.yearmon(max(attributes(returnZoo)$index))) , 1/12))
  
  zoo( 0 , seq(monthStarts[1], max(attributes(returnZoo)$index),1))
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

