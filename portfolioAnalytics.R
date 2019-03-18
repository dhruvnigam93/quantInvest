#### Historic Portfolio Performance Lumpsum ####
getHistoricPerfLump <- function(mfNames , capital, schemeCodes , rebalPeriod){
  
  portDF = merge(data.frame(mfNames) , schemeCodes , by.x = "mfNames" , by.y = "Scheme Name")

    print(paste("Getting data from scheme Codes" , paste(portDF$`Scheme Code`, collapse = ",")))
  
  returnZoo <- getSimpleHistoricReturns(portDF)
  pf_rebal <- Return.portfolio(returnZoo, weights = capital/sum(capital), rebalance_on = getRebalCode(rebalPeriod), verbose = TRUE )
  
  processedResults = postProcessFolioReturns(cumprod(1 + pf_rebal$returns)*(sum(capital)))
  return(list(numericMetrics =processedResults$metricTable ,plot_historic = processedResults$plot_historic , pfRetrns =  merge(returnZoo , pf_rebal$returns)))
}

#### Historic Portfolio Performance SIP####
getHistoricPerfSIP <- function(mfNames , capital, schemeCodes , rebalPeriod){
  
  portDF = merge(data.frame(mfNames) , schemeCodes , by.x = "mfNames" , by.y = "Scheme Name")
  print(paste("Getting data from scheme Codes" , paste(portDF$`Scheme Code`, collapse = ",")))
  returnZoo <- getSimpleHistoricReturns(portDF)  ## Simple historic returns for individual funds
  
  sipPortfolioReturnsList <- lapply(as.list(returnZoo) , calculateSIPreturns)  ## get dollar return for SIp stratefy for each fund
  sipPortfolioReturnsList <- productListVec(sipPortfolioReturnsList , capital)  ## scale by capital
  sipPortfolioReturnZoo = do.call("merge",sipPortfolioReturnsList) ## SIP returns for individual funds
  
  sipTotalReturn = zoo(rowSums(sipPortfolioReturnZoo , na.rm = T) , time(sipPortfolioReturnZoo))
  processedResults = postProcessFolioReturns(sipTotalReturn)
  return(list(nummercMetrics =processedResults$metricTable ,plot_historic = processedResults$plot_historic , pfRetrns =  merge(sipPortfolioReturnZoo , sipTotalReturn)))
}

getSimpleHistoricReturns <- function(mfDf){
  allNAV = list()
  for( i in 1:(nrow(mfDf))){
    dataTemp = getHistNAV(mfcode = mfDf$Code[i],scmCode = mfDf$`Scheme Code`[i],startDate = as.Date("1995-01-01"), endDate = Sys.Date())
    allNAV = c(allNAV , list(dataTemp))  
  }
  names(allNAV) = mfDf$`Scheme Code`
  
  #### Create combined zoo object ####
  allNAVZoo = do.call("merge",lapply( allNAV , function(x){ zoo(x = x[!duplicated(x),]$nav,order.by = x[!duplicated(x),]$date)  } ))
  returnZoo = Return.calculate(allNAVZoo)
  if(length(allNAV)!=1){
    returnZoo = returnZoo[max(apply(returnZoo ,2, function(x) ( min( which(!is.na(x)) ) ))) : nrow(returnZoo),]
  }
  returnZoo = na.fill(returnZoo,0)
  return(returnZoo)
  
}

calculateSIPreturns <- function(simpleReturnZoo){
  monthStarts = as.Date(seq((as.yearmon(min(attributes(simpleReturnZoo)$index)) + 1/12), (as.yearmon(max(attributes(simpleReturnZoo)$index))) , 1/12))
  sipReturnList = lapply(as.list(monthStarts) , function(x) { cumprod(1 + simpleReturnZoo[index(simpleReturnZoo) >= x[1]]) } )
  sipReturnCum = do.call("merge",sipReturnList)
  sipReturn = zoo(rowSums(sipReturnCum , na.rm = T) , time(sipReturnCum))
  return(sipReturn)
}


#### Portfolio Projections ####

getExpectedPerformance <- function(capital ,mean_return , sd_return , n = 5){

nDays = (1:(n*252))
nYears = nDays/252
projectedAvgRet = ((1 +mean_return)^nDays)*capital
projecctedSd = (sqrt((1 + (sd_return)^2)^nDays - 1))*capital

predFrame = data.frame(years = nYears , return = projectedAvgRet , yhigh = projectedAvgRet + projecctedSd, ylow = projectedAvgRet - projecctedSd)

pl = ggplot(predFrame , aes(x = years , y = return)) + geom_line() +
  geom_ribbon(data = predFrame , aes(ymin = ylow , ymax = yhigh) , alpha = 0.3)

return(pl)

}



