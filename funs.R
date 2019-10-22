## Get historic NAV for a given fund
getHistNAV  = function(mfcode,scmCode , startDate , endDate){
  
  urlAmfi = paste0("http://portal.amfiindia.com/NavHistoryReport_Rpt_Po.aspx?rpt=1&frmdate=",formatDate(startDate),"&todate=",
                   formatDate(endDate),"&mf=",mfcode,"&scm=",scmCode)
  html = read_html(urlAmfi)
  dataScrape = html_nodes(html , "table")
  
  navDf = tryCatch({html_table(dataScrape[[4]]) } , error = function(e){ NA })
  if(is.na(navDf))stop(paste("Data not available for fund at",urlAmfi ) )
  navDf = navDf[-(1:5),c(1,4)]
  names(navDf) = c("nav" , "date")
  
  navDf$date = as.Date(navDf$date , format = "%d-%b-%Y")
  navDf$nav = as.numeric(navDf$nav)
  
  navDf = navDf[is.finite(navDf$nav) & (navDf$nav != 0) , ]
  
  return(navDf)
}


productListVec <- function(xList,  yVec){
  
  if(length(xList) != length(yVec)){stop("Unequal lengths")} # throw unequal length error
  
  for(i in 1:length(xList)){
    xList[[i]] = xList[[i]]*yVec[i]
  }
  
  return(xList)
}

## Mapping for rebal frequency inout
getRebalCode <- function(rebalPeriod){
  map = data.frame( strings = c("Yearly", "Monthly" , "Never") , 
                    codes = c("years", "months" , NA), stringsAsFactors = F )
  code = map[map$strings ==rebalPeriod, ]$codes
  
  return(code)
}

### Format date
formatDate = function(date){
  if(substr(format(date , "%d-%h-%Y") , 0,1)=="0"){
    dateStr = substr(format(date , "%d-%h-%Y") , 2,11)
  } else {
    dateStr = format(date , "%d-%h-%Y") 
  }
  return(dateStr)
}

## Scrape all current scheme code from AMFI site
getAllSchemeCodes <- function(){
  url = "https://www.amfiindia.com/spages/NAVOpen.txt"
  allFundsData = read.delim(file = url, header = FALSE, sep = "\t", dec = ".",stringsAsFactors = F)
  allFundsData = allFundsData[grep(";",allFundsData$V1),]
  DF <- data.frame(do.call(rbind, strsplit(allFundsData, ";", fixed=TRUE)))
  DF = DF[c("X1","X4")]
  
  DF$X1 = as.character(DF$X1)
  DF$X4 = as.character(DF$X4)
  
  names(DF) = as.vector(DF[1,])
  DF = DF[-1,]
  return(DF)
}


## Load MF codes saved in csv in the working directory 
getMFcodes <- function(){
  mfCodes = read.csv("fundCodes.csv" , stringsAsFactors = F , header = T)
  mfCodes$Fund.house = gsub("amp;","",mfCodes$Fund.house)
  mfCodes$Fund.house = gsub(" Mutual Fund","",mfCodes$Fund.house)
  mfCodes$Fund.house = gsub("Franklin Templeton","Franklin",mfCodes$Fund.house)
  mfCodes$Fund.house = gsub("JM Financial","JM",mfCodes$Fund.house)
  mfCodes$Fund.house = gsub("Kotak Mahindra","Kotak",mfCodes$Fund.house)
  mfCodes = rbind(mfCodes , c("Templeton", 27))
  return(mfCodes)
}

getAllSchemeMFCodes <- function(){
  schemeCodes = getAllSchemeCodes()
  mfCodes = getMFcodes()
  schemeMFs = sapply(schemeCodes$`Scheme Name` , function(x) ( reverseGrep(x , mfCodes$Fund.house) ))
  schemeCodes$mfName = schemeMFs
  schemeCodesMfCodes = merge(schemeCodes , mfCodes , by.x = "mfName", by.y = "Fund.house",all.x = T)
  return(schemeCodesMfCodes)
}

reverseGrep <- function(mianStr, matchStrs){
  matches = c()
  for(i in 1:length(matchStrs)){
    matchIndice = c(grep(paste0("^",matchStrs[i]," ") , mianStr,ignore.case = T),grep(paste0("^",matchStrs[i],"-") , mianStr,ignore.case = T))
    if(length(matchIndice) !=0){
      matches = c(matches , matchStrs[i])
    }
  } 
  
  if(length(matches) ==0){
    return(NA)
  } else {
    return(paste(matches , collapse = ","))
  }
  
}

## Logging function for app
writeLog <- function(){
  print(environment())
  str(perfData)
  logDataFileName = paste0(format(Sys.time() , format = "%Y%m%d_%H%M%S", tz = "Asia/Kolkata") , "data.csv")
  logNameFileName = gsub("data","mfNames" , logDataFileName)
  write.csv(as.data.frame(perfData$pfRetrns) , file = paste0("/Users/dhruv/Documents/data audit/",logDataFileName))
  write.csv(schemeCodes[schemeCodes$`Scheme Name` %in% mfNames,] , file = paste0("/Users/dhruv/Documents/data audit/",logNameFileName))
}

## Post processing function for performance
postProcessFolioReturns <- function(folioReturns){
  
  meanRet = mean(folioReturns , na.rm = T)
  sdRet = sd(folioReturns, na.rm = T)
  sharpeRatio = meanRet/sdRet
  avgDrawdown = NA
  
  metricTable = data.frame( c("Average Return","Average Volatility","Sharpe Ratio","Average Drawdown Time") ,
                            c(meanRet ,sdRet ,  sharpeRatio , avgDrawdown) )
  
  plot_historic = plot(folioReturns ,main = "Historic Performance")
  return(list(metricTable = metricTable , plot_historic = plot_historic))
}
