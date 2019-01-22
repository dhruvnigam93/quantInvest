source('~/MutualFundBenchmarking/funs.R')
library(plyr)

allSchemeData = getAllSchemeMFCodes()
allSchemeData1 = allSchemeData[grep("Regular Plan",allSchemeData$`Scheme Name`,ignore.case = T),]
allSchemeData2 = allSchemeData1[grep("Growth Option|Growth",allSchemeData$`Scheme Name`,ignore.case = T),]

names(allSchemeData2)[2:3] = c("scmCode" , "scmName")
largeCap = allSchemeData2[grep("Bluechip|Large Cap|Blue Chip|LargeCap" , allSchemeData2$scmName,ignore.case = T),]

dataNAV = ddply(largeCap , .(scmCode) , function(x)getHistNAV(mfcode = x$Code , scmCode = x$scmCode,startDate = as.Date("2000-01-01"), endDate = Sys.Date()))