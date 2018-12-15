getHistNAV  = function(mfcode,scmCode , startDate , endDate){
  
urlAmfi = paste0("http://portal.amfiindia.com/NavHistoryReport_Rpt_Po.aspx?rpt=1&frmdate=",formatDate(startDate),"&todate=",
formatDate(endDate),"&mf=",mfcode,"&scm=",scmCode)
html = read_html(urlAmfi)
dataScrape = html_nodes(html , "table")

navDf = html_table(dataScrape[[4]]) 
navDf = navDf[-(1:5),c(1,4)]
names(navDf) = c("nav" , "date")
  
return(navDf)
}

formatDate = function(date){
  if(substr(format(date , "%d-%h-%Y") , 0,1)=="0"){
    dateStr = substr(format(date , "%d-%h-%Y") , 2,11)
  } else {
    dateStr = format(date , "%d-%h-%Y") 
  }
  return(dateStr)
}