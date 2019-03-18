DataCleaner_TableIncidenceByAge <- function(data, arguments=NULL){
  d <- DataCleaner_Basic(data, arguments=arguments)
  
  d2 <- copy(d)
  d2[,year:=9999]
  d <- rbind(d,d2)
  
  # divide by 2 because we have total + specific serotypes
  d <- d[year <= arguments[["yearOfInterest"]] | year==9999,.(N=.N/2),by=.(year,age)]
  
  skeleton <- data.table(expand.grid(year=c(min(d$year):arguments[["yearOfInterest"]],9999),age=unique(d$age)))
  d <- merge(skeleton,d,by=c("year","age"),all.x=T)
  d[is.na(N),N:=0]
  return(d)
}

ResultProducer_TableIncidenceByAge <- function(data, arguments=NULL){
  data[,year:=formatC(year,digits=0,format="f", decimal.mark=",")]
  data[year=="9999",year:="Total"]
  data[,N:=formatC(N,digits=0,format="f", decimal.mark=",")]
  data[,age:=factor(age,levels=names(arguments[["ageDef"]][[arguments[["LANGUAGE"]]]]))]
  d1 <- dcast.data.table(data,age ~ year, value.var="N",fill="0", drop=FALSE)
  
  d1 <- AddTitleToDF(d1,title="Raw number of cases", copyColumnNames = TRUE)
  
  write.table(d1,
              file=arguments[["filename"]],
              row.names=F,
              col.names=F,
              sep=";",
              dec=",",
              qmethod="double")
}
