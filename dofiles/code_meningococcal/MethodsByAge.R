DataCleaner_MethodsByAge <- function(data, arguments=NULL){
  data <- DataCleaner_Basic(data, arguments=arguments)
  d <- data[dead=="all" & hospital=="all" & year <= arguments[["yearOfInterest"]],
       .(N=.N),by=.(year,age,method)]
  
  skeleton <- data.table(expand.grid(
    year=c(min(d$year):arguments[["yearOfInterest"]]),
    age=unique(d$age),
    method=unique(d$method)))
  d <- merge(skeleton,d,by=c("year","age","method"),all.x=T)
  d[is.na(N),N:=0]
  nrow(d)
  
  return(d)
}

ResultProducer_MethodsByAge <- function(data, arguments=NULL){
  d <- copy(data)
  d[method=="all",denom:=as.numeric(N)]
  d[,denom:=mean(denom,na.rm=T),by=.(year,age)]
  d[,perc:=formatC(N/denom*100,format="f", decimal.mark=",",digits=1)]
  d[,denom:=NULL]
  d <- dcast.data.table(d,year + age ~ method, value.var=c("N","perc"),fill=0, drop=FALSE)
  for(i in names(d)[grep("^perc",names(d))]){
    d[get(i)=="0",(i):="0,0"]
  }
  
  retval <- vector("list",length=length(arguments[["ageDef"]][[arguments[["LANGUAGE"]]]]))
  for(i in 1:length(retval)){
    retval[[i]] <- d[age==names(arguments[["ageDef"]][[arguments[["LANGUAGE"]]]])[i]]
    retval[[i]] <- AddTitleToDF(retval[[i]],title=names(arguments[["ageDef"]][[arguments[["LANGUAGE"]]]])[i], copyColumnNames = TRUE)
    retval[[i]] <- AddTitleToDF(retval[[i]],title="")
    retval[[i]] <- AddTitleToDF(retval[[i]],title="")
  }
  
  retval <- rbindlist(retval)
  
  write.table(retval,
              file=file.path(FOLDERS$RESULTS_TODAY,arguments[["yearOfInterest"]],arguments[["LANGUAGE"]], "Tables",
                             "methods_by_age.csv"),
              row.names=F,
              col.names=F,
              sep=";",
              dec=",",
              qmethod="double")
}
