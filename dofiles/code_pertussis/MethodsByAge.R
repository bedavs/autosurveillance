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
              file=arguments[["filename"]],
              row.names=F,
              col.names=F,
              sep=";",
              dec=",",
              qmethod="double")
}

ResultProducer_FigureMethods <- function(data, arguments=NULL){
  d <- copy(data)
  d <- d[age %in% arguments[["plotAgesMethods"]][[arguments[["LANGUAGE"]]]]]
  d[method=="all",denom:=as.numeric(N)]
  d[,denom:=mean(denom,na.rm=T),by=.(year,age)]
  d[,percentage:=N/denom*100]
  d <- d[method!="all"]
  
  ordering <- names(arguments[["methodDef"]][[arguments[["LANGUAGE"]]]])
  ordering <- ordering[ordering!="all"]
  d[,method:=factor(method,levels=ordering)]
  
  q <- ggplot(d, aes(x=year, y=percentage, fill=method,group=method))
  q <- q + geom_bar(lwd=2,stat="identity",alpha=0.8)
  q <- q + scale_fill_brewer(list("NB"="Metode",
                                  "EN"="Method")[[arguments[["LANGUAGE"]]]],palette="Set2")
  q <- q + scale_x_continuous(
    list("NB"="\u00C5r",
         "EN"="Year")[[arguments[["LANGUAGE"]]]],
    breaks=seq(1996,arguments[["yearOfInterest"]],1),minor_breaks=NULL)
  q <- q + scale_y_continuous(arguments[["title_y"]])
  q <- q + labs(title=arguments[["title"]])
  q <- q + labs(caption=DATA_CAPTION[[arguments[["LANGUAGE"]]]])
  q <- q + theme_gray(base_size=THEME_BASE_SIZE)
  #q <- q + theme(legend.key.size = unit(10, "lines"))
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
  saveA4(q,
         filename=arguments[["filename"]])
  
}

