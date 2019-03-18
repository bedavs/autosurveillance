DataCleaner_TableIncidenceByAge <- function(data, arguments=NULL){
  data <- DataCleaner_Basic(data, arguments=arguments)
  #return(3)
  d <- data[dead=="all" & hospital=="all" & method=="all" & year <= arguments[["yearOfInterest"]],.(N=.N),by=.(year,age)]
  skeleton <- data.table(expand.grid(year=c(min(d$year):arguments[["yearOfInterest"]]),age=unique(d$age)))
  d <- merge(skeleton,d,by=c("year","age"),all.x=T)
  d[is.na(N),N:=0]
  nrow(d)
  d <- merge(d,pop,by=c("age","year"))
  nrow(d)
  return(d)
}

ResultProducer_TableIncidenceByAge <- function(data, arguments=NULL){
  data[,incidence:=formatC(N/pop*100000,format="f", decimal.mark=",",digits=1)]
  data[,year:=formatC(year,digits=0,format="f", decimal.mark=",")]
  data[,N:=formatC(N,digits=0,format="f", decimal.mark=",")]
  data[,age:=factor(age,levels=names(arguments[["ageDef"]][[arguments[["LANGUAGE"]]]]))]
  d1 <- dcast.data.table(data,year ~ age, value.var="N",fill="0", drop=FALSE)
  d2 <- dcast.data.table(data,year ~ age, value.var="incidence",fill="0", drop=FALSE)
  
  d1 <- AddTitleToDF(d1, title="Raw number of cases", copyColumnNames = TRUE)
  d2 <- AddTitleToDF(d2, title="Incidence (per 100.000)", copyColumnNames = TRUE)
  
  write.table(cbind(d1,"","",d2),
              file=arguments[["filename"]],
              row.names=F,
              col.names=F,
              sep=";",
              dec=",",
              qmethod="double")
}

DataCleaner_FigureIncidenceByAge <- function(data, arguments=NULL){
  data <- DataCleaner_Basic(data, arguments=arguments)
  plotData <- data[dead=="all" & hospital=="all" & method=="all" & year <= arguments[["yearOfInterest"]],.(N=.N),by=.(year,age)]
  
  skeleton <- data.table(expand.grid(
    year=c(min(plotData$year):arguments[["yearOfInterest"]]),
    age=unique(plotData$age)))
  plotData <- merge(skeleton,plotData,by=c("year","age"),all.x=T)
  plotData[is.na(N),N:=0]
  
  plotData <- plotData[age %in% arguments[["plotAges"]][[arguments[["LANGUAGE"]]]]]
  plotData[,age:=factor(age,levels=arguments[["plotAges"]][[arguments[["LANGUAGE"]]]])]
  
  plotData <- merge(plotData,pop,by=c("age","year"))
  plotData[,age:=factor(age,levels=arguments[["plotAges"]][[arguments[["LANGUAGE"]]]])]
  
  return(plotData)
}

ResultProducer_FigureIncidenceByAge <- function(data, arguments=NULL){
  topValue <- max(data$N)
  arrowHeight <- topValue*0.05
  
  q <- ggplot(data, aes(x=year, y=N, fill=age, colour=age,group=age))
  q <- q + geom_line(lwd=2)
  for(i in c(2006,2013)) q <- q + annotate("segment", x=i, xend=i, y=topValue+arrowHeight,yend=topValue,color="red",arrow=arrow(length=unit(0.1,"inches")))
  q <- q + scale_colour_brewer(list("NB"="Alder",
                                    "EN"="Age")[[arguments[["LANGUAGE"]]]],palette="Set2")
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

ResultProducer_FigureIncidenceRateByAge <- function(data, arguments=NULL){
  topValue <- max(data$N/data$pop*100000)
  arrowHeight <- topValue*0.05
  
  
  q <- ggplot(data, aes(x=year, y=N/pop*100000, fill=age, colour=age,group=age))
  q <- q + geom_line(lwd=2)
  for(i in c(2006,2013)) q <- q + annotate("segment", x=i, xend=i, y=topValue+arrowHeight,yend=topValue,color="red",arrow=arrow(length=unit(0.1,"inches")))
  q <- q + scale_colour_brewer(list("NB"="Alder",
                                    "EN"="Age")[[arguments[["LANGUAGE"]]]],palette="Set2")
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

