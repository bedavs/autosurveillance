DataCleaner_TableIncidenceByMonth <- function(data, arguments=NULL){
  d <- DataCleaner_Basic(data, arguments=arguments)
 
  d2 <- copy(d)
  d2[,month:=9999]
  d <- rbind(d,d2)
  d2 <- copy(d)
  d2[,year:=9999]
  d <- rbind(d,d2)
  
  # divide by 2 because we have total + specific serotypes
  # divide by 2 because we have total + specific ages
  d <- d[year <= arguments[["yearOfInterest"]] | year==9999,.(N=.N/4),by=.(year,month)]
  
  skeleton <- data.table(expand.grid(year=c(min(d$year):arguments[["yearOfInterest"]],9999),month=c(1:12,9999)))
  d <- merge(skeleton,d,by=c("year","month"),all.x=T)
  d[is.na(N),N:=0]
  return(d)
}

ResultProducer_TableIncidenceByMonth <- function(data, arguments=NULL){
  data[,year:=formatC(year,digits=0,format="f", decimal.mark=",")]
  data[year=="9999",year:="Total"]
  data[,month:=formatC(month,digits=0,format="f", decimal.mark=",", width=2, flag="0")]
  data[month=="9999",month :="Total"]
  data[,N:=formatC(N,digits=0,format="f", decimal.mark=",")]
  d1 <- dcast.data.table(data,year ~ month, value.var="N",fill="0", drop=FALSE)
  
  d1 <- AddTitleToDF(d1,title="Raw number of cases", copyColumnNames = TRUE)
  
  write.table(d1,
              file=arguments[["filename"]],
              row.names=F,
              col.names=F,
              sep=";",
              dec=",",
              qmethod="double")
}

ResultProducer_FigureIncidenceByMonth1Year <- function(data, arguments=NULL){
  data <- data[year!=9999 & month!=9999]
  data <- data[year==max(year)]
  data[,yearLabel:=as.character(year)]
  data <- data[,.(N=mean(N)),by=.(month,yearLabel)]
  
  q <- ggplot(data,aes(x=month,y=N,fill=yearLabel))
  q <- q + geom_bar(stat="identity",pos="dodge")
  q <- q + scale_fill_brewer(list("NB"="",
                                  "EN"="")[[arguments[["LANGUAGE"]]]],palette="Set2")
  q <- q + scale_x_continuous(
    list("NB"="M\u00E5ned",
         "EN"="Month")[[arguments[["LANGUAGE"]]]],
    breaks=1:12,
    labels=list("NB"=c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Des"),
                "EN"=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))[[arguments[["LANGUAGE"]]]],minor_breaks=NULL)
  q <- q + scale_y_continuous(
    list("NB"="M\u00E5nedlig antall meningokokk tilfeller",
         "EN"="Monthly number of meningococcal cases")[[arguments[["LANGUAGE"]]]]
  )
  q <- q + labs(title=arguments[["title"]])
  q <- q + labs(caption=DATA_CAPTION[[arguments[["LANGUAGE"]]]])
  q <- q + theme_gray(base_size=THEME_BASE_SIZE)
  #q <- q + theme(legend.key.size = unit(10, "lines"))
  #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
  saveA4(q,
         filename=arguments[["filename"]])
}

ResultProducer_FigureIncidenceByMonth1YearVs3 <- function(data, arguments=NULL){
  data <- data[year!=9999 & month!=9999]
  data <- data[year>=max(year)-3]
  data[,yearLabel:=ifelse(year==max(year),year,sprintf("%s-%s",min(year),max(year)-1))]
  data <- data[,.(N=mean(N)),by=.(month,yearLabel)]
  
  q <- ggplot(data,aes(x=month,y=N,fill=yearLabel))
  q <- q + geom_bar(stat="identity",pos="dodge")
  q <- q + scale_fill_brewer(list("NB"="",
                                  "EN"="")[[arguments[["LANGUAGE"]]]],palette="Set2")
  q <- q + scale_x_continuous(
    list("NB"="M\u00E5ned",
         "EN"="Month")[[arguments[["LANGUAGE"]]]],
    breaks=1:12,
    labels=list("NB"=c("Jan","Feb","Mar","Apr","Mai","Jun","Jul","Aug","Sep","Okt","Nov","Des"),
                "EN"=c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))[[arguments[["LANGUAGE"]]]],minor_breaks=NULL)
  q <- q + scale_y_continuous(
    list("NB"="Gjennomsnittlig antall meningokokktilfeller per m\u00E5ned",
         "EN"="Average monthly number of meningococcal cases")[[arguments[["LANGUAGE"]]]]
  )
  q <- q + labs(title=arguments[["title"]])
  q <- q + labs(caption=DATA_CAPTION[[arguments[["LANGUAGE"]]]])
  q <- q + theme_gray(base_size=THEME_BASE_SIZE)
  #q <- q + theme(legend.key.size = unit(10, "lines"))
  #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
  saveA4(q,
         filename=arguments[["filename"]])
}
