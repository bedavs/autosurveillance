DataCleaner_FigureSerotypeByYear <- function(data, arguments=NULL){
  d <- DataCleaner_Basic(data, arguments=arguments)
  d <- d[!serogroup %in% c("Total","Totalt")]
  
  d <- d[age %in% arguments[["plotAgesSerotypeByYear"]][[arguments[["LANGUAGE"]]]] & 
           (year <= arguments[["yearOfInterest"]]),
         .(N=.N),by=.(year,age,serogroup)]
  
  skeleton <- data.table(expand.grid(
    year=c(min(d$year):arguments[["yearOfInterest"]]),
    age=unique(d$age),
    serogroup=unique(d$serogroup)
  ))
  d <- merge(skeleton,d,by=c("year","age","serogroup"),all.x=T)
  d[is.na(N),N:=0]
  nrow(d)
  d[,totalN:=sum(N),by=.(age,year)]
  d[,percentage:=N/totalN*100]
  d[,serogroup:=gsub("Neisseria meningitidis","Nm",serogroup)]
  
  seros <- unique(d$serogroup)
  seros <- seros[seros!="Nm ina"]
  seros <- c(seros,"Nm ina")
  d[,serogroup:=factor(serogroup,levels=seros)]
  
  return(d)
}

ResultProducer_FigureSerotypeByYear <- function(data, arguments=NULL){
  total <- list("NB"="Totalt","EN"="Total")[[arguments[["LANGUAGE"]]]]
  data[,prettyYear:=sprintf("%s\n(n=%s)",year,totalN)]
  
  q <- ggplot(data, aes(x=prettyYear, y=percentage, fill=serogroup,group=serogroup))
  q <- q + geom_bar(lwd=2,stat="identity",alpha=0.8)
  q <- q + scale_fill_brewer(list("NB"="Serogrupper",
                                    "EN"="Serogroups")[[arguments[["LANGUAGE"]]]],palette="Set2")
  q <- q + scale_x_discrete(
    list("NB"="\u00C5r",
         "EN"="Year")[[arguments[["LANGUAGE"]]]])
  q <- q + scale_y_continuous(arguments[["title_y"]])
  q <- q + labs(title=arguments[["title"]])
  q <- q + labs(caption=DATA_CAPTION[[arguments[["LANGUAGE"]]]])
  q <- q + theme_gray(base_size=THEME_BASE_SIZE)
  #q <- q + theme(legend.key.size = unit(10, "lines"))
  #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
  saveA4(q,
         filename=arguments[["filename"]])
}
