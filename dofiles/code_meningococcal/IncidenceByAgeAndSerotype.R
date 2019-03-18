DataCleaner_TableIncidenceByAgeAndSerotype <- function(data, arguments=NULL){
  d <- DataCleaner_Basic(data, arguments=arguments)
  
  d2 <- copy(d)
  d2[,year:=9999]
  d <- rbind(d,d2)
  
  d <- d[age %in% arguments[["tableAgesTableAgeAndSerotype"]][[arguments[["LANGUAGE"]]]] & (year <= arguments[["yearOfInterest"]] | year==9999),.(N=.N),by=.(year,age,serogroup)]
  
  skeleton <- data.table(expand.grid(
    year=c(min(d$year):arguments[["yearOfInterest"]],9999),
    age=unique(d$age),
    serogroup=unique(d$serogroup)
    ))
  d <- merge(skeleton,d,by=c("year","age","serogroup"),all.x=T)
  d[is.na(N),N:=0]
  nrow(d)
  d <- merge(d,pop,by=c("age","year"),all.x=T)
  nrow(d)
  d[,totalN:=sum(N)/2,by=.(age,year)]
  d[,percentage:=N/totalN*100]
  d[,totalN:=NULL]
  return(d)
}

ResultProducer_TableIncidenceByAgeAndSerotype <- function(data, arguments=NULL){
  data[,incidence:=formatC(N/pop*100000,format="f", decimal.mark=",",digits=1)]
  data[,percentage:=formatC(percentage,format="f", decimal.mark=",",digits=1)]
  data[,year:=formatC(year,digits=0,format="f", decimal.mark=",")]
  data[year=="9999",year:="Total"]
  data[,N:=formatC(N,digits=0,format="f", decimal.mark=",")]
  data[,age:=factor(age,levels=arguments[["tableAgesTableAgeAndSerotype"]][[arguments[["LANGUAGE"]]]])]
  d1 <- dcast.data.table(data,age + serogroup ~ year, value.var="N",fill="0", drop=FALSE)
  d2 <- dcast.data.table(data,age + serogroup ~ year, value.var="percentage",fill="0", drop=FALSE)
  d3 <- dcast.data.table(data,age + serogroup ~ year, value.var="incidence",fill="0", drop=FALSE)
  
  d1 <- cbind(
    AddTitleToDF(d1[age==arguments[["tableAgesTableAgeAndSerotype"]][[arguments[["LANGUAGE"]]]][1]],
                 title="Raw number of cases", copyColumnNames = TRUE),
    "","",
    AddTitleToDF(d1[age==arguments[["tableAgesTableAgeAndSerotype"]][[arguments[["LANGUAGE"]]]][2]],
                 title="Raw number of cases", copyColumnNames = TRUE)
  )
  
  d2 <- cbind(
    AddTitleToDF(d2[age==arguments[["tableAgesTableAgeAndSerotype"]][[arguments[["LANGUAGE"]]]][1]],
                 title="Percentage", copyColumnNames = TRUE),
    "","",
    AddTitleToDF(d2[age==arguments[["tableAgesTableAgeAndSerotype"]][[arguments[["LANGUAGE"]]]][2]],
                 title="Percentage", copyColumnNames = TRUE)
  )
  
  d3 <- cbind(
    AddTitleToDF(d3[age==arguments[["tableAgesTableAgeAndSerotype"]][[arguments[["LANGUAGE"]]]][1]],
                 title="Incidence (per 100.000)", copyColumnNames = TRUE),
    "","",
    AddTitleToDF(d3[age==arguments[["tableAgesTableAgeAndSerotype"]][[arguments[["LANGUAGE"]]]][2]],
                 title="Incidence (per 100.000)", copyColumnNames = TRUE)
  )
  
  retval <- rbind(d1,"","",d2,"","",d3,fill=T)
  for(i in which(names(retval)=="age")) retval[[i]] <- as.character(retval[[i]])
  retval[is.na(retval)] <- ""
  
  write.table(retval,
              file=arguments[["filename"]],
              row.names=F,
              col.names=F,
              sep=";",
              dec=",",
              qmethod="double")
}


DataCleaner_FigureIncidenceByAgeAndSerotype <- function(data, arguments=NULL){
  d <- DataCleaner_TableIncidenceByAgeAndSerotype(data, arguments=arguments)
  d <- d[year!=9999]
  
  return(d)
}

ResultProducer_FigureIncidenceByAgeAndSerotype <- function(data, arguments=NULL){
  seros <- unique(data$serogroup)
  seros <- seros[!seros %in% c("Nm ina","Total","Totalt")]
  seros <- c(seros,"Nm ina","Total","Totalt")
  data[,serogroup:=factor(serogroup,levels=seros)]
  
  onlyShowTotal <- FALSE
  if(!is.null(arguments[["onlyShowTotal"]])) if(arguments[["onlyShowTotal"]]){
    onlyShowTotal <- TRUE
  }

  if(onlyShowTotal){
    q <- ggplot(data[serogroup %in% c("Total","Totalt")], aes(x=year, y=N/pop*100000))
  } else {
    q <- ggplot(data, aes(x=year, y=N/pop*100000, colour=serogroup,group=serogroup))
  }
  q <- q + geom_line(lwd=2)
  q <- q + scale_colour_brewer(list("NB"="Serogrupper",
                                    "EN"="Serogroups")[[arguments[["LANGUAGE"]]]],palette="Set2")
  q <- q + scale_x_continuous(
    list("NB"="\u00C5r",
         "EN"="Year")[[arguments[["LANGUAGE"]]]],
    breaks=seq(1996,arguments[["yearOfInterest"]],1),minor_breaks=NULL)
  q <- q + scale_y_continuous(arguments[["title_y"]])
  q <- q + expand_limits(y=0)
  q <- q + labs(title=arguments[["title"]])
  q <- q + labs(caption=DATA_CAPTION[[arguments[["LANGUAGE"]]]])
  q <- q + theme_gray(base_size=THEME_BASE_SIZE)
  #q <- q + theme(legend.key.size = unit(10, "lines"))
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
  saveA4(q,
         filename=arguments[["filename"]])
}
