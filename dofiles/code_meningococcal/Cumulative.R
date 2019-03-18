DataCleaner_Cumulative <- function(data, arguments=NULL){
  data <- DataCleaner_Basic(data, arguments=arguments)
  
  dataWeekly <- data[dead=="all" & hospital=="all" & method=="all" & age %in% arguments[["plotAges"]][[arguments[["LANGUAGE"]]]],.(N=.N),by=.(age,Pdato)]
  dataWeekly[,Pdato:=as.Date(Pdato)]
  skeleton <- data.table(expand.grid(
    age=unique(dataWeekly$age),
    Pdato=seq.Date(min(dataWeekly$Pdato),as.Date(TODAYS_DATE),by=1)))
  dataWeekly <- merge(skeleton,dataWeekly,by=c("age","Pdato"),all.x=T)
  dataWeekly[is.na(N),N:=0]
  
  dataWeekly[,month:=as.numeric(format.Date(Pdato,"%m"))]
  dataWeekly[,yr_wk:=as.numeric(format.Date(Pdato,"%G"))]
  dataWeekly[,wk:=as.numeric(format.Date(Pdato,"%V"))]
  dataWeekly <- dataWeekly[,.(N=sum(N),Pdato=min(Pdato)),by=.(age,yr_wk,wk)]
  dataWeekly[,season:=sprintf("%s/%s",yr_wk,yr_wk+1)]
  dataWeekly[wk<=26,season:=sprintf("%s/%s",yr_wk-1,yr_wk)]
  setorder(dataWeekly,age,Pdato)
  dataWeekly[!is.na(wk),wk_in_season:=(wk+26)%%53+1]
  dataWeekly[,cum_N:=cumsum(N),by=.(age,season)]
  
  dataWeekly <- dataWeekly[season <= arguments[["seasonOfInterest"]]]
  
  return(dataWeekly)
}

ResultProducer_Cumulative <- function(data, arguments=NULL){
  data <- data[season>"1995/1996"]
  seasonList <- rev(sort(unique(data$season)))
  
  maxSeason <- arguments[["seasonOfInterest"]]
  seasons <- seasonList[which(seasonList==maxSeason):which(seasonList=="1996/1997")]
  colouredSeasons <- seasons[1:5]
  
  weekList <- na.omit(unique(data[,c("wk_in_season","wk")]))
  setorder(weekList,wk)
  weekList <- weekList[wk %in% c(seq(1,52,13),26)]
  
  data[,age:=factor(age,levels=arguments[["plotAges"]][[arguments[["LANGUAGE"]]]])]
  
  data[,vaccinationProgram:=list(
    "NB"="F\u00F8r 2014/2015",
    "EN"="Before 2014/2015")[[arguments[["LANGUAGE"]]]]]
  data[season>="2014/2015",vaccinationProgram:=list(
    "NB"="2014/2015 og videre",
    "EN"="2014/2015 and onwards")[[arguments[["LANGUAGE"]]]]]
  
  q <- ggplot(data, aes(x=wk_in_season,y=cum_N,group=season,lty=vaccinationProgram))
  q <- q + geom_line(aes(group=season),alpha=0.1,lwd=0.5)
  q <- q + geom_line(data=data[season %in% colouredSeasons], mapping=aes(colour=season), lwd=1)
  q <- q + geom_line(data=data[season==maxSeason], mapping=aes(colour=season), lwd=2.5)
  q <- q + facet_wrap(~age,scales="free")
  q <- q + scale_linetype_discrete(
    list("NB"="Vaksinasjonprogram",
         "EN"="Vaccination program")[[arguments[["LANGUAGE"]]]]
  )
  q <- q + guides(linetype = guide_legend(override.aes = list(size = 1)))
  q <- q + scale_colour_brewer(
    list("NB"="Sesong",
         "EN"="Season")[[arguments[["LANGUAGE"]]]]
    ,palette="Set2")
  q <- q + scale_y_continuous(
    list("NB"="Kumulativt antall meldte tilfeller",
         "EN"="Cumulative number of registered cases")[[arguments[["LANGUAGE"]]]])
  q <- q + scale_x_continuous(list("NB"="Uke",
                                   "EN"="Week")[[LANGUAGE]],
                              breaks=weekList$wk_in_season,
                              labels=weekList$wk,
                              minor_breaks=NULL)
  q <- q + labs(title=list(
    "NB"=sprintf("Kumulativt antall meldte tilfeller fra sesongen %s til sesongen %s\n(farget fra %s til %s)",
                 min(seasons),
                 max(seasons),
                 min(colouredSeasons),
                 max(colouredSeasons)),
    "EN"=sprintf("Cumulative number of registered cases from season %s to season %s\n(colored from %s to %s)",
                 min(seasons),
                 max(seasons),
                 min(colouredSeasons),
                 max(colouredSeasons)))[[arguments[["LANGUAGE"]]]])
  q <- q + labs(caption=DATA_CAPTION[[arguments[["LANGUAGE"]]]])
  q <- q + theme_gray(base_size=16)
  #q <- q + theme(legend.key.size = unit(10, "lines"))
  #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
  saveA4(q,
         filename=file.path(FOLDERS$RESULTS_TODAY,gsub("/","_",arguments[["seasonOfInterest"]]),arguments[["LANGUAGE"]],"Figures",arguments[["filename"]]))
}

