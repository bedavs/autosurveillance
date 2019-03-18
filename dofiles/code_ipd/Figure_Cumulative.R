Figure_Cumulative <- function(rawGroupNumbers,ageListCumulative,seasonOfInterest,seasonList,LANGUAGE,BASE_FOLDER, USE_TITLE=TRUE){
  
  maxSeason <- seasonOfInterest
  seasons <- seasonList[which(seasonList==maxSeason):which(seasonList=="2004/2005")]
  colouredSeasons <- seasons[1:5]
  
  weekList <- na.omit(unique(rawGroupNumbers[,c("week","s_week")]))
  setorder(weekList,week)
  weekList <- weekList[week %in% c(seq(1,52,13),26)]
  
  for(v in unique(rawGroupNumbers$vaccine)){
    Sys.sleep(1)
    plotData <- rawGroupNumbers[time=="week" & 
                                  season %in% seasons &
                                  age %in% ageListCumulative[[LANGUAGE]] &
                                  vaccine==v]
    plotData[,age:=factor(age,levels=ageListCumulative[[LANGUAGE]])]
    
    TITLE <- list(
      "NB"=sprintf("Kumulativt antall tilfeller fra %s til %s\n(farget fra %s til %s)",
                   min(seasons),
                   max(seasons),
                   min(colouredSeasons),
                   max(colouredSeasons)),
      "EN"=sprintf("Cumulative number of cases from %s to %s\n(colored from %s to %s)",
                   min(seasons),
                   max(seasons),
                   min(colouredSeasons),
                   max(colouredSeasons)))[[LANGUAGE]]
    TITLE_Y <- list("NB"="Kumulativt antall tilfeller",
                    "EN"="Cumulative number of cases")[[LANGUAGE]]
    
    q <- ggplot(plotData, aes(x=s_week,y=scum_num,group=season))
    q <- q + geom_line(aes(group=season),alpha=0.1,lwd=0.5)
    q <- q + geom_line(data=plotData[season %in% colouredSeasons], mapping=aes(colour=season), lwd=1)
    q <- q + geom_line(data=plotData[season==maxSeason], mapping=aes(colour=season), lwd=2.5)
    q <- q + facet_wrap(~age,scales="free")
    q <- q + scale_colour_brewer(
      list("NB"="Sesong",
           "EN"="Season")[[LANGUAGE]]
      ,palette="Set2")
    q <- q + scale_x_continuous(list("NB"="Uke",
                                     "EN"="Week")[[LANGUAGE]],
                                breaks=weekList$s_week,
                                labels=weekList$week,
                                minor_breaks=NULL)
    if(USE_TITLE){
      q <- q + labs(title=TITLE)
      q <- q + scale_y_continuous(TITLE_Y)
    } else {
      q <- q + scale_y_continuous(TITLE)
    }
    q <- q + labs(caption=DATA_CAPTION[[LANGUAGE]])
    q <- q + theme_gray(base_size=THEME_BASE_SIZE)
    #q <- q + theme(legend.key.size = unit(10, "lines"))
    #q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
    saveA4(q,
           filename=file.path(BASE_FOLDER,gsub("/","_",seasonOfInterest),LANGUAGE,"Figures_cumulative",sprintf("%s_Cumulative_vax_%s.png",LANGUAGE,v)))
  }
}