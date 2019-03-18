Figure_Incidence_Vax <- function(correctedGroupNumbers, ageListRestricted,yearOfInterest,LANGUAGE,BASE_FOLDER, USE_TITLE=TRUE){
  for(v in unique(correctedGroupNumbers$vaccine)){
    Sys.sleep(1)
    if(v %in% c("All IPD")){
      yearMin <- min(correctedGroupNumbers$year)
      frequencyOfYearPlotting <- 2
    } else if(v=="missing"){
      next
    } else {
      yearMin <- 2004
      frequencyOfYearPlotting <- 1
    }
    
    TITLE <- list("NB"=sprintf("Insidens av invasiv pneumokokksykdom for\n%s serotyper etter aldersgruppe",Decapitalize(v)),
                 "EN"=sprintf("Incidence of invasive pneumococcal disease for\n%s serotypes by age group",Decapitalize(v)))[[LANGUAGE]]
    TITLE_Y <- list("NB"="Antall meldte tilfeller IPD per 100 000",
                    "EN"="Number recorded IPD cases per 100 000")[[LANGUAGE]]
    if(!USE_TITLE){
      TITLE <- gsub(" etter aldersgruppe","",TITLE)
      TITLE <- gsub(" by age group","",TITLE)
    }
    
    q <- ggplot(correctedGroupNumbers[time=="year" & vaccine==v & year>=yearMin & year<=yearOfInterest & age %in% ageListRestricted[[LANGUAGE]]],
                aes(x=year, y=incidence, color=age, group=age))
    #q <- q + geom_line(colour="black",lwd=2.5)
    q <- q + geom_line(lwd=2)
    q <- q + scale_color_brewer(list("NB"="Alder",
                                     "EN"="Age")[[LANGUAGE]],palette="Set2")
    q <- q + scale_x_continuous(
      list("NB"="\u00C5r",
           "EN"="Year")[[LANGUAGE]],
      breaks=seq(yearMin,max(correctedGroupNumbers$year),frequencyOfYearPlotting),minor_breaks=NULL)
    if(USE_TITLE){
      q <- q + labs(title=TITLE)
      q <- q + scale_y_continuous(TITLE_Y)
    } else {
      q <- q + scale_y_continuous(TITLE)
    }
    q <- q + labs(caption=DATA_CAPTION[[LANGUAGE]])
    q <- q + theme_gray(base_size=THEME_BASE_SIZE)
    #q <- q + theme(legend.key.size = unit(10, "lines"))
    q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
    saveA4(q,
           filename=file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Figures_incidence_vax",sprintf("%s_Incidence_vax_%s.png",LANGUAGE,v)))
  }
}

