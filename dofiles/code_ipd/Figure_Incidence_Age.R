Figure_Incidence_Age <- function(correctedGroupNumbers, yearOfInterest, LANGUAGE, BASE_FOLDER, vaxDefIncidenceAge, USE_TITLE=TRUE){
   
   for(a in unique(correctedGroupNumbers$age)){
     Sys.sleep(1)
     plotData <- correctedGroupNumbers[
       time=="year" & 
         vaccine %in% vaxDefIncidenceAge &
         year>=2004 & 
         year<=yearOfInterest &
         age==a]
     plotData[,vaccine:=factor(vaccine,levels=vaxDefIncidenceAge)]
     
     if(LANGUAGE=="NB"){
       if(length(grep("alle",tolower(a))) | length(grep("yngre",tolower(a)))){
         fancyAge <- tolower(a)
       } else {
         fancyAge <- sprintf("%singer",tolower(a))
       }
     } else {
       fancyAge <- tolower(a)
     }
     
     TITLE <- list("NB"=sprintf("Insidens av invasiv pneumokokksykdom etter vaksineserotype%s%s",ifelse(USE_TITLE," - ",",\n"),fancyAge),
                   "EN"=sprintf("Incidence of invasive pneumococcal disease by vaccine serotype%s%s",ifelse(USE_TITLE," - ",",\n"),fancyAge))[[LANGUAGE]]
     TITLE_Y <- list("NB"="Korrigert antall IPD tilfeller per 100.000",
                     "EN"="Corrected number of IPD cases per 100,000")[[LANGUAGE]]
     
     q <- ggplot(plotData, aes(x=year, y=cnum, fill=vaccine, colour=vaccine,group=vaccine))
     #q <- q + geom_line(colour="black",lwd=2.5)
     q <- q + geom_line(lwd=2)
     q <- q + scale_colour_brewer(list("NB"="Serotype",
                                       "EN"="Serotype")[[LANGUAGE]],palette="Set2")
     q <- q + scale_fill_brewer(list("NB"="Serotype",
                                     "EN"="Serotype")[[LANGUAGE]],palette="Set2")
     q <- q + scale_x_continuous(
       list("NB"="\u00C5r",
            "EN"="Year")[[LANGUAGE]],
       breaks=seq(2004,max(correctedGroupNumbers$year),1),minor_breaks=NULL)
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
            filename=file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Figures_incidence_age",sprintf("%s_Incidence_age_%s.png",LANGUAGE,gsub("<","LT",a))))
   }
 }