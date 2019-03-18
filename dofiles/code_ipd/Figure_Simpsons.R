Figure_Simpsons <- function(rawSpecificNumbers,ageListRestrictedAll,ageListSimpsons,DATA_CAPTION,yearOfInterest,BASE_FOLDER,LANGUAGE, USE_TITLE=TRUE){
  
  simpsons <- rawSpecificNumbers[year >= 2004 & year<=yearOfInterest & 
                                   time=="year" & 
                                   !vaccine %in% c("All IPD","missing") &
                                   age %in% c(ageListRestrictedAll[[LANGUAGE]],ageListOld[[LANGUAGE]])]
  simpsons[,p:=num/num_all]
  simpsons <- simpsons[,.(simpsons=sum(p^2)),by=.(age,year)]
  
  toPlot <- simpsons[age %in% ageListSimpsons[[LANGUAGE]]]
  toPlot[,age:=factor(age,levels=ageListSimpsons[[LANGUAGE]])]
  
  q <- ggplot(toPlot, aes(x=year,y=1-simpsons,colour=age,group=age))
  q <- q + geom_line(lwd=2)
  q <- q + scale_y_continuous(
    list("NB"="Simpson's Index (1-D)","EN"="Simpson's Index (1-D)")[[LANGUAGE]],lim=c(0,1))
  q <- q + scale_x_continuous(
    list("NB"="\u00C5r","EN"="Year")[[LANGUAGE]],
    breaks=seq(min(simpsons$year),max(simpsons$year),1),
    minor_breaks=NULL)
  q <- q + scale_color_brewer(
    list("NB"="Alder","EN"="Age")[[LANGUAGE]],
    palette="Set2")
  if(USE_TITLE) q <- q + labs(title=
                  list("NB"="Simpson's Index (1-D)","EN"="Simpson's Index (1-D)")[[LANGUAGE]]
  )
  q <- q + labs(caption=DATA_CAPTION[[LANGUAGE]])
  q <- q + theme_gray(base_size=THEME_BASE_SIZE)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust=0.5))
  saveA4(q,
         filename=file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Figures_serotype_diversity",sprintf("%s_Diversity.png",LANGUAGE)))
  
}