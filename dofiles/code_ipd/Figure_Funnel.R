Figure_Funnel_Season <- function(
  rawSpecificNumbers,
  ageListFunnel,
  LANGUAGE,
  folder,
  seasonOfInterest,
  seasonList,
  USE_TITLE=TRUE
  ){
  
  for(a in ageListFunnel[[LANGUAGE]]){
    for(comparison in c("singleYearLast","4yearbaseline")){
      Sys.sleep(1)
      
      seasonOfInterestIndex <- which(seasonOfInterest==seasonList)  
      if(comparison=="singleYearLast"){
        baseline <- seasonList[seasonOfInterestIndex+1]
      } else if(comparison=="4yearbaseline"){
        baseline <- seasonList[(seasonOfInterestIndex+1):(seasonOfInterestIndex+4)]
      }
      #if(min(baseline)<"2013/2014") baseline <- baseline[1:which(baseline=="2013/2014")]
      
      baselineOldest <- baseline[length(baseline)]
      baselineSoonest <- baseline[1]
      
      if(seasonOfInterest==TODAYS_SEASON){
        if(comparison=="singleYearLast"){
          title <- list(
            "NB"=sprintf("%s, sammenlikning av serotyper%si %s med %s\n(kun data f\u00F8r uke %s)",
                         a,
                         ifelse(USE_TITLE," ","\n"),
                         seasonOfInterest,
                         baselineSoonest,
                         TODAYS_WEEK
            ),
            "EN"=sprintf("%s, comparison of serotypes%sin %s with %s\n(only data before week %s)",
                         a,
                         ifelse(USE_TITLE," ","\n"),
                         seasonOfInterest,
                         baselineSoonest,
                         TODAYS_WEEK
            )
          )
          
          plotData <- rawSpecificNumbers[s_week < TODAYS_WEEK_WITHIN_SEASON & 
                                           time=="week" &
                                           age==a & 
                                           season %in% c(seasonOfInterest,baseline) &
                                           !vaccine %in% c("missing","All IPD")]
          
        } else if(comparison=="4yearbaseline"){
          title <- list(
            "NB"=sprintf("%s, sammenlikning av serotyper i %s\nmed gjennomsnittet fra %s-%s (kun data f\u00F8r uke %s)",
                         a,
                         seasonOfInterest,
                         baselineOldest,
                         baselineSoonest,
                         TODAYS_WEEK
            ),
            "EN"=sprintf("%s, comparison of serotypes in %s\nwith average from %s-%s (only data before week %s)",
                         a,
                         seasonOfInterest,
                         baselineOldest,
                         baselineSoonest,
                         TODAYS_WEEK
            )
          )
          
          plotData <- rawSpecificNumbers[s_week < TODAYS_WEEK_WITHIN_SEASON & 
                                           time=="week" &
                                           age==a & 
                                           season %in% c(seasonOfInterest,baseline) &
                                           !vaccine %in% c("missing","All IPD")]
          plotData[season!=seasonOfInterest,season:=baselineSoonest]
          plotData <- plotData[,.(num=mean(num)),by=.(vaccine,season,week)]
        }
      } else {
        if(comparison=="singleYearLast"){
          title <- list(
            "NB"=sprintf("%s, sammenlikning av serotyper%si %s med %s",
                         a,
                         ifelse(USE_TITLE," ","\n"),
                         seasonOfInterest,
                         baselineSoonest
            ),
            "EN"=sprintf("%s, comparison of serotypes%sin %s with %s",
                         a,
                         ifelse(USE_TITLE," ","\n"),
                         seasonOfInterest,
                         baselineSoonest
            )
          )
          
          plotData <- rawSpecificNumbers[time=="week" & 
                                           age==a & 
                                           season %in% c(seasonOfInterest,baseline) &
                                           !vaccine %in% c("missing","All IPD")]
        } else if(comparison=="4yearbaseline"){
          title <- list(
            "NB"=sprintf("%s, sammenlikning av serotyper i %s\nmed gjennomsnittet fra %s-%s",
                         a,
                         seasonOfInterest,
                         baselineOldest,
                         baselineSoonest
            ),
            "EN"=sprintf("%s, comparison of serotypes in %s\nwith average from %s-%s",
                         a,
                         seasonOfInterest,
                         baselineOldest,
                         baselineSoonest
            )
          )
          
          plotData <- rawSpecificNumbers[time=="week" & 
                                           age==a & 
                                           season %in% c(seasonOfInterest,baseline) &
                                           !vaccine %in% c("missing","All IPD")]
          plotData[season!=seasonOfInterest,season:=baselineSoonest]
          plotData <- plotData[,.(num=mean(num)),by=.(vaccine,season,week)]
        }
      }
      plotData <- plotData[,.(num=round(sum(num))),by=.(vaccine,season)]
      
      if(comparison=="singleYearLast"){
        xTitle <- list("NB"=sprintf("Antall tilfeller i %s",baselineSoonest),
                       "EN"=sprintf("Number of cases in %s",baselineSoonest))[[LANGUAGE]]
        
        yTitle <- list("NB"=sprintf("Forhold av (antall tilfeller i %s/antall tilfeller i %s)",seasonOfInterest,baselineOldest),
                       "EN"=sprintf("Ratio of (number of cases in %s/number of cases in %s)",seasonOfInterest,baselineOldest))[[LANGUAGE]]
      } else if(comparison=="4yearbaseline"){
        xTitle <- list("NB"=sprintf("Gjennomsnittlig antall tilfeller i %s-%s",baselineOldest,baselineSoonest),
                       "EN"=sprintf("Average number of cases in %s-%s",baselineOldest,baselineSoonest))[[LANGUAGE]]
        
        yTitle <- list("NB"=sprintf("Forhold av (antall tilfeller i %s/gjennomsnittet i %s-%s)",ShortenSeason(seasonOfInterest),ShortenSeason(baselineOldest),ShortenSeason(baselineSoonest)),
                       "EN"=sprintf("Ratio of (number of cases in %s/average in %s-%s)",ShortenSeason(seasonOfInterest),ShortenSeason(baselineOldest),ShortenSeason(baselineSoonest)))[[LANGUAGE]]
      }
      title <- title[[LANGUAGE]]
      
      setorder(plotData,vaccine,season)
      plotData[,num_previous:=num]
      plotData[season!=seasonOfInterest,num:=NA]
      plotData[,num:=as.numeric(num)]
      plotData[,num:=max(num,na.rm=T),by=.(vaccine)]
      plotData <- plotData[season!=seasonOfInterest]
      
      if(LANGUAGE=="NB"){
        plotData[,season_baseline:=sprintf("Referanse sesong: %s",season)]
      } else {
        plotData[,season_baseline:=sprintf("Reference season: %s",season)]
      }
      plotData[,season_baseline:=factor(season_baseline,levels=rev(sort(unique(plotData$season_baseline))))]
      
      plotData[num_previous==0, num_previous:=1]
      plotData[num==0, num:=1]
      plotData[,percentageChange := num/num_previous]
      
      upper <- qpois(0.975,1:max(plotData$num_previous))
      lower <- qpois(0.025,1:max(plotData$num_previous))
      confidenceIntervals <- data.table(num_previous=1:max(plotData$num_previous),upper,lower)
      confidenceIntervals[,upperScaled:=upper/num_previous]
      confidenceIntervals[,lowerScaled:=lower/num_previous]
      
      plotData <- merge(plotData,confidenceIntervals,by="num_previous")
      changeLevels <- list(
        "NB"=c("Signifikant \u00F8kning","Ingen endring","Signifikant reduksjon"),
        "EN"=c("Significant increase","No change","Significant decrease")
      )[[LANGUAGE]]
      plotData[,change:=changeLevels[2]]
      plotData[num>upper,change:=changeLevels[1]]
      plotData[num<lower,change:=changeLevels[3]]
      plotData[,change:=factor(change,levels=changeLevels)]
      
      q <- ggplot(plotData, aes(x=num_previous))
      q <- q + geom_ribbon(data=confidenceIntervals, mapping=aes(ymax=log2(upperScaled),ymin=log2(lowerScaled)),alpha=0.5)
      q <- q + geom_hline(yintercept=0,lty=2,colour="red")
      q <- q + geom_label(aes(y=log2(percentageChange), label=vaccine, fill=change),hjust=0.5,vjust=0.5)
      if(nrow(plotData[change!=changeLevels[2]])>0) q <- q + geom_label(data=plotData[change!=changeLevels[2]],mapping=aes(y=log2(percentageChange), label=vaccine, fill=change))
      q <- q + scale_x_continuous(xTitle)
      q <- q + scale_fill_brewer("",palette="RdBu", drop = FALSE)
      if(USE_TITLE){
        q <- q + labs(title=title)
        q <- q + scale_y_continuous(
          yTitle,
          breaks=c(-10:10),labels=2^(-10:10))
      } else {
        q <- q + scale_y_continuous(
          title,
          breaks=c(-10:10),labels=2^(-10:10))
      }
      q <- q + labs(caption=DATA_CAPTION[[LANGUAGE]])
      q <- q + theme_gray(base_size=THEME_BASE_SIZE)
      if(comparison=="singleYearLast"){
        saveA4(q,
               filename=file.path(folder,"Figures_serotype_funnel_singleyearlast",sprintf("%s_Serotype_%s.png",LANGUAGE,a)))  
      } else if(comparison=="4yearbaseline"){
        saveA4(q,
               filename=file.path(folder,"Figures_serotype_funnel_4yearbaseline",sprintf("%s_Serotype_%s.png",LANGUAGE,a)))
      }
    }
  }
}

Figure_Funnel_Year <- function(rawSpecificNumbers,ageListFunnel,LANGUAGE,base_folder,yearOfInterest, USE_TITLE=TRUE){
  
  for(a in ageListFunnel[[LANGUAGE]]){
    for(comparison in c("singleYearLast","4yearbaseline")){
      Sys.sleep(1)
      
      seasonOfInterestIndex <- which(seasonOfInterest==seasonList)  
      if(comparison=="singleYearLast"){
        baseline <- yearOfInterest-1
      } else if(comparison=="4yearbaseline"){
        baseline <- c((yearOfInterest-1):(yearOfInterest-4))
      }
      #if(min(baseline)<"2013/2014") baseline <- baseline[1:which(baseline=="2013/2014")]
      
      baselineOldest <- baseline[length(baseline)]
      baselineSoonest <- baseline[1]
      
      if(yearOfInterest==TODAYS_YEAR){
        if(comparison=="singleYearLast"){
          title <- list(
            "NB"=sprintf("%s, sammenlikning av serotyper i %s med %s\n(kun data f\u00F8r uke %s)",
                         a,
                         yearOfInterest,
                         baselineSoonest,
                         TODAYS_WEEK
            ),
            "EN"=sprintf("%s, comparison of serotypes in %s with %s\n(only data before week %s)",
                         a,
                         yearOfInterest,
                         baselineSoonest,
                         TODAYS_WEEK
            )
          )
          
          plotData <- rawSpecificNumbers[week < TODAYS_WEEK & 
                                           time=="week" &
                                           age==a & 
                                           year %in% c(yearOfInterest,baseline) &
                                           !vaccine %in% c("missing","All IPD")]
          
        } else if(comparison=="4yearbaseline"){
          title <- list(
            "NB"=sprintf("%s, sammenlikning av serotyper i %s\nmed gjennomsnittet fra %s-%s (kun data f\u00F8r uke %s)",
                         a,
                         yearOfInterest,
                         baselineOldest,
                         baselineSoonest,
                         TODAYS_WEEK
            ),
            "EN"=sprintf("%s, comparison of serotypes in %s\nwith average from %s-%s (only data before week %s)",
                         a,
                         yearOfInterest,
                         baselineOldest,
                         baselineSoonest,
                         TODAYS_WEEK
            )
          )
          
          plotData <- rawSpecificNumbers[week < TODAYS_WEEK & 
                                           time=="week" &
                                           age==a & 
                                           year %in% c(yearOfInterest,baseline) &
                                           !vaccine %in% c("missing","All IPD")]
          plotData[year!=yearOfInterest,year:=baselineSoonest]
          plotData <- plotData[,.(num=mean(num)),by=.(vaccine,year,week)]
        }
      } else {
        if(comparison=="singleYearLast"){
          title <- list(
            "NB"=sprintf("%s, sammenlikning av serotyper i %s med %s",
                         a,
                         yearOfInterest,
                         baselineSoonest
            ),
            "EN"=sprintf("%s, comparison of serotypes in %s with %s",
                         a,
                         yearOfInterest,
                         baselineSoonest
            )
          )
          
          plotData <- rawSpecificNumbers[time=="week" & 
                                           age==a & 
                                           year %in% c(yearOfInterest,baseline) &
                                           !vaccine %in% c("missing","All IPD")]
        } else if(comparison=="4yearbaseline"){
          title <- list(
            "NB"=sprintf("%s, sammenlikning av serotyper i %s\nmed gjennomsnittet fra %s-%s",
                         a,
                         yearOfInterest,
                         baselineOldest,
                         baselineSoonest
            ),
            "EN"=sprintf("%s, comparison of serotypes in %s\nwith average from %s-%s",
                         a,
                         yearOfInterest,
                         baselineOldest,
                         baselineSoonest
            )
          )
          
          plotData <- rawSpecificNumbers[time=="week" & 
                                           age==a & 
                                           year %in% c(yearOfInterest,baseline) &
                                           !vaccine %in% c("missing","All IPD")]
          plotData[year!=yearOfInterest,year:=baselineSoonest]
          plotData <- plotData[,.(num=mean(num)),by=.(vaccine,year,week)]
        }
      }
      plotData <- plotData[,.(num=round(sum(num))),by=.(vaccine,year)]
      
      if(comparison=="singleYearLast"){
        xTitle <- list("NB"=sprintf("Antall tilfeller i %s",baselineSoonest),
                       "EN"=sprintf("Number of cases in %s",baselineSoonest))[[LANGUAGE]]
        
        yTitle <- list("NB"=sprintf("Forhold av (antall tilfeller i %s/antall tilfeller i %s)",yearOfInterest,baselineOldest),
                       "EN"=sprintf("Ratio of (number of cases in %s/number of cases in %s)",yearOfInterest,baselineOldest))[[LANGUAGE]]
      } else if(comparison=="4yearbaseline"){
        xTitle <- list("NB"=sprintf("Gjennomsnittlig antall tilfeller i %s-%s",baselineOldest,baselineSoonest),
                       "EN"=sprintf("Average number of cases in %s-%s",baselineOldest,baselineSoonest))[[LANGUAGE]]
        
        yTitle <- list("NB"=sprintf("Forhold av (antall tilfeller i %s/gjennomsnittet i %s-%s)",yearOfInterest,baselineOldest,baselineSoonest),
                       "EN"=sprintf("Ratio of (number of cases in %s/average in %s-%s)",yearOfInterest,baselineOldest,baselineSoonest))[[LANGUAGE]]
      }
      title <- title[[LANGUAGE]]
      
      setorder(plotData,vaccine,year)
      plotData[,num_previous:=num]
      plotData[year!=yearOfInterest,num:=NA]
      plotData[,num:=as.numeric(num)]
      plotData[,num:=max(num,na.rm=T),by=.(vaccine)]
      plotData <- plotData[year!=yearOfInterest]
      
      if(LANGUAGE=="NB"){
        plotData[,year_baseline:=sprintf("Referanse sesong: %s",year)]
      } else {
        plotData[,year_baseline:=sprintf("Reference season: %s",year)]
      }
      plotData[,year_baseline:=factor(year_baseline,levels=rev(sort(unique(plotData$year_baseline))))]
      
      plotData[num_previous==0, num_previous:=1]
      plotData[num==0, num:=1]
      plotData[,percentageChange := num/num_previous]
      
      upper <- qpois(0.975,1:max(plotData$num_previous))
      lower <- qpois(0.025,1:max(plotData$num_previous))
      confidenceIntervals <- data.table(num_previous=1:max(plotData$num_previous),upper,lower)
      confidenceIntervals[,upperScaled:=upper/num_previous]
      confidenceIntervals[,lowerScaled:=lower/num_previous]
      
      plotData <- merge(plotData,confidenceIntervals,by="num_previous")
      changeLevels <- list(
        "NB"=c("Signifikant \u00F8kning","Ingen endring","Signifikant reduksjon"),
        "EN"=c("Significant increase","No change","Significant decrease")
      )[[LANGUAGE]]
      plotData[,change:=changeLevels[2]]
      plotData[num>upper,change:=changeLevels[1]]
      plotData[num<lower,change:=changeLevels[3]]
      plotData[,change:=factor(change,levels=changeLevels)]
      
      q <- ggplot(plotData, aes(x=num_previous))
      q <- q + geom_ribbon(data=confidenceIntervals, mapping=aes(ymax=log2(upperScaled),ymin=log2(lowerScaled)),alpha=0.5)
      q <- q + geom_hline(yintercept=0,lty=2,colour="red")
      q <- q + geom_label(aes(y=log2(percentageChange), label=vaccine, fill=change),hjust=0.5,vjust=0.5)
      if(nrow(plotData[change!=changeLevels[2]])>0) q <- q + geom_label(data=plotData[change!=changeLevels[2]],mapping=aes(y=log2(percentageChange), label=vaccine, fill=change))
      q <- q + scale_x_continuous(xTitle)
      q <- q + scale_fill_brewer("",palette="RdBu", drop = FALSE)
      if(USE_TITLE){
        q <- q + labs(title=title)
        q <- q + scale_y_continuous(
          yTitle,
          breaks=c(-10:10),labels=2^(-10:10))
      } else {
        q <- q + scale_y_continuous(
          title,
          breaks=c(-10:10),labels=2^(-10:10))
      }
      q <- q + labs(caption=DATA_CAPTION[[LANGUAGE]])
      q <- q + theme_gray(base_size=THEME_BASE_SIZE)
      if(comparison=="singleYearLast"){
        saveA4(q,
               filename=file.path(base_folder,"Figures_serotype_funnel_singleyearlast",sprintf("%s_Serotype_%s.png",LANGUAGE,a)))  
      } else if(comparison=="4yearbaseline"){
        saveA4(q,
               filename=file.path(base_folder,"Figures_serotype_funnel_4yearbaseline",sprintf("%s_Serotype_%s.png",LANGUAGE,a)))
      }
    }
  }
}

