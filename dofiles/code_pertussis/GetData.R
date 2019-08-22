SavePop <- function(FOLDERS){
  try({
    pop <- data.table(read.csv(url("https://data.ssb.no/api/v0/dataset/59322.csv?lang=en"),stringsAsFactors = FALSE))
    pop <- pop[sex=="0 Both sexes"]
    pop[,sex:=NULL]
    pop[,contents:=NULL]
    pop[,x:=as.numeric(stringr::str_extract(age,"^[0-9][0-9][0-9]"))]
    pop[,age:=NULL]
    setnames(pop,c("year","pop","xage"))
    for(i in 1:5){
      popx <- pop[year==max(year)]
      popx[,year:=year+1]
      pop <- rbind(pop,popx)
    }
    saveRDS(pop,file.path(FOLDERS$DOFILES_DATA,"pertussis_pop.RDS"))
  },TRUE)
}

GetData <- function(){
  if(org::PROJ$computer_id==1){
    channel <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};SERVER=dm-prod;DATABASE=MsisAnalyse;")
    masterData <- RODBC::sqlQuery(channel, "SELECT Alder\u00C5r, AlderM\u00E5neder, Pr\u00F8vedato\u00C5r, Pr\u00F8vedatoM\u00E5ned, Pr\u00F8vedato, Metode, ErInnlagtSykehus, Utfall FROM ViewNominativ WHERE Diagnose='Kikhoste';")
    #masterData <- RODBC::sqlQuery(channel, "SELECT Alaar, Alm, Paar, Pmnd, Pdato, Met, Innlagt, Utfall FROM ViewAnonyme WHERE Diagnose='Kikhoste';")
    RODBC::odbcClose(channel)
    
    saveRDS(masterData, file=sprintf("%s/pertussis.RDS",org::PROJ$DATA))
    
    #saveRDS(masterData, file=sprintf("%s/pneumokokk_%s.RDS",RESULTS_BASE,todaysDate))
    #write.table(masterData, file=sprintf("%s/pneumokokk_%s.txt",RESULTS_BASE,todaysDate))
    
    #Smittestoff -> Smstoff
    #Alder\u00C5r -> Alaar
    #AlderM\u00E5neder -> Alm
    #Pr\u00F8vedato\u00C5r -> Paar
    #Pr\u00F8vedatoM\u00E5ned -> Pmnd
    #Pr\u00F8vedato -> Pdato
    #Metode -> Met
    #InnlagtSykehus -> Innlagt
    
    names(masterData) <- c("Alaar", "Alm", "Paar", "Pmnd", "Pdato", "Met", "Innlagt", "Utfall")
    masterData <- data.table(masterData)
    
  } else {
    masterData <- data.table(readRDS(file.path(
      org::PROJ$DATA,
      "pertussis.RDS"
    )))
    
    names(masterData) <- c("Alaar", "Alm", "Paar", "Pmnd", "Pdato", "Met", "Innlagt", "Utfall")
    
    if(TODAYS_YEAR>max(masterData$Paar)){
      missingYears <- TODAYS_YEAR:(max(masterData$Paar)+1)
      retval <- list()
      retval[[1]] <- masterData
      for(i in missingYears){
        retval[[i]] <- masterData[Paar==max(masterData$Paar)]
        retval[[i]][,Paar:=i]
        retval[[i]][,Pdato:=as.POSIXct(stringr::str_replace(Pdato,sprintf("^%s",max(masterData$Paar)),as.character(i)))]
      }
      masterData <- rbindlist(retval)
    }
  }
  
  FixNorwegian(masterData,"Utfall")
  FixNorwegian(masterData,"Innlagt")
  FixNorwegian(masterData,"Met")
  
  masterData[!is.na(Utfall),isDead:=0]
  masterData[Utfall=="D\u00F8d av sykdommen",isDead:=1]
  
  return(masterData)
}