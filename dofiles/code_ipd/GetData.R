pop_get <- function(){
  retval <- fhidata::norway_population_current[level=="national",c("year","pop","age")]
  setnames(retval,"age","xage")
  
  return(retval)
}

data_get <- function(){
  if(org::PROJ$computer_id==1){
    channel <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};SERVER=dm-prod;DATABASE=MsisAnalyse;")
    masterData <- RODBC::sqlQuery(channel, "SELECT Serotype, Alder\u00C5r, AlderM\u00E5neder, Pr\u00F8vedato\u00C5r, Pr\u00F8vedatoM\u00E5ned, Pr\u00F8vedato FROM ViewNominativ WHERE Diagnose='Syst. pneumokokksykdom';")
    RODBC::odbcClose(channel)
    
    saveRDS(masterData, file=sprintf("%s/ipd.RDS",org::PROJ$DATA))
    
    #Serotype -> Pneusero
    #Alder\u00C5r -> Alaar
    #AlderM\u00E5neder -> Alm
    #Pr\u00F8vedato\u00C5r -> Paar
    #Pr\u00F8vedatoM\u00E5ned -> Pmnd
    #Pr\u00F8vedato -> Pdato
    
    masterData <- data.table(masterData)
    setnames(masterData, c("Pneusero", "Alaar", "Alm", "Paar", "Pmnd", "Pdato"))
    
  } else {
    masterData <- data.table(readRDS(file.path(
      org::PROJ$DATA,
      "ipd.RDS"
    )))
    
    setnames(masterData, c("Pneusero", "Alaar", "Alm", "Paar", "Pmnd", "Pdato"))
    
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
  
  return(masterData)
}