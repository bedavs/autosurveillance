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
    saveRDS(pop,file.path(FOLDERS$DOFILES_DATA,"ipd_pop.RDS"))
  },TRUE)
}

GetData <- function(FOLDERS){
  if(.Platform$OS.type=="unix"){
    masterData <- data.table(readRDS(file.path(
      "/data",
      "pneumokokk_2017-11-28.RDS")))
    
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
    
  } else {
    channel <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};SERVER=dm-prod;DATABASE=MsisAnalyse;")
    masterData <- RODBC::sqlQuery(channel, "SELECT Serotype, Alder\u00C5r, AlderM\u00E5neder, Pr\u00F8vedato\u00C5r, Pr\u00F8vedatoM\u00E5ned, Pr\u00F8vedato FROM ViewNominativ WHERE Diagnose='Syst. pneumokokksykdom';")
    
    saveRDS(masterData, file=sprintf("%s/ipd.RDS",FOLDERS$RESULTS_DATA))
    
    names(masterData) <- c("Pneusero", "Alaar", "Alm", "Paar", "Pmnd", "Pdato")
    
    #Serotype -> Pneusero
    #Alder\u00C5r -> Alaar
    #AlderM\u00E5neder -> Alm
    #Pr\u00F8vedato\u00C5r -> Paar
    #Pr\u00F8vedatoM\u00E5ned -> Pmnd
    #Pr\u00F8vedato -> Pdato

    #saveRDS(masterData, file=sprintf("%s/pneumokokk_%s.RDS",RESULTS_BASE,todaysDate))
    #write.table(masterData, file=sprintf("%s/pneumokokk_%s.txt",RESULTS_BASE,todaysDate))
    
    RODBC::odbcClose(channel)
    masterData <- data.table(masterData)
    
  }
  
  return(masterData)
}