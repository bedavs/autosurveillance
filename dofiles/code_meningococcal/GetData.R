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
    saveRDS(pop,file.path(FOLDERS$DOFILES_DATA,"meningococcal_pop.RDS"))
  },TRUE)
}

GetData <- function(FOLDERS){
  if(org::PROJ$computer_id==1){
    channel <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};SERVER=dm-prod;DATABASE=MsisAnalyse;")
    masterData <- RODBC::sqlQuery(channel, "SELECT MeningokokkSubtype, Alder\u00C5r, Pr\u00F8vedato\u00C5r, Pr\u00F8vedatoM\u00E5ned FROM ViewNominativ WHERE Diagnose='Syst. meningokokksykdom';")
    RODBC::odbcClose(channel)
    
    saveRDS(masterData, file=sprintf("%s/meningococcal.RDS",org::PROJ$DATA))
    
    names(masterData) <- c("Smstoff", "Alaar", "Paar", "Pmnd")
    
    #Smittestoff -> Smstoff
    #Alder\u00C5r -> Alaar
    #AlderM\u00E5neder -> Alm
    #Pr\u00F8vedato\u00C5r -> Paar
    #Pr\u00F8vedatoM\u00E5ned -> Pmnd
    #Pr\u00F8vedato -> Pdato
    
    masterData <- data.table(masterData)
    
  } else {
    masterData <- data.table(readRDS(file.path(
      org::PROJ$DATA,
      "meningococcal.RDS"
    )))
    
    names(masterData) <- c("Smstoff", "Alaar", "Paar", "Pmnd")
  }
    
  
  masterData[,Smstoff:=as.character(Smstoff)]
  #FixNorwegian(masterData,"Smstoff")
  for(i in 0:9) masterData[,Smstoff:=gsub(i,"",Smstoff)]
  masterData[,Smstoff:=trimws(Smstoff,which="both")]
  masterData[Smstoff=="Neisseria meningitidis ina",Smstoff:="Nm ina"]
  masterData[is.na(Smstoff),Smstoff:="Nm ina"]
  masterData[Smstoff!="Nm ina",Smstoff:=paste0("Nm ",Smstoff)]
  
  print(xtabs(~masterData$Smstoff,addNA=T))
  
  return(masterData)
}

