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
  if(.Platform$OS.type=="unix"){
    masterData <- data.table(readRDS(file.path(
      "/data",
      "meningococcal_2018-01-20.RDS")))
    
  } else {
    channel <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};SERVER=dm-prod;DATABASE=MsisAnalyse;")
    masterData <- RODBC::sqlQuery(channel, "SELECT Smittestoff, Alder\u00C5r, Pr\u00F8vedato\u00C5r, Pr\u00F8vedatoM\u00E5ned FROM ViewAnonyme WHERE Diagnose='Syst. meningokokksykdom';")
    
    saveRDS(masterData, file=sprintf("%s/meningococcal.RDS",FOLDERS$RESULTS_DATA))
    
    names(masterData) <- c("Smstoff", "Alaar", "Alm", "Paar", "Pmnd")
    
    #Smittestoff -> Smstoff
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
  
  masterData[,Smstoff:=as.character(Smstoff)]
  #FixNorwegian(masterData,"Smstoff")
  for(i in 0:9) masterData[,Smstoff:=gsub(i,"",Smstoff)]
  masterData[,Smstoff:=trimws(Smstoff,which="both")]
  masterData[Smstoff=="Neisseria meningitidis ina",Smstoff:="Nm ina"]
  
  return(masterData)
}