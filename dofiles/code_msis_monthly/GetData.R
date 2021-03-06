
GetData <- function(FOLDERS,diag){
  if(.Platform$OS.type=="unix"){
    masterData <- data.table(readRDS(file.path(
      "/analyses",
      "data_raw",
      "internal_surveillance",
      "pneumokokk_2017-09-21.RDS")))
    
  } else {
    channel <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};SERVER=sql-emsis;DATABASE=eMSIS_Research;")
    s <- sprintf ("SELECT * FROM ViewNominative  WHERE Diag in %s;", diag)
    masterData <- RODBC::sqlQuery(channel, s)
    RODBC::odbcClose(channel)
    masterData <- data.table(masterData)
  }
  
  return(masterData)
}
