
GetDataTub <- function(FOLDERS){

  if(.Platform$OS.type=="unix"){
    masterData <- data.table(readRDS(file.path(
      "/analyses",
      "data_raw",
      "internal_surveillance",
      "pneumokokk_2017-09-21.RDS")))
    
  } else {
    channel <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};SERVER=sql-emsis;DATABASE=eMSIS_Research;")
    masterData <- RODBC::sqlQuery(channel, "SELECT * FROM ViewTubData")
    
    RODBC::odbcClose(channel)
    masterData <- data.table(masterData)
    #saveRDS(masterData, file=sprintf("test_MSIS_monthly.RDS",RESULTS_BASE,todaysDate))
    
    #saveRDS(masterData, file="data/test_MSIS_monthly.RDS")
    
  }
  
  return(masterData)
}
