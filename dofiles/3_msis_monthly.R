library(data.table)
library(ggplot2)
library(scales)

if(.Platform$OS.type=="unix"){
  setwd(file.path("/autosurveillance", "dofiles"))
} else {
  setwd(file.path("G:", "Helseregistre", "MSIS",
                  "Sp\u00F8rringer og data UTEN person-id",
                  "Auto_surveillance", "dofiles"))
}

fileSources = file.path("code_shared",list.files("code_shared",pattern="*.[rR]$"))
sapply(fileSources,source,.GlobalEnv)

fileSources = file.path("code_msis_monthly",list.files("code_msis_monthly",pattern="*.[rR]$"))

############################################################################
Group = c("SMSB", "SMSL","SMSH","SMSM")
test= c(SMSB, SMSL, SMSH, SMSM)

for (g in 1:4) {
  group <- Group[g]
  diag <- test[g]
  FOLDERS <- SetDirectories(type=paste("MSIS",group,sep="/"))
  print(FOLDERS)
  ############################
  # source(file.path("code_msis_monthly","RUN_MSIS_monthly.R"))
  
}

############################################################################

