if(.Platform$OS.type=="unix"){
  setwd(file.path(
    "/git", "internal_surveillance", "dofiles"))
} else {
  setwd(file.path("G:", "Helseregistre", "MSIS",
                  "Sp\u00F8rringer og data UTEN person-id",
                  "Auto_surveillance", "dofiles"))
}

source("1_ipd.R")
source("2_tb.R")
source("4_pertussis.R")
source("5_meningococcal.R")


# finished
for(i in 1:10) print("FINISHED RUNNING EVERYTHING")
