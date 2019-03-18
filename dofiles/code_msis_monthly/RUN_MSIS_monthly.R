
################################
########## SMSB  ###############
################################
if (group=="SMSB") {

DATA <- GetData(FOLDERS,diag)
  # 1. General Table
  dataset <- Data.Clean (DATA, group)
  dataset <- calculate.totals.mean (dataset)
  dataset <- prep.to.table(dataset, group)
  dataset[Smstoff=="Hep B-virus, akutt", Smstoff:="HepBakutt"]
  dataset[Smstoff=="Hep B-virus, kronisk", Smstoff:="HepBkron"]
  dataset[Smstoff=="Hep C-virus", Smstoff:="Hepc"]
  mytable1 <- create.table(dataset,group)
  savetables(mytable1,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, group, F)
  
  ## TUB
DATA <- GetDataTub(FOLDERS)
  # 2. AktivTB

  tablenumber <- 1
  dataset <- get.tub.akti(DATA)
  dataset <- Data.Clean.TUB(dataset,group,tablenumber)
  dataset <- calculate.totals.mean.TUB (dataset,tablenumber)
  mytable2 <- create.table.TUB(dataset,tablenumber)
  savetables(mytable2,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, "Aktiv TB",F)
  
  # 3. TBkvartal
  tablenumber <- 2
  dataset <-  get.tub.akti(DATA)
  dataset <- Data.Clean.TUB(dataset,group,tablenumber)
  dataset <- calculate.totals.mean.TUB (dataset,tablenumber)
  mytable3 <- create.table.TUB(dataset,tablenumber)
  savetables(mytable3,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, "TB kvartal",F)
  
  # 4. TBresistens
  tablenumber <- 3
  dataset <- get.tub.akti(DATA)
  dataset <- Data.Clean.TUB(dataset,group,tablenumber)
  dataset <- calculate.totals.mean.TUB (dataset,tablenumber)
  mytable4 <- create.table.TUB(dataset,tablenumber)
  savetables(mytable4,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, "TB resistens",F)
  
}

################################
########## SMSL  ###############
################################

if (group=="SMSL") {
  
  DATA <- GetData(FOLDERS,diag)
  # 1. General Table
  dataset <-Data.Clean (DATA, group)
  dataset <- calculate.totals.mean (dataset)
  dataset <- prep.to.table(dataset, group)
  mytable1 <- as.data.frame(create.table(dataset,group))
  savetables(mytable1,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, group,F)
  
  
  # 2. Pneumokokk & Pneumosero
  dataset <- Data.Clean (DATA, group)
  dataset <- select.diag.SMSL(dataset, 1)
  dataset <- create.age.groups(dataset, 1)
  
  mytable2 <- create.table.Pneum(dataset,1,1)
  savetables(mytable2,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, "Pneumokokk",F)
  
  mytable3 <- create.table.Pneum(dataset,1,2)
  savetables(mytable3,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, "Pneumosero",F)
  
  # 3. Kikhoste
  dataset <- Data.Clean (DATA, group)
  dataset <- select.diag.SMSL(dataset, 2)
  dataset <- create.age.groups(dataset, 2)
  mytable4 <- create.table.Pneum(dataset,2,1)
  savetables(mytable4,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, "Kikhoste", F)
  
  mytable5 <-create.table.Pneum(dataset,2,2)
  savetables(mytable5,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, "kikhoste Antall til Innlagt", F)
  
  
}
################################
########## SMSH  ###############
################################

if (group=="SMSH") {
  # 1. General Table
  DATA <- GetData(FOLDERS,diag)

  dataset <-Data.Clean (DATA, group)
  
  #dataset <- change.diag.names (dataset, group)
  ### function  change.diag.names does not work 
  dataset[,Smstoff:=iconv(dataset$Smstoff, "latin1","ASCII//TRANSLIT")]
  dataset[,Infeksjonsstatus:=iconv(dataset$Infeksjonsstatus, "latin1","ASCII//TRANSLIT")]

  dataset[Diag=="MRSA"& Infeksjonsstatus=="Barerskap", Smstoff:="Staph aureus meticillinres, baring"]
  dataset[Diag=="MRSA"& Infeksjonsstatus=="Infeksjon", Smstoff:="Staph aureus meticillinres, inf"]
  dataset[Diag=="MRSA"& Infeksjonsstatus=="Ukjent", Smstoff:="Staph aureus meticillinres, ukjentstatus"]
  dataset[Diag=="MRSA"& is.na(Infeksjonsstatus)==T, Smstoff:="Staph aureus meticillinres, ukjentstatus"]
  ### function  change.diag.names does not work
  dataset <- calculate.totals.mean (dataset)
  dataset <- prep.to.table(dataset, group)
  mytable1 <- create.table(dataset ,group)
  savetables(mytable1,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, group,F)
  
  
}
###############################
########## SMSM  ###############
################################

if (group=="SMSM") {
  
DATA <- GetData(FOLDERS,diag)

  # 1. General Table
  dataset <-Data.Clean (DATA, group)
  dataset <- calculate.totals.mean (dataset)
  dataset=prep.to.table(dataset, group)
  mytable=create.table(dataset,group)
  ind= apply(mytable, 1, function(x) all(is.na(x)))
  mytable1=mytable[!ind,]
  savetables(mytable1,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, group,F)
  
  # 2. ecolienterittunntattEHEC
  dataset <-Data.Clean (DATA, group)
  dataset <- calculate.totals.mean (dataset)
  dataset <- dataset[Diag=="E. coli-enteritt unntatt EHEC",]
  dataset=prep.to.table(dataset, group)
  mytable2=create.table(dataset,group)
  savetables(mytable2,FOLDERS$RESULTS_TODAY_ALL_WITH_TITLES,group, "SMSM ecolienterittunntattEHEC",F)
  
}




