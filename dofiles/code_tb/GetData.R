GetData <- function(FOLDERS){
  if(.Platform$OS.type=="unix"){
    list.files("/analyses")
    list.files("/analyses/data_raw")
    masterData <- data.table(readRDS(file.path(
      "/data",
      "tb.RDS")))
    
  } else {
    channel <- RODBC::odbcDriverConnect(connection="Driver={SQL Server};SERVER=dm-prod;DATABASE=MsisAnalyse;")
    #masterData <- RODBC::sqlQuery(channel, "SELECT Pr\u00F8vedato\u00C5r, HendelseType, F\u00F8deland, MorsF\u00F8deland, FarsF\u00F8deland, F\u00F8deverdensdel, Aldersgruppe, Bostedsfylke, Innsykningsorgan, Indikasjon, Herkomst, PeriodeINorge, DyrkningResultat, MikroskopiLuftveisResultat, Rapp_smitteopp, Rifampicin, Isoniazid, Pyrazinamide, Ethambutol, Streptomycin, Behandlingsresultat, IGRAResultat FROM ViewTuberkulose;")
    #masterData <- RODBC::sqlQuery(channel, "SELECT Paar, Kategori, Fland, Mfland, Ffland, Fverd, Algr, Bofylk, Organ_1, Indik, Herkomst, Pernor, Dyrk_res, Dirluft_res, Rapp_smitteopp, R_res, H_res, P_res, E_res, S_res, Beh_res, IGRA FROM ViewTuberkulose;")
    masterData <- RODBC::sqlQuery(channel, "SELECT Paar, HendelseType, Fland, Mfland, Ffland, Fverd, Aldgr, Bofylk, Organ, Indik, Herkomst, Pernor, Dyrk_res, Dirluft_res, Rapp_smitteopp, res_R, res_H, res_P, res_E, res_S, Beh_res, IGRA FROM ViewTuberkulose;")
    
    saveRDS(masterData, file=sprintf("%s/tb.RDS",FOLDERS$RESULTS_DATA))
    
    
    RODBC::odbcClose(channel)
    masterData <- data.table(masterData)
    
    #saveRDS(masterData, file=sprintf("%s/pneumokokk_%s.RDS",RESULTS_BASE,todaysDate))
    #write.table(masterData, file=sprintf("%s/pneumokokk_%s.txt",RESULTS_BASE,todaysDate))
  }
    names(masterData) <- c(
      "Paar",
      "Kategori",
      "Fland",
      "Mfland",
      "Ffland",
      "Fverd",
      "Algr",
      "Bofylk",
      "Indik",
      "Herkomst",
      "Pernor",
      "Dyrk_res",
      "Dirluft_res",
      "R_res",
      "H_res",
      "P_res",
      "E_res",
      "S_res",
      "Beh_res",
      "IGRA"
      )
    
    # MorsF\u00F8deland -> Mfland
    # FarsF\u00F8deland -> Ffland
    # F\u00F8deverdensdel -> Fverd
    # Aldersgruppe -> Algr
    # Bostedsfylke -> Bofylk
    # Innsykningsorgan -> Organ_1 *
    # Indikasjon -> Indikasjon
    # Herkomst -> Herkomst
    # PeriodeINorge -> Pernor
    # DyrkningResultat -> Dyrk_res *
    # MikroskopiLuftveisResultat -> Dirluft_res
    # Rapp_smitteopp
    # Rifampicin -> R_res
    # Isoniazid -> H_res
    # Pyrazinamide -> P_res
    # Ethambutol -> E_res
    # Streptomycin -> S_res
    # Behandlingsresultat -> Beh_res
    # IGRAResultat -> IGRA

    #F\u00F8deland -> Fland
    #HendelseType -> Kategori
    #Smittestoff -> Smstoff
    #Alder\u00C5r -> Alaar
    #AlderM\u00E5neder -> Alm
    #Pr\u00F8vedato\u00C5r -> Paar
    #Pr\u00F8vedatoM\u00E5ned -> Pmnd
    #Pr\u00F8vedato -> Pdato
    #Metode -> Met
    #InnlagtSykehus -> Innlagt
  
  return(masterData)
}

CleanData <- function(data){
  
  FixNorwegian(data,"Fland")
  FixNorwegian(data,"Mfland")
  FixNorwegian(data,"Ffland")
  FixNorwegian(data,"Fverd")
  FixNorwegian(data,"Bofylk")
  #FixNorwegian(data,"Organ_1")
  FixNorwegian(data,"Indik")
  FixNorwegian(data,"Herkomst")
  FixNorwegian(data,"Pernor")
  FixNorwegian(data,"Beh_res")
  FixNorwegian(data,"Dyrk_res")
  FixNorwegian(data,"Dirluft_res")
  
  FixTrondelag(data,"Bofylk")
  
  countryMisspelling <- c(
    "Kasahkstan"="Kasakhstan"
  )
  RecodeDT(data,countryMisspelling,"Fland")
  RecodeDT(data,countryMisspelling,"Mfland")
  RecodeDT(data,countryMisspelling,"Ffland")
  
  ######
  data[,cyear:=Paar]
  
  ######
  data[!is.na(Kategori), isActive:=1]
  data[Kategori %in% c(
    "Feil diagnose",
    "Forebyggende behandling",
    "Innflyttet under behandling",
    "Vurderes"), isActive:=0]
  xtabs(~Kategori+isActive,data=data, exclude=NULL)
  with(data, table(Kategori, isActive, useNA="always"))
  
  ######
  data[!is.na(Fland), isForeignBorn:=1]
  data[Fland=="Norge", isForeignBorn:=0]
  with(data, table(Fland, isForeignBorn, useNA="always"))
  
  ######
  data[!is.na(Mfland), isForeignBornMother:=1]
  data[Mfland=="Norge", isForeignBornMother:=0]
  with(data, table(Mfland, isForeignBornMother, useNA="always"))
  
  ######
  data[!is.na(Ffland), isForeignBornFather:=1]
  data[Ffland=="Norge", isForeignBornFather:=0]
  with(data, table(Ffland, isForeignBornFather, useNA="always"))
  
  ######
  data[, isForeignBornParent:=0]
  data[isForeignBornMother==1 | isForeignBornFather==1, isForeignBornParent:=1]
  data[isForeignBornParent==0 & (is.na(isForeignBornMother) | is.na(isForeignBornFather)), isForeignBornParent:=NA]
  
  ######
  data[, cFverdNB:=as.character(Fverd)]
  data[Fverd %in% c("Nord-Amerika","S\u00F8r- og Mellom-Amerika"), cFverdNB:="Amerika"]
  data[Fverd %in% c("Europa"), cFverdNB:="Europa utenfor Norge"]
  data[isForeignBorn==0, cFverdNB:="Norge"]
  data[is.na(Fverd), cFverdNB:="Ukjent"]
  with(data, table(Fverd, cFverdNB, useNA="always"))
  
  ######
  data[, cFlandNB:=as.character(Fland)]
  
  ######
  data[, cAlgr:=as.character(Algr)]
  
  ######
  data[, cNorwegianStatusNB := as.character(NA)]
  data[isForeignBorn==0 & isForeignBornParent==0, cNorwegianStatusNB:="Norskf\u00F8dt med to norskf\u00F8dte foreldre"]
  data[isForeignBorn==0 & isForeignBornParent==1, cNorwegianStatusNB:="Norskf\u00F8dt med minst en utenlandsf\u00F8dt forelder"]
  data[isForeignBorn==1, cNorwegianStatusNB:="Utenlandsf\u00F8dte"]
  
  ######
  data[, cFylke := as.character(Bofylk)]
  data[Bofylk=="Oslo (f)", cFylke:="Oslo"]
  data[Bofylk=="Utenfor Fastlands-Norge", cFylke:="Utenfor fastlandet"]
  
  ######
  # data[, cOrganNB:= Organ_1]
  # data[Organ_1 %in% c(
  #   "Columna",
  #   "Ben/ledd utenom columna",
  #   "Columna/ben/ledd"
  # ), cOrganNB:="Ben/ledd/columna"]
  # 
  # data[Organ_1 %in% c(
  #   "Sentralnervesystem annet enn meninger",
  #   "Meninger",
  #   "Meninger/CNS"
  # ), cOrganNB:="Sentralnervesystem"]
  # 
  # data[Organ_1 %in% c(
  #   "Lymfe/hilusglandler"
  # ), cOrganNB:="Lymfeknuter"]
  # 
  # with(data, table(Organ_1, cOrganNB, useNA="always"))
  # 
  # ######
  # data[, cOrganLungsVsLymphNB := as.character(NA)]
  # data[!is.na(cOrganNB), cOrganLungsVsLymphNB:="Alle andre TB tilfeller"]
  # data[cOrganNB == "Lunge", cOrganLungsVsLymphNB:="TB i lunger"]
  # data[cOrganNB == "Lymfeknuter", cOrganLungsVsLymphNB:="TB i lymfeknuter"]
  
  ######
  data[, cIndikNB := Indik]
  data[is.na(Indik), cIndikNB:="Ubesvart"]
  
  ######
  data[, cHerkomstNB := Herkomst]
  data[is.na(Herkomst), cHerkomstNB := "Ubesvart"]
  
  ######
  data[, cPernorNB := Pernor]
  data[is.na(Pernor), cPernorNB := "Ubesvart"]
  
  data[Pernor %in% c(
    "Under en m\u00E5ned",
    "1-6 m\u00E5neder",
    "6-12 m\u00E5neder"
  ), cPernorNB := "Under ett \u00E5r i Norge"]
  
  data[Pernor %in% c(
    "1-2 \u00E5r",
    "3-4 \u00E5r"
  ), cPernorNB := "Ett til fire \u00E5r i Norge"]
  
  data[Pernor %in% c(
    "5-9 \u00E5r",
    "10 \u00E5r eller mer"
  ), cPernorNB := "5 \u00E5r eller mer i Norge"]
  
  ######
  data[, cDyrkResAllOrganNB := Dyrk_res]
  data[Dyrk_res %in% c(
    "Ikke utf\u00F8rt",
    "Ukjent"
  ) | is.na(Dyrk_res), cDyrkResAllOrganNB := "Ukjent/ikke utf\u00F8rt"]
  
  #data[, cDyrkResLungNB := cDyrkResAllOrganNB]
  #data[cOrganNB=="Lunge", cDyrkResLungNB := NA]
  
  ######
  data[, cDirluftAllOrganNB := Dirluft_res]
  data[Dirluft_res %in% c(
    "Ikke utf\u00F8rt",
    "Ukjent"
  ) | is.na(Dirluft_res), cDirluftAllOrganNB := "Ukjent/ikke utf\u00F8rt"]
  
  #data[, cDirluftLungNB := cDirluftAllOrganNB]
  #data[cOrganNB=="Lunge", cDirluftLungNB := NA]
  
  ######
  #data[,isNeedsReport:=0]
  #data[cOrganNB=="Lunge" & cDyrkResLungNB=="Positivt", isNeedsReport:=1]
  
  ######
  # data[, Rapp_smitteopp:=gsub(" ", "", Rapp_smitteopp)]
  # data[, isReported:=NA]
  # data[isNeedsReport==1, isReported:=0]
  # data[!is.na(isReported) & Rapp_smitteopp=="Ja", isReported:=1]
  # 
  ######
  for(x in c("R","H","P","E","S")){
    uncleaned <- paste0(x,"_res")
    cleaned <- paste0("is",x,"Res")
    
    txt <- paste0('data[cDyrkResAllOrganNB=="Positiv",',cleaned,':=0]')
    eval(parse(text=txt))
    
    txt <- paste0('data[!is.na(',cleaned,') & ',uncleaned,'%in% c("Lavgradig resistens","Resistent"),',cleaned,':=1]')
    eval(parse(text=txt))
  }
  
  ######
  data[isActive==1 & cDyrkResAllOrganNB=="Positiv", isMDR:=0]
  xtabs(~data$cDyrkResAllOrganNB+data$isActive)
  xtabs(~data$isMDR)
  data[!is.na(isMDR) & isRRes==1 & isHRes==1, isMDR:=1]
  xtabs(~data$isMDR)
  ######
  data[,cBirthPlaceSovietNB:=cFverdNB]
  
  data[cFverdNB %in% c(
    "Europa utenfor Norge"
  ), cBirthPlaceSovietNB:="Andre europeiske land"]
  
  data[cBirthPlaceSovietNB %in% c(
    "Andre europeiske land"
  ) & cFlandNB %in% c(
    "Armenia",
    "Aserbajdsjan",
    "Belarus",
    "Estland",
    "Georgia",
    "Kasahkstan",
    "Latvia",
    "Litauen",
    "Moldova",
    "Russland",
    "Tadsjikistan",
    "Turkmenistan",
    "Ukraina",
    "Usbekistan"
  ), cBirthPlaceSovietNB:="Tidligere Sovjet"]
  
  ######
  #data[cOrganNB=="Lunge" & Kategori=="Tuberkulose for f\u00F8rste gang" & isActive==1, isLungTBFirstTime:=1]
  
  ######
  data[,cTreatmentResNB:=Beh_res]
  data[is.na(Beh_res), cTreatmentResNB := "Mangler"]
  data[,cTreatmentResNB := trimws(cTreatmentResNB)]
  
  ######
  data[,cTreatmentResCollapsedNB := cTreatmentResNB]
  data[cTreatmentResNB %in% c(
    "D\u00F8d av annen \u00E5rsak",
    "D\u00F8d av ukjent \u00E5rsak",
    "D\u00F8d av tuberkulose",
    "D\u00F8d tub medvirkende"
  ), cTreatmentResCollapsedNB:="D\u00F8d"]
  
  data[cTreatmentResNB %in% c(
    "Forsvunnet fra behandling"
  ), cTreatmentResCollapsedNB:="Forsvunnet"]
  
  data[cTreatmentResNB %in% c(
    "Reist frivillig ut av landet"
  ), cTreatmentResCollapsedNB:="Reist frivillig"]
  
  data[cTreatmentResNB %in% c(
    "Bortvist fra landet"
  ), cTreatmentResCollapsedNB:="Bortvist"]
  
  data[cTreatmentResNB %in% c(
    "MDRTB behandlingsregime"
  ), cTreatmentResCollapsedNB:="Fortsatt under behandling"]
  
  ######
  unique(data$Kategori)
  data[!is.na(Kategori), isLatentTB:=0]
  data[Kategori=="Forebyggende behandling", isLatentTB:=1]
  
  ######
  data[isLatentTB==1,cIGRAResultNB:="Ukjent/ubesvart/gr\u00E5sone/inkonklusiv IGRA"]
  data[isLatentTB==1 & IGRA=="Negativ IGRA",cIGRAResultNB:="Negativ IGRA"]
  data[isLatentTB==1 & IGRA=="Positiv IGRA",cIGRAResultNB:="Positiv IGRA"]
  
  ######
  cleanedVars <- unique(c(grep("^c",names(data)),grep("^is",names(data))))
  cleanData <- data[,cleanedVars,with=F]
}