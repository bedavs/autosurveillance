library(data.table)
library(ggplot2)

if(.Platform$OS.type=="unix"){
  setwd(file.path("/autosurveillance", "dofiles"))
} else {
  setwd(file.path("G:", "Helseregistre", "MSIS",
    "Sp\u00F8rringer og data UTEN person-id",
    "Auto_surveillance", "dofiles"))
}

fileSources = file.path("code_shared",list.files("code_shared",pattern="*.[rR]$"))
sapply(fileSources,source,.GlobalEnv)

fileSources = file.path("code_ipd",list.files("code_ipd",pattern="*.[rR]$"))
sapply(fileSources,source,.GlobalEnv)

FOLDERS <- SetDirectories(type="IPD")

if(.Platform$OS.type=="unix") SavePop(FOLDERS)
masterData <- GetData(FOLDERS)
masterPop <- readRDS(file.path(FOLDERS$DOFILES_DATA,"ipd_pop.RDS"))

# Original
# Definition of serotypes according to vaccines
# All IPD: Includes all cases reported (also those with missing serotypes)
# PCV7: 4, 6B, 9V, 14, 18C, 19F, 23F
# PCV10non7: 1, 5, 7F
# PCV13non10: 3, 6A, 19A
# PCV13: all of the above (4, 6B, 9V, 14, 18C, 19F, 23F, 1, 5, 7F, 3, 6A and 19A)
# nonPCV13: all ST not included in PCV13
# PPV23: 1, 2, 3, 4, 5, 6B, 7F, 8, 9N, 9V, 10A, 11A, 12F, 14, 15B, 17F, 18C, 19A, 19F, 20, 22F, 23F, 33F
# PPV23nonPCV13: ST included in PPV23, but not in PCV13 (2, 8, 9N, 10A, 11A, 12F, 15B, 17F, 20, 22F, 33F)*
# NVT: ST not included in PCV13 or PPV23 (i.e. different from PPV23 + 6A serotypes)
# missing: Cases without serotype reported (for now you can included non-typeable/rough
# 
# *PCV13 includes serotype 6A, which is not included in PPV23 (so numbers don’t sum up correctly)

# Added on 2017-12-04
# The PCV15 (Mercks) will include all PCV13 serotypes AND serotypes 22F and 33F.
# The cPCV7 (Pfizer)will include serotypes 8, 10A, 11A, 12F, 15B, 22F, and 33F
# The PCV20 (Pfizer) will include all PCV13 serotypes and  cPCV7 serotypes

vaxDefMaster <- list(
  "All IPD"=unique(masterData$Pneusero),
  "PCV7"=c("4","6B","9V","14","18C","19F","23F"),
  "PCV10non7"=c("1","5","7F"),
  "PCV13non10"=c("3","6A","19A"),
  "PCV13"=c(),
  "PCV13non7"=c(),
  "nonPCV13"=c(),
  "PPV23"=c("1", "2", "3", "4", "5", "6B", "7F", "8", "9N", "9V", "10A", "11A", "12F", "14", "15B", "17F", "18C", "19A", "19F", "20", "22F", "23F", "33F"),
  "PPV23nonPCV13"=c(),
  "NVT"=c(),
  "PCV15"=c(),
  "cPCV7 (8, 10A, 11A, 12F, 15B, 22F, 33F)"=c("8","10A","11A","12F","15B","22F","33F"),
  "22F + 33F"=c("22F","33f"),
  "missing"=NA)

vaxDefMaster[["PCV13"]] = c(vaxDefMaster[["PCV7"]],vaxDefMaster[["PCV10non7"]],vaxDefMaster[["PCV13non10"]])
vaxDefMaster[["PCV13non7"]] = vaxDefMaster[["PCV13"]][!vaxDefMaster[["PCV13"]] %in% c(NA,vaxDefMaster[["PCV7"]])] 
vaxDefMaster[["nonPCV13"]] = vaxDefMaster[["All IPD"]][!vaxDefMaster[["All IPD"]] %in% c(NA,vaxDefMaster[["PCV13"]])]
vaxDefMaster[["PPV23nonPCV13"]] = vaxDefMaster[["PPV23"]][!vaxDefMaster[["PPV23"]] %in% c(NA,vaxDefMaster[["PCV13"]])] 
vaxDefMaster[["NVT"]] = vaxDefMaster[["All IPD"]][!vaxDefMaster[["All IPD"]] %in% c(NA,vaxDefMaster[["PCV13"]],vaxDefMaster[["PPV23"]])] 
vaxDefMaster[["PCV20"]] = c(vaxDefMaster[["PCV13"]],vaxDefMaster[["cPCV7"]])

ageDef <- list(
  "NB"=list(
    "Yngre enn 2 \u00E5r"=c(0,1),
    "2 til 4 \u00E5r"=c(2:4),
    "Yngre enn 5 \u00E5r"=c(0:4),
    "5 til 19 \u00E5r"=c(5:19),
    "20 til 49 \u00E5r"=c(20:49),
    "50 til 64 \u00E5r"=c(50:64),
    "5 til 64 \u00E5r"=c(5:64),
    "Yngre enn 65 \u00E5r"=c(0:64),
    "65+ \u00E5r"=c(65:200),
    "5+ \u00E5r"=c(5:200),
    "65 til 74 \u00E5r"=c(65:74),
    "75 til 84 \u00E5r"=c(75:84),
    "85+ \u00E5r"=c(85:200),
    "Alle aldre"=c(0:200)
    ),
  "EN"=list(
    "Less than 2 years"=c(0,1),
    "2 to 4 years"=c(2:4),
    "Less than 5 years"=c(0:4),
    "5 to 19 years"=c(5:19),
    "20 to 49 years"=c(20:49),
    "50 to 64 years"=c(50:64),
    "5 to 64 years"=c(5:64),
    "Less than 65 years"=c(0:64),
    "65+ years"=c(65:200),
    "5+ years"=c(5:200),
    "65 to 74 years"=c(65:74),
    "75 to 84 years"=c(75:84),
    "85+ years"=c(85:200),
    "All ages"=c(0:200)
  )
)

ageListRestricted <- list(
  "NB"=c(
    "Yngre enn 2 \u00E5r",
    "2 til 4 \u00E5r",
    "5 til 19 \u00E5r",
    "20 til 49 \u00E5r",
    "50 til 64 \u00E5r",
    "65+ \u00E5r"),
  "EN"=c(
    "Less than 2 years",
    "2 to 4 years",
    "5 to 19 years",
    "20 to 49 years",
    "50 to 64 years",
    "65+ years")
)

ageListRestrictedAll <- list(
  "NB"=c(
    "Yngre enn 2 \u00E5r",
    "2 til 4 \u00E5r",
    "5 til 19 \u00E5r",
    "20 til 49 \u00E5r",
    "50 til 64 \u00E5r",
    "65+ \u00E5r",
    "Alle aldre"),
  "EN"=c(
    "Less than 2 years",
    "2 to 4 years",
    "5 to 19 years",
    "20 to 49 years",
    "50 to 64 years",
    "65+ years",
    "All ages")
)

ageListOld <- list(
  "NB"=c(
    "65 til 74 \u00E5r",
    "75 til 84 \u00E5r",
    "85+ \u00E5r"),
  "EN"=c(
    "65 to 74 years",
    "75 to 84 years",
    "85+ years")
)

ageListSimpsons <- list(
  "NB"=c(
    "65 til 74 \u00E5r",
    "75 til 84 \u00E5r",
    "85+ \u00E5r",
    "Alle aldre"),
  "EN"=c(
    "65 to 74 years",
    "75 to 84 years",
    "85+ years",
    "All ages")
)

ageListFunnel <- list(
  "NB"=c(
    "Yngre enn 65 \u00E5r",
    "65+ \u00E5r",
    "Alle aldre"),
  "EN"=c(
    "Less than 65 years",
    "65+ years",
    "All ages")
)

ageListCumulative <- list(
  "NB"=c(
    "Alle aldre",
    "Yngre enn 5 \u00E5r",
    "65+ \u00E5r",
    "65 til 74 \u00E5r",
    "75 til 84 \u00E5r",
    "85+ \u00E5r"),
  "EN"=c(
    "All ages",
    "Less than 5 years",
    "65+ years",
    "65 to 74 years",
    "75 to 84 years",
    "85+ years")
)

vaxDefSpecific <- list()
for(i in as.character(unique(masterData$Pneusero))){
  if(is.na(i)) next
  if(i=="") next
  vaxDefSpecific[[i]] <- i
}
vaxDefSpecific[["All IPD"]] <- unique(masterData$Pneusero)
vaxDefSpecific[["missing"]] <- NA

vaxDefIncidenceAgeMaster <- c("All IPD","NVT","PCV13non7","PPV23nonPCV13","PCV7")

for(SUPERFOLDER in c("ALL_WITHOUT_TITLES","ALL_WITH_TITLES","SHAREPOINT")){
  print(SUPERFOLDER)
  BASE_FOLDER <- BaseFolder(SUPERFOLDER = SUPERFOLDER, FOLDERS = FOLDERS)
  if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
    USE_TITLE <- FALSE
  } else {
    USE_TITLE <- TRUE
  }
  
  for(LANGUAGE in c("NB","EN")){
    # START RUNNING CODE
    vaxDef <- vaxDefMaster
    vaxDefIncidenceAge <- vaxDefIncidenceAgeMaster
    if(LANGUAGE=="NB"){
      names(vaxDef)[names(vaxDef)=="NVT"] <- "Ikke-vaksine serotyper"
      vaxDefIncidenceAge[vaxDefIncidenceAge=="NVT"] <- "Ikke-vaksine serotyper"
    }
    
    pop <- CleanPop(masterPop,ageDef[[LANGUAGE]])
    rawGroupNumbers <- CleanRawNumbers(masterData, ageDef[[LANGUAGE]], vaxDef)
    rawGroupNumbers <- merge(rawGroupNumbers,pop,by=c("age","year"))
    
    rawSpecificNumbers <- CleanRawNumbers(masterData, ageDef[[LANGUAGE]], vaxDefSpecific)
    rawSpecificNumbers <- merge(rawSpecificNumbers,pop,by=c("age","year"))
    
    correctedGroupNumbers <- CorrectGroupNumbers(rawGroupNumbers)
   
    seasonList <- rev(unique(rawSpecificNumbers$season))
    
    for(seasonOfInterest in seasonList[1:3]){
      CreateFolders(SUPERFOLDER=SUPERFOLDER, FOLDERS=FOLDERS,seasonOfInterest=seasonOfInterest,LANGUAGE=LANGUAGE)
      
      ## funnel plot for serotypes
      if(SUPERFOLDER != "SHAREPOINT"){
        Figure_Funnel_Season(rawSpecificNumbers,ageListFunnel,LANGUAGE,BASE_FOLDER,seasonOfInterest, seasonList, USE_TITLE=USE_TITLE)
      }
      
      ## Figures_cumulative
      Figure_Cumulative(rawGroupNumbers,ageListCumulative,seasonOfInterest,seasonList,LANGUAGE,BASE_FOLDER, USE_TITLE=USE_TITLE)
      
    }
     
    ## TRUNCATING YEARS AS APPROPRIATE
    for(yearOfInterest in c(TODAYS_YEAR:(TODAYS_YEAR-2))){
      CreateFolders(SUPERFOLDER=SUPERFOLDER, FOLDERS=FOLDERS,yearOfInterest=yearOfInterest,LANGUAGE=LANGUAGE)
      ## Serotype specific table
      Table_Serotype(rawSpecificNumbers,ageDef,yearOfInterest,BASE_FOLDER,LANGUAGE)
      
      ## funnel plot for serotypes
      if(SUPERFOLDER != "SHAREPOINT"){
        Figure_Funnel_Year(rawSpecificNumbers,ageListFunnel,LANGUAGE,BASE_FOLDER,yearOfInterest, USE_TITLE=USE_TITLE)
      }
      
      # simpsons index
      if(SUPERFOLDER != "SHAREPOINT"){
        Figure_Simpsons(rawSpecificNumbers,ageListRestrictedAll,ageListSimpsons,DATA_CAPTION,yearOfInterest,BASE_FOLDER,LANGUAGE, USE_TITLE=USE_TITLE)
      }
      
      ## Grouped table
      Table_Number_Incidence(correctedGroupNumbers,ageDef,yearOfInterest,LANGUAGE,BASE_FOLDER)
      
      ## Figures_incidence_vax
      correctedGroupNumbers[,age:=factor(age,levels=names(ageDef[[LANGUAGE]]))]
      
      Figure_Incidence_Vax(correctedGroupNumbers, ageListRestricted,yearOfInterest,LANGUAGE,BASE_FOLDER, USE_TITLE=USE_TITLE)
      
      ## Figure_incidence_age
      Figure_Incidence_Age(correctedGroupNumbers, yearOfInterest, LANGUAGE, BASE_FOLDER, vaxDefIncidenceAge=vaxDefIncidenceAge, USE_TITLE=USE_TITLE)
    }
  }
}

#setwd(RESULTS_BASE)
#zip(sprintf("../surveillance_pneumokokk_%s.zip",TODAYS_DATE), list.files())

# make font slightly smaller

for(i in 1:10) print("FINISHED RUNNING IPD")