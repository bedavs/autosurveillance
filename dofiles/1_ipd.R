if(.Platform$OS.type!="unix"){
  files <- list.files("//red.fhi.sec/app/R/3.5/")
  for(p in c("^fhidata_", "^org_")){
    p1 <- max(files[grep(p, files)])
    install.packages(paste0("//red.fhi.sec/app/R/3.5/", p1), repos=NULL)
  }
}

org::AllowFileManipulationFromInitialiseProject()
org::InitialiseProject(
  HOME = c(
    "G:/Helseregistre/MSIS/MSIS_UtenPersonid/autosurveillance/dofiles/",
    "/autosurveillance/dofiles/"
  ),
  SHARED = c(
    "G:/Helseregistre/MSIS/MSIS_UtenPersonid/autosurveillance/results/ipd/",
    "/results/ipd/"
  ),
  DATA = c(
    "G:/Helseregistre/MSIS/MSIS_UtenPersonid/autosurveillance/data/",
    "/data/"
  ),
  folders_to_be_sourced = c(
    "code_shared",
    "code_ipd"
  )
)

library(data.table)
library(ggplot2)

masterData <- data_get()
masterPop <- pop_get()

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

analyses <- expand.grid(
  superfolder = c("ALL_WITHOUT_TITLES","ALL_WITH_TITLES","SHAREPOINT"),
  language = c("NB","EN"),
  stringsAsFactors = FALSE
)
setDT(analyses)
analyses[, use_title := FALSE]
analyses[superfolder %in% c("ALL_WITHOUT_TITLES"), use_title:=TRUE]

for(i in 1:nrow(analyses)){
  a <- analyses[i,]
  cat("********\n\n",i,"/",nrow(analyses),"\n\n")
  print(a)
  
  # START RUNNING CODE
  vaxDef <- vaxDefMaster
  vaxDefIncidenceAge <- vaxDefIncidenceAgeMaster
  if(a$language=="NB"){
    names(vaxDef)[names(vaxDef)=="NVT"] <- "Ikke-vaksine serotyper"
    vaxDefIncidenceAge[vaxDefIncidenceAge=="NVT"] <- "Ikke-vaksine serotyper"
  }
    
  pop <- CleanPop(masterPop,ageDef[[a$language]])
  rawGroupNumbers <- CleanRawNumbers(masterData, ageDef[[a$language]], vaxDef)
  rawGroupNumbers <- merge(rawGroupNumbers,pop,by=c("age","year"))
    
  rawSpecificNumbers <- CleanRawNumbers(masterData, ageDef[[a$language]], vaxDefSpecific)
  rawSpecificNumbers <- merge(rawSpecificNumbers,pop,by=c("age","year"))
    
  correctedGroupNumbers <- CorrectGroupNumbers(rawGroupNumbers)
   
  seasonList <- rev(unique(rawSpecificNumbers[time=="week"]$season))
    
  for(seasonOfInterest in seasonList[1:3]){
    print(seasonOfInterest)
    
    CreateFolders(
      language = a$language,
      superfolder = a$superfolder,
      seasonOfInterest=seasonOfInterest
    )
    
    folder <- path(
      type = "season", 
      language = a$language, 
      superfolder = a$superfolder,
      time = seasonOfInterest
    )
      
    ## funnel plot for serotypes
    if(a$superfolder != "SHAREPOINT"){
      Figure_Funnel_Season(
        rawSpecificNumbers,
        ageListFunnel,
        LANGUAGE=a$language,
        folder = folder,
        seasonOfInterest,
        seasonList,
        USE_TITLE=a$use_title
      )
    }
      
    ## Figures_cumulative
    Figure_Cumulative(
      rawGroupNumbers,
      ageListCumulative,
      seasonOfInterest,
      seasonList,
      LANGUAGE = a$language,
      folder = folder, 
      USE_TITLE=a$use_title
    )
      
  }
     
  ## TRUNCATING YEARS AS APPROPRIATE
  for(yearOfInterest in c(TODAYS_YEAR:(TODAYS_YEAR-2))){
    print(yearOfInterest)
    
    CreateFolders(
      language = a$language,
      superfolder = a$superfolder,
      yearOfInterest=yearOfInterest
    )
    
    base_folder <- path(
      type = "year", 
      language = a$language, 
      superfolder = a$superfolder,
      time = yearOfInterest
    )
    
    ## Serotype specific table
    Table_Serotype(
      rawSpecificNumbers,
      ageDef,
      yearOfInterest,
      base_folder = base_folder,
      LANGUAGE = a$language
    )
    
    ## funnel plot for serotypes
    if(a$superfolder != "SHAREPOINT"){
      Figure_Funnel_Year(
        rawSpecificNumbers,
        ageListFunnel,
        LANGUAGE = a$language,
        base_folder = base_folder,
        yearOfInterest,
        USE_TITLE=a$use_title
      )
    
    # simpsons index
      Figure_Simpsons(
        rawSpecificNumbers,
        ageListRestrictedAll,
        ageListSimpsons,
        DATA_CAPTION,
        yearOfInterest,
        base_folder = base_folder,
        LANGUAGE = a$language,
        USE_TITLE=a$use_title
      )
    }
    
    ## Grouped table
    Table_Number_Incidence(
      correctedGroupNumbers,
      ageDef,
      yearOfInterest,
      LANGUAGE = a$language,
      base_folder = base_folder
    )
    
    ## Figures_incidence_vax
    correctedGroupNumbers[,age:=factor(age,levels=names(ageDef[[a$language]]))]
    
    Figure_Incidence_Vax(
      correctedGroupNumbers,
      ageListRestricted,
      yearOfInterest,
      LANGUAGE = a$language,
      base_folder = base_folder,
      USE_TITLE=a$use_title
    )
    
    ## Figure_incidence_age
    Figure_Incidence_Age(
      correctedGroupNumbers,
      yearOfInterest,
      LANGUAGE = a$language,
      base_folder = base_folder,
      vaxDefIncidenceAge=vaxDefIncidenceAge,
      USE_TITLE=a$use_title
    )
  }
}

#setwd(RESULTS_BASE)
#zip(sprintf("../surveillance_pneumokokk_%s.zip",TODAYS_DATE), list.files())

# make font slightly smaller

for(i in 1:10) print("FINISHED RUNNING IPD")