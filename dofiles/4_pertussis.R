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
    "G:/Helseregistre/MSIS/MSIS_UtenPersonid/autosurveillance/results/pertussis/",
    "/results/pertussis/"
  ),
  DATA = c(
    "G:/Helseregistre/MSIS/MSIS_UtenPersonid/autosurveillance/data/",
    "/data/"
  ),
  folders_to_be_sourced = c(
    "code_shared",
    "code_pertussis"
  )
)

library(data.table)
library(ggplot2)
library(scales)

masterData <- GetData()[Paar >= 1996]

ageDef <- list(
  "NB"=list(
    "Alle aldersgrupper"=c(YearToMonthsStart(0):YearToMonthsEnd(200)),
    "0-2m"=c(0:2),
    "3-5m"=c(3:5),
    "6-11m"=c(6:11),
    "<1 \u00E5r"=c(0:11),
    "12-23m"=c(12:23),
    "<2 \u00E5r"=c(0:23),
    "2-6 \u00E5r"=c(YearToMonthsStart(2):YearToMonthsEnd(6)),
    "7-14 \u00E5r"=c(YearToMonthsStart(7):YearToMonthsEnd(14)),
    "15-19 \u00E5r"=c(YearToMonthsStart(15):YearToMonthsEnd(19)),
    "20-39 \u00E5r"=c(YearToMonthsStart(20):YearToMonthsEnd(39)),
    "40-64 \u00E5r"=c(YearToMonthsStart(40):YearToMonthsEnd(64)),
    ">=65 \u00E5r"=c(YearToMonthsStart(65):YearToMonthsEnd(200)),
    ">=20 \u00E5r"=c(YearToMonthsStart(20):YearToMonthsEnd(200)),
    "7-19 \u00E5r"=c(YearToMonthsStart(7):YearToMonthsEnd(19))
  ),
  "EN"=list(
    "All ages"=c(YearToMonthsStart(0):YearToMonthsEnd(200)),
    "0-2m"=c(0:2),
    "3-5m"=c(3:5),
    "6-11m"=c(6:11),
    "<1 year"=c(0:11),
    "12-23m"=c(12:23),
    "<2 years"=c(0:23),
    "2-6 years"=c(YearToMonthsStart(2):YearToMonthsEnd(6)),
    "7-14 years"=c(YearToMonthsStart(7):YearToMonthsEnd(14)),
    "15-19 years"=c(YearToMonthsStart(15):YearToMonthsEnd(19)),
    "20-39 years"=c(YearToMonthsStart(20):YearToMonthsEnd(39)),
    "40-64 years"=c(YearToMonthsStart(40):YearToMonthsEnd(64)),
    ">=65 years"=c(YearToMonthsStart(65):YearToMonthsEnd(200)),
    ">=20 years"=c(YearToMonthsStart(20):YearToMonthsEnd(200)),
    "7-19 years"=c(YearToMonthsStart(7):YearToMonthsEnd(19))
  )
)

outcomeAgeDef <- list(
  "NB"=list(
    "0y_200y"="Alle aldersgrupper",
    "0y_0y"="<1 \u00E5r",
    "0m_2m"="0-2m"
  ),
  "EN"=list(
    "0y_200y"="All ages",
    "0y_0y"="<1 year",
    "0m_2m"="0-2m"
  )
)

hospitalDef <- list(
  "NB"=list(
    "all"=unique(masterData$Innlagt),
    "hospital"=c("Ja","Sykehjem, innlagt"),
    "nursinghome"=c("Sykehjem, innlagt")
  ),
  "EN"=list(
    "all"=unique(masterData$Innlagt),
    "hospital"=c("Ja","Sykehjem, innlagt"),
    "nursinghome"=c("Sykehjem, innlagt")
  )
)

deadDef <- list(
  "NB"=list(
    "all"=unique(masterData$Utfall),
    "dead"=c("D\u00F8d av sykdommen")
  ),
  "EN"=list(
    "all"=unique(masterData$Utfall),
    "dead"=c("D\u00F8d av sykdommen")
  )
)

methodDef <- list(
  "NB"=list(
    "all"=unique(masterData$Met),
    "Dyrkning"="Dyrkning",
    "PCR"=c("Nukleinsyrep\u00E5visning (PCR, LCR mv)","Antigenp\u00E5visning"),
    "Serologi"=c("Antistoffp\u00E5visning"),
    "Metode ikke kjent"=c(
      NA,
      "Ukjent",
      "Ingen laboratorieunders\u00F8kelse",
      "Ingen svar fra prim\u00E6rlaboratorium",
      "Negativ laboratorieunders\u00F8kelse",
      "Annen metode"
    )
  ),
  "EN"=list(
    "all"=unique(masterData$Met),
    "Culture"="Dyrkning",
    "PCR"=c("Nukleinsyrep\u00E5visning (PCR, LCR mv)","Antigenp\u00E5visning"),
    "Serology"=c("Antistoffp\u00E5visning"),
    "Unknown method"=c(
      NA,
      "Ukjent",
      "Ingen laboratorieunders\u00F8kelse",
      "Ingen svar fra prim\u00E6rlaboratorium",
      "Negativ laboratorieunders\u00F8kelse",
      "Annen metode"
    )
  )
)

titleDef <- list(
  "NB"=c(
    "N_Alle aldersgrupper_all",
    "N_<1 \u00E5r_all",
    
    "N_innlagt_alle",
    "%_innlagt_alle",
    "N_sykehj_alle",
    "%_sykehj_alle",
    
    "N_innlagt_<1 \u00E5r",
    "%_innlagt_<1 \u00E5r",
    
    "N_d\u00F8de_alle",
    "N_d\u00F8de_<1 \u00E5r"
  ),
  "EN"=c(
    "N_All ages_all",
    "N_<1 year_all",
    
    "N_hosp_all_ages",
    "%_hosp_all_ages",
    "N_nurs_home_all",
    "%_nurs_home_all",
    
    "N_hosp_<1 year",
    "%_hosp_<1 year",
    
    "N_deaths_all",
    "N_deaths_<1 year"
  )
)

#
plotAgesFigure1 <- list(
  "NB"=c("0-2m","3-5m","6-11m","12-23m"),
  "EN"=c("0-2m","3-5m","6-11m","12-23m")
)
titleAgesFigureIncidence1 <- list(
  "NB"="Antall meldte pertussis tilfeller - 0-2 \u00E5ringer",
  "EN"="Number of reported pertussis cases - 0-2 year olds")
titleAgesFigureIncidenceRate1 <-list(
  "NB"="Antall meldte pertussis tilfeller per 100.000 - 0-2 \u00E5ringer",
  "EN"="Number of reported pertussis cases per 100.000 - 0-2 year olds")

#
plotAgesFigure2 <- list(
  "NB"=c("<2 \u00E5r","2-6 \u00E5r","7-14 \u00E5r","15-19 \u00E5r",">=20 \u00E5r"),
  "EN"=c("<2 years","2-6 years","7-14 years","15-19 years",">=20 years")
)
titleAgesFigureIncidence2 <- list(
  "NB"="Antall meldte pertussis tilfeller - alle aldersgrupper",
  "EN"="Number of reported pertussis cases - all age groups")
titleAgesFigureIncidenceRate2 <- list(
  "NB"="Antall meldte pertussis tilfeller per 100.000 - alle aldersgrupper",
  "EN"="Number of reported pertussis cases per 100.000 - all age groups")

#
plotAgesMethods1 <- list(
  "NB"=list(
    "0y_200y"="Alle aldersgrupper"
  ),
  "EN"=list(
    "0y_200y"="All ages"
  )
)
titleAgesMethodsFigure1 <- list(
  "NB"="Rapportert laboratoriemetode - alle aldersgrupper",
  "EN"="Reported laboratory methods - all age groups")

#
plotAgesMethods2 <- list(
  "NB"=list(
    "0y_200y"="<2 \u00E5r"
  ),
  "EN"=list(
    "0y_200y"="<2 years"
  )
)
titleAgesMethodsFigure2 <- list(
  "NB"="Rapportert laboratoriemetode - 0-2 \u00E5ringer",
  "EN"="Reported laboratory methods - 0-2 year olds")

#
plotCumulativeFigure1 <- list(
  "NB"=c("Alle aldersgrupper"),
  "EN"=c("All ages")
)

plotCumulativeFigure2 <- list(
  "NB"=c("<2 \u00E5r"),
  "EN"=c("<2 years")
)

plotCumulativeFigure3 <- list(
  "NB"=c("2-6 \u00E5r"),
  "EN"=c("2-6 years")
)

plotCumulativeFigure4 <- list(
  "NB"=c("7-19 \u00E5r"),
  "EN"=c("7-19 years")
)

stack <- list()
stackSkeleton <- list(
  label=NULL,
  data=NULL,
  pop=NULL,
  DataCleaner=NULL, 
  ResultProducer=NULL,
  arguments=NULL
  )

for(SUPERFOLDER in c("ALL_WITHOUT_TITLES","ALL_WITH_TITLES","SHAREPOINT")){
  print(SUPERFOLDER)
  for(LANGUAGE in c("NB","EN")){
    for(yearOfInterest in 2015:TODAYS_YEAR){
      Sys.sleep(1)
      
      suppressWarnings(CreateFolders(
        language=LANGUAGE, 
        superfolder=SUPERFOLDER, 
        yearOfInterest=yearOfInterest,
        seasonOfInterest=NULL
      ))
      
      stack[[length(stack)+1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "TableIncidenceByAge"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_TableIncidenceByAge
      stack[[length(stack)]]$ResultProducer <- ResultProducer_TableIncidenceByAge
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE"=LANGUAGE,
        "yearOfInterest"=yearOfInterest,
        "ageDef"=ageDef,
        "filename"=file.path(
          org::PROJ$SHARED_TODAY,
          LANGUAGE,
          SUPERFOLDER,
          yearOfInterest,
          "Tables",
          "incidence_by_age.csv"
        )
      )
      
      stack[[length(stack)+1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "OutcomeByAge"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_OutcomeByAge
      stack[[length(stack)]]$ResultProducer <- ResultProducer_OutcomeByAge
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE"=LANGUAGE,
        "yearOfInterest"=yearOfInterest,
        "ageDef"=ageDef,
        "outcomeAgeDef"=outcomeAgeDef,
        "titleDef"=titleDef,
        "filename"=file.path(
          org::PROJ$SHARED_TODAY,
          LANGUAGE,
          SUPERFOLDER,
          yearOfInterest,
          "Tables",
          "outcome_by_age.csv")
      )
      
      stack[[length(stack)+1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "MethodsByAge"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_MethodsByAge
      stack[[length(stack)]]$ResultProducer <- ResultProducer_MethodsByAge
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE"=LANGUAGE,
        "yearOfInterest"=yearOfInterest,
        "ageDef"=ageDef,
        "filename"=file.path(
          org::PROJ$SHARED_TODAY,
          LANGUAGE,
          SUPERFOLDER,
          yearOfInterest,
          "Tables",
          "methods_by_age.csv"
        )
      )
    
      for(i in 1:2){ 
        titles <- Titles(
          LANGUAGE=LANGUAGE,
          SUPERFOLDER=SUPERFOLDER,
          TITLE_MAIN=get(sprintf("titleAgesMethodsFigure%s",i)),
          TITLE_Y=list("NB"="Prosent",
                       "EN"="Percent")
        )
        stack[[length(stack)+1]] <- stackSkeleton
        stack[[length(stack)]]$label <- "FigureMethods"
        stack[[length(stack)]]$data <- "masterData"
        stack[[length(stack)]]$pop <- "pop"
        stack[[length(stack)]]$DataCleaner <- DataCleaner_MethodsByAge
        stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureMethods
        stack[[length(stack)]]$arguments <- list(
          "LANGUAGE"=LANGUAGE,
          "yearOfInterest"=yearOfInterest,
          "ageDef"=ageDef,
          "methodDef"=methodDef,
          "plotAgesMethods"=get(sprintf("plotAgesMethods%s",i)),
          "title"=titles[["TITLE"]],
          "title_y"=titles[["TITLE_Y"]],
          "filename"=file.path(
            org::PROJ$SHARED_TODAY,
            LANGUAGE,
            SUPERFOLDER,
            yearOfInterest,
            "Figures",
            sprintf("%s_Methods_%s.png",LANGUAGE,i))
        )
        
        titles <- Titles(
          LANGUAGE=LANGUAGE,
          SUPERFOLDER=SUPERFOLDER,
          TITLE_MAIN=get(sprintf("titleAgesFigureIncidence%s",i)),
          TITLE_Y=list("NB"="Antall pertussis tilfeller",
                       "EN"="Number of pertussis cases")
        )
        stack[[length(stack)+1]] <- stackSkeleton
        stack[[length(stack)]]$label <- "FigureIncidenceByAge"
        stack[[length(stack)]]$data <- "masterData"
        stack[[length(stack)]]$pop <- "pop"
        stack[[length(stack)]]$DataCleaner <- DataCleaner_FigureIncidenceByAge
        stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureIncidenceByAge
        stack[[length(stack)]]$arguments <- list(
          "plotAges"=get(sprintf("plotAgesFigure%s",i)),
          "LANGUAGE"=LANGUAGE,
          "yearOfInterest"=yearOfInterest,
          "ageDef"=ageDef,
          "title"=titles[["TITLE"]],
          "title_y"=titles[["TITLE_Y"]],
          "filename"=file.path(org::PROJ$SHARED_TODAY,
                               LANGUAGE,
                               SUPERFOLDER,
                               yearOfInterest,
                               "Figures",
                               filename=sprintf("%s_Incidence_Figure_%s.png",LANGUAGE,i))
        )
       
        titles <- Titles(
          LANGUAGE=LANGUAGE,
          SUPERFOLDER=SUPERFOLDER,
          TITLE_MAIN=get(sprintf("titleAgesFigureIncidenceRate%s",i)),
          TITLE_Y=list("NB"="Antall pertussis tilfeller per 100.000",
                       "EN"="Number of pertussis cases per 100,000"),
          substitutions_y=c(" - ",",\n")
        )
        stack[[length(stack)+1]] <- stackSkeleton
        stack[[length(stack)]]$label <- "FigureIncidenceRateByAge"
        stack[[length(stack)]]$data <- "masterData"
        stack[[length(stack)]]$pop <- "pop"
        stack[[length(stack)]]$DataCleaner <- DataCleaner_FigureIncidenceByAge
        stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureIncidenceRateByAge
        stack[[length(stack)]]$arguments <- list(
          "plotAges"=get(sprintf("plotAgesFigure%s",i)),
          "LANGUAGE"=LANGUAGE,
          "yearOfInterest"=yearOfInterest,
          "ageDef"=ageDef,
          "title"=titles[["TITLE"]],
          "title_y"=titles[["TITLE_Y"]],
          "filename"=file.path(
            org::PROJ$SHARED_TODAY,
            LANGUAGE,
            SUPERFOLDER,
            yearOfInterest,
            "Figures",
            filename=sprintf("%s_IncidenceRate_Figure_%s.png",LANGUAGE,i))
        )
      }
    }
    
    for(seasonOfInterest in SeasonList(minSeason=2015)){
      Sys.sleep(1)
      
      suppressWarnings(CreateFolders(
        language=LANGUAGE, 
        superfolder=SUPERFOLDER, 
        yearOfInterest=NULL,
        seasonOfInterest=seasonOfInterest
      ))
      
      for(i in 1:4){
        if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
          USE_TITLE <- FALSE
        } else {
          USE_TITLE <- TRUE
        } 
        
        stack[[length(stack)+1]] <- stackSkeleton
        stack[[length(stack)]]$label <- "Cumulative"
        stack[[length(stack)]]$data <- "masterData"
        stack[[length(stack)]]$pop <- "pop"
        stack[[length(stack)]]$DataCleaner <- DataCleaner_Cumulative
        stack[[length(stack)]]$ResultProducer <- ResultProducer_Cumulative
        stack[[length(stack)]]$arguments <- list(
          "plotAges"=get(sprintf("plotCumulativeFigure%s",i)),
          "LANGUAGE"=LANGUAGE,
          "seasonOfInterest"=seasonOfInterest,
          "ageDef"=ageDef,
          "USE_TITLE"=USE_TITLE,
          "filename"=file.path(
            org::PROJ$SHARED_TODAY,
            LANGUAGE,
            SUPERFOLDER,
            gsub("/","_",seasonOfInterest),
            "Figures",
            sprintf("%s_Cumulative_Figure_%s.png",LANGUAGE,i))
        )
      }
    }
  }
}

for(i in 1:length(stack)) print(sprintf("%s %s", i, stack[[i]]$label))



ProcessStack <- function(stack, i){
  s <- stack[[i]]
  assign("pop",CleanPop(ageDef=ageDef[[s$arguments$LANGUAGE]]),.GlobalEnv)
  cleanedData <- s$DataCleaner(data=get(s$data),arguments=s$arguments)
  s$ResultProducer(data=cleanedData,arguments=s$arguments)
}

for(i in 1:length(stack)){
  print(sprintf("%s/%s",i,length(stack)))
  Sys.sleep(1)
  ProcessStack(stack, i)
}

for(i in 1:10) print("FINISHED RUNNING PERTUSSIS")

