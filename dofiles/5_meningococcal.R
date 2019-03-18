library(data.table)
library(ggplot2)

if (.Platform$OS.type == "unix") {
  setwd(file.path("/autosurveillance", "dofiles"))
} else {
  setwd(file.path(
    "G:", "Helseregistre", "MSIS",
    "Sp\u00F8rringer og data UTEN person-id",
    "Auto_surveillance", "dofiles"
  ))
}

fileSources <- file.path("code_shared", list.files("code_shared", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

fileSources <- file.path("code_meningococcal", list.files("code_meningococcal", pattern = "*.[rR]$"))
sapply(fileSources, source, .GlobalEnv)

FOLDERS <- SetDirectories(type = "Meningococcal")

if (.Platform$OS.type == "unix") SavePop(FOLDERS)
masterData <- GetData(FOLDERS)[Paar >= 2007]
masterPop <- readRDS(file.path(FOLDERS$DOFILES_DATA, "meningococcal_pop.RDS"))

ageDef <- list(
  "NB" = list(
    "Alle aldersgrupper" = c(0:200),
    "0-4" = c(0:4),
    "5-15" = c(5:15),
    "16-19" = c(16:19),
    "20-39" = c(20:39),
    "40-59" = c(40:59),
    "60+" = c(60:200)
  ),
  "EN" = list(
    "All ages" = c(0:200),
    "0-4" = c(0:4),
    "5-15" = c(5:15),
    "16-19" = c(16:19),
    "20-39" = c(20:39),
    "40-59" = c(40:59),
    "60+" = c(60:200)
  )
)

serogroupDef <- list(
  "NB" = list(
    "Totalt" = unique(masterData$Smstoff)
  ),
  "EN" = list(
    "Total" = unique(masterData$Smstoff)
  )
)
for (i in unique(masterData$Smstoff)) {
  serogroupDef[["NB"]][[i]] <- i
  serogroupDef[["EN"]][[i]] <- i
}

tableAgesTableAgeAndSerotype <- list(
  "NB" = c("Alle aldersgrupper", "16-19"),
  "EN" = c("All ages", "16-19")
)

plotAgesFigure1 <- list(
  "NB" = c("Alle aldersgrupper"),
  "EN" = c("All ages")
)

plotAgesFigure2 <- list(
  "NB" = c("16-19"),
  "EN" = c("16-19")
)

plotAgesSerotypeByYear1 <- list(
  "NB" = c("Alle aldersgrupper"),
  "EN" = c("All ages")
)

plotAgesSerotypeByYear2 <- list(
  "NB" = c("16-19"),
  "EN" = c("16-19")
)


stack <- list()
stackSkeleton <- list(
  label = NULL,
  data = NULL,
  pop = NULL,
  DataCleaner = NULL,
  ResultProducer = NULL,
  arguments = NULL
)

for(SUPERFOLDER in c("SHAREPOINT","ALL_WITH_TITLES","ALL_WITHOUT_TITLES")){
  print(SUPERFOLDER)
  BASE_FOLDER <- BaseFolder(SUPERFOLDER = SUPERFOLDER, FOLDERS = FOLDERS)
  for (LANGUAGE in c("NB", "EN")) {
    for (yearOfInterest in 2015:TODAYS_YEAR) {
      Sys.sleep(1)
      suppressWarnings(CreateFolders(SUPERFOLDER=SUPERFOLDER, FOLDERS = FOLDERS, yearOfInterest = yearOfInterest, LANGUAGE = LANGUAGE))
  
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "TableIncidenceByAgeAndSerotype"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_TableIncidenceByAgeAndSerotype
      stack[[length(stack)]]$ResultProducer <- ResultProducer_TableIncidenceByAgeAndSerotype
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "tableAgesTableAgeAndSerotype" = tableAgesTableAgeAndSerotype,
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Tables",
                             "incidence_by_age_and_serotype.csv")
      )
  
      TEMP_TITLE_Y <- list("NB"="Antall meningokokk tilfeller per 100.000",
                           "EN"="Number of meningococcal cases per 100,000")[[LANGUAGE]]
      TEMP_TITLE_MAIN <- list(
        "NB"="Insidens fordelt etter serogrupper - alle aldersgrupper",
        "EN"="Incidence by serogroup - all age groups")[[LANGUAGE]]
      if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
        TITLE <- NULL
        TITLE_Y <- TEMP_TITLE_MAIN
      } else {
        TITLE <- TEMP_TITLE_MAIN
        TITLE_Y <- TEMP_TITLE_Y
      }
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "FigureIncidenceByAgeAndSerotype_allages"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_FigureIncidenceByAgeAndSerotype
      stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureIncidenceByAgeAndSerotype
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "tableAgesTableAgeAndSerotype" = plotAgesFigure1,
        "onlyShowTotal"=FALSE,
        "title"=TITLE,
        "title_y"=TITLE_Y,
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Figures",
                             sprintf("%s_incidence_by_age_serotype_all_ages.png",LANGUAGE))
      )
      
      TEMP_TITLE_Y <- list("NB"="Antall meningokokk tilfeller per 100.000",
                           "EN"="Number of meningococcal cases per 100,000")[[LANGUAGE]]
      TEMP_TITLE_MAIN <- list(
        "NB"="Insidens fordelt etter serogrupper - 16-19 \u00E5ringer",
        "EN"="Incidence by serogroup - 16-19 year olds")[[LANGUAGE]]
      if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
        TITLE <- NULL
        TITLE_Y <- TEMP_TITLE_MAIN
      } else {
        TITLE <- TEMP_TITLE_MAIN
        TITLE_Y <- TEMP_TITLE_Y
      } 
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "FigureIncidenceByAgeAndSerotype_16-19"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_FigureIncidenceByAgeAndSerotype
      stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureIncidenceByAgeAndSerotype
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "tableAgesTableAgeAndSerotype" = plotAgesFigure2,
        "onlyShowTotal"=FALSE,
        "title"=TITLE,
        "title_y"=TITLE_Y,
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Figures",
                             sprintf("%s_incidence_by_age_serotype_16-19.png",LANGUAGE))
      )
      
      ##
      
      TEMP_TITLE_Y <- list("NB"="Antall meningokokk tilfeller per 100.000",
                           "EN"="Number of meningococcal cases per 100,000")[[LANGUAGE]]
      TEMP_TITLE_MAIN <- list(
        "NB"="Insidens fordelt etter serogrupper - alle aldersgrupper",
        "EN"="Incidence by serogroup - all age groups")[[LANGUAGE]]
      if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
        TITLE <- NULL
        TITLE_Y <- TEMP_TITLE_MAIN
      } else {
        TITLE <- TEMP_TITLE_MAIN
        TITLE_Y <- TEMP_TITLE_Y
      }
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "FigureIncidenceByAgeAndSerotype_allages"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_FigureIncidenceByAgeAndSerotype
      stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureIncidenceByAgeAndSerotype
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "tableAgesTableAgeAndSerotype" = plotAgesFigure1,
        "onlyShowTotal"=TRUE,
        "title"=TITLE,
        "title_y"=TITLE_Y,
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Figures",
                             sprintf("%s_incidence_by_age_all_ages.png",LANGUAGE))
      )
      
      TEMP_TITLE_Y <- list("NB"="Antall meningokokk tilfeller per 100.000",
                           "EN"="Number of meningococcal cases per 100,000")[[LANGUAGE]]
      TEMP_TITLE_MAIN <- list(
        "NB"="Insidens fordelt etter serogrupper - 16-19 \u00E5ringer",
        "EN"="Incidence by serogroup - 16-19 year olds")[[LANGUAGE]]
      if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
        TITLE <- NULL
        TITLE_Y <- TEMP_TITLE_MAIN
      } else {
        TITLE <- TEMP_TITLE_MAIN
        TITLE_Y <- TEMP_TITLE_Y
      } 
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "FigureIncidenceByAgeAndSerotype_16-19"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_FigureIncidenceByAgeAndSerotype
      stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureIncidenceByAgeAndSerotype
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "tableAgesTableAgeAndSerotype" = plotAgesFigure2,
        "onlyShowTotal"=TRUE,
        "title"=TITLE,
        "title_y"=TITLE_Y,
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Figures",
                             sprintf("%s_incidence_by_age_16-19.png",LANGUAGE))
      )
      
      ##
     
      TEMP_TITLE_Y <- list("NB"="Prosent",
                           "EN"="Percent")[[LANGUAGE]]
      TEMP_TITLE_MAIN <- list(
        "NB"="Serogruppefordeling per \u00E5r - alle aldersgrupper",
        "EN"="Serogroup distribution by year - all age groups")[[LANGUAGE]]
      if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
        TITLE <- NULL
        TITLE_Y <- TEMP_TITLE_MAIN
      } else {
        TITLE <- TEMP_TITLE_MAIN
        TITLE_Y <- TEMP_TITLE_Y
      }
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "FigureSerotypeByYear_allages"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_FigureSerotypeByYear
      stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureSerotypeByYear
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "plotAgesSerotypeByYear" = plotAgesSerotypeByYear1,
        "title"=TITLE,
        "title_y"=TITLE_Y,
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Figures",
                             sprintf("%s_serotype_by_year_allages.png",LANGUAGE))
      )
     
      TEMP_TITLE_Y <- list("NB"="Prosent",
                           "EN"="Percent")[[LANGUAGE]]
      TEMP_TITLE_MAIN <- list(
        "NB"="Serogruppefordeling per \u00E5r - 16-19 \u00E5ringer",
        "EN"="Serogroup distribution by year - 16-19 year olds")[[LANGUAGE]]
      if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
        TITLE <- NULL
        TITLE_Y <- TEMP_TITLE_MAIN
      } else {
        TITLE <- TEMP_TITLE_MAIN
        TITLE_Y <- TEMP_TITLE_Y
      }
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "FigureSerotypeByYear_16-19"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_FigureSerotypeByYear
      stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureSerotypeByYear
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "plotAgesSerotypeByYear" = plotAgesSerotypeByYear2,
        "title"=TITLE,
        "title_y"=TITLE_Y,
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Figures",
                             sprintf("%s_serotype_by_year_16-19.png",LANGUAGE))
      )
  
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "TableIncidenceByAge"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_TableIncidenceByAge
      stack[[length(stack)]]$ResultProducer <- ResultProducer_TableIncidenceByAge
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Tables",
                             "incidence_by_age.csv")
      )
      
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "TableIncidenceByMonth"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_TableIncidenceByMonth
      stack[[length(stack)]]$ResultProducer <- ResultProducer_TableIncidenceByMonth
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Tables",
                             "incidence_by_month.csv")
      )
    
      titles <- Titles(
        LANGUAGE=LANGUAGE,
        SUPERFOLDER=SUPERFOLDER,
        TITLE_MAIN=list("NB"="Gjennomsnittlig antall meningokokktilfeller per m\u00E5ned",
                        "EN"="Average monthly number of meningococcal cases"),
        TITLE_Y=list(
          "NB"="Insidens per m\u00E5ned - alle aldersgrupper",
          "EN"="Incidence per month - all age groups")
        )
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "ResultProducer_FigureIncidenceByMonth1Year"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_TableIncidenceByMonth
      stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureIncidenceByMonth1Year
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "title"=titles[["TITLE"]],
        "title_y"=titles[["TITLE_Y"]],
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Figures",
                             sprintf("%s_incidence_by_month_1year.png",LANGUAGE))
      )
      
      TEMP_TITLE_Y <- list("NB"="Gjennomsnittlig antall meningokokktilfeller per m\u00E5ned",
                           "EN"="Average monthly number of meningococcal cases")[[LANGUAGE]]
      TEMP_TITLE_MAIN <- list(
        "NB"="Insidens per m\u00E5ned",
        "EN"="Incidence per month")[[LANGUAGE]]
      if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
        TITLE <- NULL
        TITLE_Y <- TEMP_TITLE_MAIN
      } else {
        TITLE <- TEMP_TITLE_MAIN
        TITLE_Y <- TEMP_TITLE_Y
      }
      stack[[length(stack) + 1]] <- stackSkeleton
      stack[[length(stack)]]$label <- "ResultProducer_FigureIncidenceByMonth1YearVs3"
      stack[[length(stack)]]$data <- "masterData"
      stack[[length(stack)]]$pop <- "pop"
      stack[[length(stack)]]$DataCleaner <- DataCleaner_TableIncidenceByMonth
      stack[[length(stack)]]$ResultProducer <- ResultProducer_FigureIncidenceByMonth1YearVs3
      stack[[length(stack)]]$arguments <- list(
        "LANGUAGE" = LANGUAGE,
        "yearOfInterest" = yearOfInterest,
        "ageDef" = ageDef,
        "title"=TITLE,
        "title_y"=TITLE_Y,
        "filename"=file.path(BASE_FOLDER,
                             yearOfInterest,
                             LANGUAGE,
                             "Figures",
                             sprintf("%s_incidence_by_month_1year_vs_3.png",LANGUAGE))
      )
    }
  }
}

for (i in 1:length(stack)) print(sprintf("%s %s", i, stack[[i]]$label))



ProcessStack <- function(stack, i) {
  s <- stack[[i]]
  assign("pop", CleanPop(masterPop, ageDef = ageDef[[s$arguments$LANGUAGE]]), .GlobalEnv)
  cleanedData <- s$DataCleaner(data = get(s$data), arguments = s$arguments)
  s$ResultProducer(data = cleanedData, arguments = s$arguments)
}

for (i in 1:length(stack)) {
  print(sprintf("%s/%s", i, length(stack)))
  Sys.sleep(1)
  ProcessStack(stack, i)
}

for (i in 1:10) print("FINISHED RUNNING Meningococcal")