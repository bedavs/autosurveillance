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

fileSources = file.path("code_tb",list.files("code_tb",pattern="*.[rR]$"))
sapply(fileSources,source,.GlobalEnv)

FOLDERS <- SetDirectories(type="TB")

data <- GetData(FOLDERS)
data <- CleanData(data)
for(SUPERFOLDER in c("ALL_WITH_TITLES","ALL_WITHOUT_TITLES")){
  print(SUPERFOLDER)
  BASE_FOLDER <- BaseFolder(SUPERFOLDER = SUPERFOLDER, FOLDERS = FOLDERS)
  if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
    USE_TITLE <- FALSE
  } else {
    USE_TITLE <- TRUE
  }
  for(year in c(TODAYS_YEAR:(TODAYS_YEAR-9))){
    print(year)
    for(language in c("EN","NB")){
      unlink(file.path(BASE_FOLDER,year,language),recursive = TRUE, force=TRUE)
      Sys.sleep(1)
      dir.create(file.path(BASE_FOLDER,year,language), recursive=TRUE)
      for(f in c(1:10)){
        Sys.sleep(1)
        txt <- sprintf("res <- Figure%s(data[cyear<=year],language=language,USE_TITLE=%s)",f,USE_TITLE)
        eval(parse(text=txt))
        
        saveA4(res[["q"]],
               filename=file.path(BASE_FOLDER,year,language,sprintf("%s_Figure_%s.png",language,f)))
        
        write.table(res[["tab"]],
                    file=file.path(BASE_FOLDER,year,language,sprintf("%s_Figure_data_%s.csv",language,f)),
                    row.names=F,
                    col.names=T,
                    sep=";",
                    dec=",",
                    qmethod="double")
      }
      for(f in c(1:1)){
        Sys.sleep(1)
        txt <- sprintf("d1 <- Table%s(data[cyear<=year])",f)
        eval(parse(text=txt))
        
        write.table(d1,
                    file=file.path(BASE_FOLDER,year,language,sprintf("%s_Table_%s.csv",language,f)),
                    row.names=F,
                    col.names=F,
                    sep=";",
                    dec=",",
                    qmethod="double")
      }
    }
  }
}

# clean up empty directories
unlink(BaseFolder(SUPERFOLDER = "Sharepoint", FOLDERS = FOLDERS),recursive=TRUE,force=TRUE)

# finished
for(i in 1:10) print("FINISHED RUNNING TB")
