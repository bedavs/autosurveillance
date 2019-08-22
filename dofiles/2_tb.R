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
    "G:/Helseregistre/MSIS/MSIS_UtenPersonid/autosurveillance/results/tb/",
    "/results/tb/"
  ),
  DATA = c(
    "G:/Helseregistre/MSIS/MSIS_UtenPersonid/autosurveillance/data/",
    "/data/"
  ),
  folders_to_be_sourced = c(
    "code_shared",
    "code_tb"
  )
)

library(data.table)
library(ggplot2)
library(scales)

data <- GetData()
data <- CleanData(data)

analyses <- expand.grid(
  superfolder = c("ALL_WITHOUT_TITLES","ALL_WITH_TITLES"),
  language = c("NB","EN"),
  year=c(TODAYS_YEAR:(TODAYS_YEAR-9)),
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
  create_folders(
    language = a$language,
    superfolder = a$superfolder,
    year = a$year
  )
  
  base_folder <- file.path(
    org::PROJ$SHARED_TODAY,
    a$language,
    a$superfolder,
    a$year
  )
  
  for(f in c(1:10)){
    Sys.sleep(1)
    txt <- sprintf("res <- Figure%s(data=data[cyear<=a$year],language=a$language,USE_TITLE=%s)",f,a$use_title)
    eval(parse(text=txt))
    
    saveA4(res[["q"]],
           filename=file.path(base_folder,sprintf("%s_Figure_%s.png",a$language,f)))
    
    write.table(res[["tab"]],
                file=file.path(base_folder,sprintf("%s_Figure_data_%s.csv",a$language,f)),
                row.names=F,
                col.names=T,
                sep=";",
                dec=",",
                qmethod="double")
  }
  for(f in c(1:1)){
    Sys.sleep(1)
    txt <- sprintf("d1 <- Table%s(data[cyear<=a$year])",f)
    eval(parse(text=txt))
    
    write.table(d1,
                file=file.path(base_folder,sprintf("%s_Table_%s.csv",a$language,f)),
                row.names=F,
                col.names=F,
                sep=";",
                dec=",",
                qmethod="double")
  }
}

# finished
for(i in 1:10) print("FINISHED RUNNING TB")
