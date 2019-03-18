DataCleaner_OutcomeByAge <- function(data, arguments=NULL){
  dead <- data[isDead==1,c("Alaar","Paar")]
  data <- DataCleaner_Basic(data, arguments=arguments)
  
  retval <- list("dead"=dead,"data"=data)
  return(retval)
}

ResultProducer_OutcomeByAge <- function(data, arguments=NULL){
  dead <- data[["dead"]]
  data <- data[["data"]]
  
  data[,title:=as.character(NA)]
  
  data[dead=="all" & hospital=="all" & method=="all" & age==arguments$outcomeAgeDef[[arguments$LANGUAGE]]["0y_200y"],
    title:=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][1]]
  data[dead=="all" & hospital=="all" & method=="all" & age==arguments$outcomeAgeDef[[arguments$LANGUAGE]]["0y_0y"],
    title:=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][2]]
  
  data[dead=="all" & hospital=="hospital" & method=="all" & age==arguments$outcomeAgeDef[[arguments$LANGUAGE]]["0y_200y"],
    title:=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][3]]
  data[dead=="all" & hospital=="hospital" & method=="all" & age==arguments$outcomeAgeDef[[arguments$LANGUAGE]]["0m_2m"],
    title:=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][4]] # junk, just need to get a variable in there to replace with percentage
  
  data[dead=="all" & hospital=="nursinghome" & method=="all" & age==arguments$outcomeAgeDef[[arguments$LANGUAGE]]["0y_200y"],
    title:=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][5]]
  data[dead=="all" & hospital=="nursinghome" & method=="all" & age==arguments$outcomeAgeDef[[arguments$LANGUAGE]]["0m_2m"],
    title:=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][6]] # junk, just need to get a variable in there to replace with percentage
  
  data[dead=="all" & hospital=="hospital" & method=="all" & age==arguments$outcomeAgeDef[[arguments$LANGUAGE]]["0y_0y"],
    title:=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][7]]
  data[dead=="all" & hospital=="hospital" & method=="all" & age==arguments$outcomeAgeDef[[arguments$LANGUAGE]]["0m_2m"],
    title:=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][8]] # junk, just need to get a variable in there to replace with percentage
  
  data[dead=="dead" & hospital=="all" & method=="all" & age==arguments$outcomeAgeDef[[arguments$LANGUAGE]]["0y_200y"],
    title:=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][9]]
  data[dead=="dead" & hospital=="all" & method=="all" & age==arguments$outcomeAgeDef[[arguments$LANGUAGE]]["0y_0y"],
    title:=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][10]]
  
  data <- data[!is.na(title),.(N=.N),by=.(title,year)]
  data[,title:=factor(title,levels=arguments[["titleDef"]][[arguments[["LANGUAGE"]]]])]
  data <- dcast.data.table(data,year ~ title, value.var="N",fill=0, drop=FALSE)
  
  txt <- sprintf("data[,`%s`:=round(100*`%s`/`%s`)]",
                 arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][4],
                 arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][3],
                 arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][1])
  eval(parse(text=txt))
  
  txt <- sprintf("data[,`%s`:=round(100*`%s`/`%s`)]",
                 arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][6],
                 arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][5],
                 arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][1])
  eval(parse(text=txt))
  
  txt <- sprintf("data[,`%s`:=round(100*`%s`/`%s`)]",
                 arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][8],
                 arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][7],
                 arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][2])
  eval(parse(text=txt))
  
  data[,(arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][1]):=NULL]
  data[,(arguments[["titleDef"]][[arguments[["LANGUAGE"]]]][2]):=NULL]
  
  data <- AddTitleToDF(data,"Outcomes", copyColumnNames = T)
  
  dead <- dead[, list(lapply(.SD, c)), by=Paar]
  dead[,ages:=paste0(unlist(V1),collapse=","),by=Paar]
  dead[,V1:=NULL]
  skeleton <- data.table(expand.grid(Paar=1996:yearOfInterest))
  dead <- merge(skeleton,dead,by="Paar",all.x=T)
  dead[is.na(ages),ages:="-"]
  setnames(dead,"ages","[age of death]")
  
  dead <- AddTitleToDF(dead,"Deaths", copyColumnNames = T)
  dead <- AddTitleToDF(dead," ")
  
  tab <- rbindlist(list(data,cbind(dead,"","","","","","","")), use.names = F)
  
  write.table(tab,
              file=arguments[["filename"]],
              row.names=F,
              col.names=F,
              sep=";",
              dec=",",
              qmethod="double")
}
