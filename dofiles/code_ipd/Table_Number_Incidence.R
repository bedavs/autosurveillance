Table_Number_Incidence <- function(correctedGroupNumbers,ageDef,yearOfInterest,LANGUAGE,base_folder){
  
  tab1uncorrected <- correctedGroupNumbers[time=="year"]
  tab1uncorrected[,num:=formatC(num,digits=0,format="f", decimal.mark=",")]
  tab1uncorrected <- dcast.data.table(correctedGroupNumbers,age+year~vaccine,value.var="num", fun.aggregate=max)
  tab1uncorrected[,age:=factor(age,levels=names(ageDef[[LANGUAGE]]))]
  setorder(tab1uncorrected,age,year)
  
  tab1corrected <- correctedGroupNumbers[time=="year"]
  tab1corrected[,cnum:=formatC(cnum,digits=0,format="f", decimal.mark=",")]
  tab1corrected <- dcast.data.table(correctedGroupNumbers,age+year~vaccine,value.var="cnum", fun.aggregate=max)
  tab1corrected[,age:=factor(age,levels=names(ageDef[[LANGUAGE]]))]
  setorder(tab1corrected,age,year)
  tab1corrected[,missing:=NULL]
  
  tab1incidence <- correctedGroupNumbers[time=="year"]
  tab1incidence[,incidence:=formatC(incidence,digits=1,format="f", decimal.mark=",")]
  tab1incidence <- dcast.data.table(tab1incidence,age+year~vaccine,value.var="incidence", fun.aggregate=max)
  tab1incidence[,age:=factor(age,levels=names(ageDef[[LANGUAGE]]))]
  setorder(tab1incidence,age,year)
  tab1incidence[,missing:=NULL]
  
  tab1percentage <- correctedGroupNumbers[time=="year"]
  tab1percentage[,percentage:=paste0(formatC(percentage,digits=0,format="f", decimal.mark=","),"%")]
  tab1percentage <- dcast.data.table(tab1percentage,age+year~vaccine,value.var="percentage", fun.aggregate=max)
  tab1percentage[,age:=factor(age,levels=names(ageDef[[LANGUAGE]]))]
  setorder(tab1percentage,age,year)
  tab1percentage[,`All IPD`:=NULL]
  tab1percentage[,missing:=NULL]
  
  mainTable <- list()
  for(a in names(ageDef[[LANGUAGE]])){
    mainTable[[a]] <- cbind(
      tab1uncorrected[age==a & year %in% c(yearOfInterest:2004)],
      " "="    "," "="    "," "="    ",
      tab1corrected[age==a & year %in% c(yearOfInterest:2004)],
      " "="    "," "="    "," "="    ",
      tab1incidence[age==a & year %in% c(yearOfInterest:2004)],
      " "="    "," "="    "," "="    ",
      tab1percentage[age==a & year %in% c(yearOfInterest:2004)])
    
    bufferTop <- mainTable[[a]][1:2]
    bufferBottom <- mainTable[[a]][1:2]
    for(i in 1:ncol(mainTable[[a]])){
      bufferTop[[i]] <- bufferBottom[[i]] <- "  "
      bufferTop[[i]][2] <- names(mainTable[[a]])[i]
    }
    bufferTop[[1]][1] <- "Raw number of cases"
    bufferTop[[ncol(tab1uncorrected)+4]][1] <- "Numbers corrected for missing serotypes"
    bufferTop[[ncol(tab1uncorrected)+ncol(tab1corrected)+7]][1] <- "Corrected Incidence (/100.000)"
    bufferTop[[ncol(tab1uncorrected)+ncol(tab1corrected)+ncol(tab1incidence)+10]][1] <- "% of all disease"
    mainTable[[a]] <- rbind(bufferTop,mainTable[[a]],bufferBottom)
  }
  mainTable <- rbindlist(mainTable)
  mainTable <- mainTable[-nrow(mainTable)]
  
  write.table(mainTable,
              file=file.path(base_folder, "Tables","incidence_by_age_and_vaccine.csv"),
              row.names=F,
              col.names=FALSE,
              sep=";",
              dec=",",
              qmethod="double")
}