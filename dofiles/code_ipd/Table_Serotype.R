Table_Serotype <- function(rawSpecificNumbers,ageDef,yearOfInterest,base_folder,LANGUAGE){
  tab <- rawSpecificNumbers[time=="year" & year>=2004 & year<=yearOfInterest]
  tab[,num:=formatC(num,digits=0,format="f", decimal.mark=",")]
  tab <- dcast.data.table(tab, age + vaccine ~ year, value.var="num")
  
  mainTable <- list()
  for(a in rev(names(ageDef[[LANGUAGE]]))){
    mainTable[[a]] <- tab[age==a]
    
    ordering <- rawSpecificNumbers[time=="year" & age==a & year==yearOfInterest & !vaccine %in% c("All IPD","missing")]
    setorder(ordering,-num)
    mainTable[[a]][,vaccine:=factor(vaccine,levels=c(ordering$vaccine,"missing","All IPD"))]
    setorder(mainTable[[a]],-age,vaccine)
    
    bufferTop <- mainTable[[a]][1:2]
    bufferBottom <- mainTable[[a]][1:2]
    for(i in 1:ncol(mainTable[[a]])){
      bufferTop[[i]] <- bufferBottom[[i]] <- "  "
      bufferTop[[i]][2] <- names(mainTable[[a]])[i]
    }
    
    mainTable[[a]] <- rbind(bufferTop,mainTable[[a]],bufferBottom)
  }
  mainTable <- rbindlist(mainTable)
  mainTable <- mainTable[-nrow(mainTable)]
  
  write.table(mainTable,
              file=file.path(base_folder,"Tables","serotype_specific_frequency.csv"),
              row.names=F,
              col.names=FALSE,
              sep=";",
              dec=",",
              qmethod="double")
}