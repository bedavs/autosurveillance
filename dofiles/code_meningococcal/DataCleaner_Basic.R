DataCleaner_Basic <- function(data, arguments=NULL){
  # Incidence by age
  temp <- list()
  for(var in names(arguments[["ageDef"]][[arguments[["LANGUAGE"]]]])){
    temp[[var]] <- data[Alaar %in% arguments[["ageDef"]][[arguments[["LANGUAGE"]]]][[var]]]
    temp[[var]][,age:=var]
  }
  retval <- rbindlist(temp)
  
  # Incidence by hospital
  temp <- list()
  for(var in names(serogroupDef[[arguments[["LANGUAGE"]]]])){
    temp[[var]] <- retval[Smstoff %in% serogroupDef[[arguments[["LANGUAGE"]]]][[var]]]
    temp[[var]][,serogroup:=var]
  }
  retval <- rbindlist(temp)
  
  setnames(retval,"Paar","year")
  setnames(retval,"Pmnd","month")
  
  return(retval)
}