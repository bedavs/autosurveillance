DataCleaner_Basic <- function(data, arguments=NULL){
  # Incidence by age
  temp <- list()
  for(var in names(arguments[["ageDef"]][[arguments[["LANGUAGE"]]]])){
    temp[[var]] <- data[Alm %in% arguments[["ageDef"]][[arguments[["LANGUAGE"]]]][[var]]]
    temp[[var]][,age:=var]
  }
  retval <- rbindlist(temp)
  
  # Incidence by hospital
  temp <- list()
  for(var in names(hospitalDef[[arguments[["LANGUAGE"]]]])){
    temp[[var]] <- retval[Innlagt %in% hospitalDef[[arguments[["LANGUAGE"]]]][[var]]]
    temp[[var]][,hospital:=var]
  }
  retval <- rbindlist(temp)
  
  # Incidence by dead
  temp <- list()
  for(var in names(deadDef[[arguments[["LANGUAGE"]]]])){
    temp[[var]] <- retval[Utfall %in% deadDef[[arguments[["LANGUAGE"]]]][[var]]]
    temp[[var]][,dead:=var]
  }
  retval <- rbindlist(temp)
  
  # Incidence by method
  temp <- list()
  for(var in names(methodDef[[arguments[["LANGUAGE"]]]])){
    temp[[var]] <- retval[Met %in% methodDef[[arguments[["LANGUAGE"]]]][[var]]]
    temp[[var]][,method:=var]
  }
  retval <- rbindlist(temp)
  
  setnames(retval,"Paar","year")
  
  return(retval)
}