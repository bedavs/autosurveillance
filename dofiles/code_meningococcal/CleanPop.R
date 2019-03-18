CleanPop <- function(masterPop,ageDef){
  pop <- vector("list",length=length(ageDef))
  for(i in names(ageDef)) pop[[i]] <- copy(masterPop)
  pop <- rbindlist(pop,idcol="age")
  
  pop[,keep:=FALSE]
  for(i in names(ageDef)) pop[age==i & xage %in% ageDef[[i]], keep:=TRUE]
  pop <- pop[keep==TRUE]
  pop[,keep:=NULL]
  
  pop <- pop[,.(pop=sum(pop)),by=.(age,year)]
  return(pop)
}
