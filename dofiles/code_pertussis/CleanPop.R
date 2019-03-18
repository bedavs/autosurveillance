CleanPop <- function(masterPop,ageDef){
  pop <- vector("list",length=length(ageDef))
  for(i in names(ageDef)) pop[[i]] <- copy(masterPop)
  pop <- rbindlist(pop,idcol="age")
  
  temp <- vector("list",length=12)
  for(i in 1:12){
    temp[[i]] <- copy(pop)
    temp[[i]][,xmonth:=(xage)*12+i-1]
    temp[[i]][,pop:=pop/12]
  }
  pop <- rbindlist(temp)
  
  pop[,keep:=FALSE]
  for(i in names(ageDef)) pop[age==i & xmonth %in% ageDef[[i]], keep:=TRUE]
  pop <- pop[keep==TRUE]
  pop[,keep:=NULL]
  
  pop <- pop[,.(pop=sum(pop)),by=.(age,year)]
  return(pop)
}
