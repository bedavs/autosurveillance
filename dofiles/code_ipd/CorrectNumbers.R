CorrectGroupNumbers <- function(rawGroupNumbers){
  correctedGroupNumbers <- rawGroupNumbers[time=="year"]
  
  correctedGroupNumbers[,y_missing:=as.numeric(NA)]
  correctedGroupNumbers[vaccine=="missing",y_missing:=as.numeric(num)]
  correctedGroupNumbers[,y_missing:=sum(y_missing,na.rm=T),by=.(age,year)]
  
  correctedGroupNumbers[!vaccine %in% c("All IPD","missing"),cnum:=num+(num/num_all)*y_missing]
  correctedGroupNumbers[vaccine %in% c("All IPD"),cnum:=as.numeric(num)]
  correctedGroupNumbers[is.na(cnum),cnum:=0]
  correctedGroupNumbers[,incidence:=100000*cnum/pop]
  correctedGroupNumbers[,cnum:=round(cnum)]
  correctedGroupNumbers[!vaccine %in% c("All IPD","missing"),percentage:=cnum/num_all*100]
  
  correctedGroupNumbers[,y_missing:=NULL]
  correctedGroupNumbers[,num_all:=NULL]
  
  correctedGroupNumbers[,vaccine:=factor(vaccine,levels=names(vaxDef))]
  
  return(correctedGroupNumbers)
}
