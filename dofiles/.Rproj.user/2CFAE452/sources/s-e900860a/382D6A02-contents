AddTitle <- function(d,title){
  d <- as.data.frame(d)
  for(i in 1:ncol(d)) d[,i] <- as.character(d[,i])
  dtop <- d[1,,drop=F]
  dtop <- rbind(dtop,dtop)
  for(i in 1:ncol(dtop)){
    dtop[1:2,i] <- ""
    dtop[2,i] <- names(d)[i]
  }
  dtop[1,1] <- title
  return(rbind(dtop,d))
}


Table1 <- function(data){
  dataCurrent <- data[cyear==max(cyear)]
  dataLast <- data[cyear==max(cyear)-1]
  
  retval <- list()
  for(i in c("current","last")){
    if(i=="current"){
      temp1 <- copy(dataCurrent)
      temp2 <- copy(dataCurrent)
    } else if(i=="last") {
      temp1 <- copy(dataLast)
      temp2 <- copy(dataLast)
    }
    
    temp2[,cFverdNB:="Totalt"]
    temp <- rbind(temp1,temp2)
    
    temp <- temp[isActive==1,.(
      numCases=.N,
      numIsolates=sum(cDyrkResAllOrganNB=="Positivt",na.rm=T),
      numIsoniazid_H=sum(isHRes,na.rm=T),
      numRifampicin_R=sum(isRRes,na.rm=T),
      numEthanbutol_E=sum(isERes,na.rm=T),
      numPyrazinamid_P=sum(isPRes,na.rm=T),
      numMDR_TB_HR=sum(isMDR,na.rm=T)
    ),by=.(cFverdNB)]
    
    skeleton <- data.table(cFverdNB=c(
      "Norge",
      "Europa utenfor Norge",
      "Asia",
      "Afrika",
      "Amerika",
      "Oseania",
      "Ukjent",
      "Totalt",
      "Proportion resistant isolates (%)"
      ))
    retval[[i]] <- merge(skeleton,temp,all=T,by=c("cFverdNB"))
    for(j in names(retval[[i]])){
      retval[[i]][is.na(get(j)),(j):=0]
    }
    
    columnsToFormat <- copy(names(retval[[i]]))
    for(j in columnsToFormat){
      j_new <- sprintf("x%s",j)
      if(j %in% c("cFverdNB","numCases","numIsolates")) next
      numerator <- retval[[i]][cFverdNB=="Totalt",j,with=F]
      denominator <- retval[[i]][cFverdNB=="Totalt","numIsolates"]
      retval[[i]][,(j_new):=as.character(get(j))]
      retval[[i]][cFverdNB=="Proportion resistant isolates (%)",(j_new):=formatC(as.numeric(numerator)/as.numeric(denominator)*100,digits=1,format="f")]
      retval[[i]][,(j):=NULL]
    }
  }
  
  tab <- copy(retval[["current"]])
  for(j in names(retval[["current"]])){
    if(j=="cFverdNB") next
    tab[,(j):=sprintf("%s (%s)",get(j),retval[["last"]][[j]])]
  }
  tab[,cFverdNB:=factor(cFverdNB,levels=c(
    "Norge",
    "Europa utenfor Norge",
    "Asia",
    "Afrika",
    "Amerika",
    "Oseania",
    "Ukjent",
    "Totalt",
    "Proportion resistant isolates (%)"
  ))]
  setorder(tab,cFverdNB)
  
  tab <- AddTitleToDF(tab,title=sprintf("Antimicrobial susceiptibility of Mycobacterium tuberculosis complex human infections in %s. Figures from %s are shown in paraentheses.",max(data$cyear),max(data$cyear)-1),
               copyColumnNames = TRUE)
  return(tab)
}

Table_x1 <- function(data){
  res <- data[!cPernorNB %in% c(
    "Ubesvart","Ukjent"
  ),.(isTB=sum(isActive)),by=.(cPernorNB,cyear)]
  resx <- res[,.(isTB=sum(isTB)),by=cyear]
  resx[,cPernorNB:="Totalt utenlandsfødte m kjent oppholdstid"]
  setcolorder(resx,names(res))
  res <- rbind(res,resx)
  res[,cPernorNB:=factor(cPernorNB,levels=c(
    "Under ett år i Norge",
    "Ett til fire år i Norge",
    "5 år eller mer i Norge",
    "Totalt utenlandsfødte m kjent oppholdstid"
  ))]
  setorder(res,-cyear,cPernorNB)
  setcolorder(res,c("cyear","cPernorNB","isTB"))
  res[,perc:=sum(isTB)/2,by=cyear]
  res[,perc:=paste0(round(isTB/perc*100),"%")]
  res <- AddTitle(res,title="Oppholdstid i Norge før diagnose (der angitt), meldt MSIS")
  setattr(res,"filename","oppholdstid_i_norge.xlsx")
  return(res)
}

Table_x2 <- function(data){
  d <- copy(data)
  d[,newIndik:=as.character(NA)]
  d[cIndikNB %in% c(
    "Arbeid med pasienter eller barn"
  ), newIndik:="Arbeid"]
  
  d[cIndikNB %in% c(
    "Immunsvekkende tilstand/behandling"
  ), newIndik:="Immunesvekkelse"]
  
  d[cIndikNB %in% c(
    "Rutineundersøkelse av innvandrer"
  ), newIndik:="Innvandring"]
  
  d[cIndikNB %in% c(
    "Smitteoppsporing (miljøundersøkelse)"
  ), newIndik:="Smitteoppsporing"]
  
  d[cIndikNB %in% c(
    "Ubesvart",
    "Ukjent"
  ), newIndik:="Ubesvart/ukjent"]
  
  d[is.na(newIndik) & !is.na(cIndikNB), newIndik:="Annet"]
  
  xtabs(~d$newIndik)
  d[,newIndik := factor(newIndik,levels=c(
    "Arbeid",
    "Immunesvekkelse",
    "Innvandring",
    "Smitteoppsporing",
    "Annet",
    "Ubesvart/ukjent"
  ))]
  
  res <- d[,.(isTB=sum(isActive)),by=.(newIndik,cyear)]
  setorder(res,-cyear,newIndik)
  setcolorder(res,c("cyear","newIndik","isTB"))
  res <- AddTitle(res,title="Indikasjon for undersøkelse, tuberkulosetilfeller meldt MSIS")
  setattr(res,"filename","indikasjon_for_undersokelse.xlsx")
  return(res)
}




