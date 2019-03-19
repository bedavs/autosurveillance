palette <- "Set1"

Figure1 <- function(data, language="NB", USE_TITLE=TRUE){
  legend1Options <- list()
  legend1Options[["NB"]] <- c("Norskf\u00F8dte","Utenlandsf\u00F8dte","Totalt")
  legend1Options[["EN"]] <- c("Nor. born","Foreign born","All cases")
  
  
  legendOptions <- legend1Options[[language]]
  
  pd1 <- data[,.(isActive=base::sum(isActive,na.rm=T)),
              by=.(cyear)]
  pd1[,isForeignBorn:=2]
  
  pd2 <- data[,.(isActive=sum(isActive,na.rm=T)),
              by=.(cyear,isForeignBorn)]
  setcolorder(pd2,names(pd1))
  pd <- rbind(pd1,pd2)
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),0:2,stringsAsFactors = FALSE))
  setnames(skeleton, c("cyear","isForeignBorn"))
  pd <- merge(skeleton,pd,by=c("cyear","isForeignBorn"),all.x=TRUE)
  pd[is.na(isForeignBorn), isForeignBorn:=0]
  
  pd[,prettyName:=""]
  pd[isForeignBorn==0,prettyName:=legendOptions[1]]
  pd[isForeignBorn==1,prettyName:=legendOptions[2]]
  pd[isForeignBorn==2,prettyName:=legendOptions[3]]
  pd <- pd[cyear > max(cyear)-35]
  
  if(language=="NB"){
    titleX <- "\u00C5r"
    titleY <- "Tuberkulosetilfeller"
    title <- sprintf("Meldte tuberkulosetilfeller etter f\u00F8dested\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  } else {
    titleX <- "Year"
    titleY <- "Tuberculosis cases"
    title <- sprintf("Tuberculosis cases by birthplace\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  }
  
  q <- ggplot(pd,aes(x=cyear,y=isActive,colour=prettyName,group=prettyName,nudgeX=0.4,bottomY=0))
  q <- q + geom_line(lwd=1.5,colour="black")
  q <- q + geom_line(lwd=1)
  q <- q + theme_gray(THEME_BASE_SIZE)
  q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + expand_limits(y=0)
  q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=3,byrow=T))
  q <- q + scale_fill_brewer(palette=palette)
  q <- q + theme(legend.position="bottom")
  if(USE_TITLE){
    q <- q + labs(title=title)
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(caption=DATA_CAPTION[[language]])
  } else {
    q <- q + scale_y_continuous(title,breaks=pretty_breaks())
  }
  
  tab <- copy(pd)
  tab <- dcast.data.table(tab,cyear~prettyName,value.var = "isActive")
  return(list(
    q=q,
    tab=tab
  ))
}

Figure2 <- function(data, language="NB", USE_TITLE=TRUE){
  pd <- data[isActive==1,
             .(isActive=sum(isActive,na.rm=T)),
             by=.(cyear,cFlandNB)]
  if(language=="EN") RecodeDT(pd,CountriesNBtoEN(),"cFlandNB")
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),unique(pd$cFlandNB),stringsAsFactors = FALSE))
  setnames(skeleton,c("cyear","cFlandNB"))
  
  pd <- merge(skeleton,pd,by=c("cyear","cFlandNB"),all.x=TRUE)
  pd[is.na(isActive), isActive:=0]
  pd[cyear==max(cyear),rank:=rank(-isActive)]
  pd[,rank:=mean(rank,na.rm=T),by=cFlandNB]
  

  pd <- pd[cyear > max(cyear)-15 & rank<7]
  
  if(language=="NB"){
    titleX <- "\u00C5r"
    titleY <- "Tuberkulosetilfeller"
    title <- sprintf("Vanligste f\u00F8deland for meldte tuberkulosetilfeller\nMSIS %s",max(pd$cyear))
  } else {
    titleX <- "Year"
    titleY <- "Tuberculosis cases"
    title <- sprintf("Most common birthplaces for tuberculosis cases\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  }
  
  pd[,prettyName:=cFlandNB]
  q <- ggplot(pd,aes(x=cyear,y=isActive,colour=prettyName,group=prettyName,nudgeX=0.4,bottomY=0))
  q <- q + geom_line(lwd=1.5,colour="black")
  q <- q + geom_line(lwd=1)
  q <- q + theme_gray(THEME_BASE_SIZE)
  q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + expand_limits(y=0)
  if(USE_TITLE){
    q <- q + labs(title=title)
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(caption=DATA_CAPTION[[language]])
  } else {
    q <- q + scale_y_continuous(title,breaks=pretty_breaks())
  }
  q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=3,byrow=T))
  q <- q + scale_fill_brewer(palette=palette)
  q <- q + theme(legend.position="bottom")
  
  tab <- copy(pd)
  tab <- dcast.data.table(tab,cyear~prettyName,value.var = "isActive")
  return(list(
    q=q,
    tab=tab
  ))
}



Figure3 <- function(data, language="NB", USE_TITLE=TRUE){
  pd1 <- data[!is.na(cAlgr) & !is.na(cNorwegianStatusNB),
              .(isActive=sum(isActive,na.rm=T)),
              by=.(cAlgr,cNorwegianStatusNB,cyear)]
  pd2 <- data[!is.na(cAlgr),
              .(isActive=sum(isActive,na.rm=T)),
              by=.(cAlgr,cyear)]
  pd2[,cNorwegianStatusNB:="Totalt"]
  setcolorder(pd2,names(pd1))
  pd <- rbind(pd1,pd2)
  
  switch <- c(
    "Totalt"="Total",
    "Utenlandsf\u00F8dte"="Foreign born",
    "Norskf\u00F8dt med to norskf\u00F8dte foreldre"="Nor. born: 2 Nor. born parents",
    "Norskf\u00F8dt med minst en utenlandsf\u00F8dt forelder"="Nor. born: >=1 foreign born parents"
  )
  if(language=="EN") RecodeDT(pd,switch,"cNorwegianStatusNB")
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),unique(pd$cNorwegianStatusNB),unique(pd$cAlgr),stringsAsFactors = FALSE))
  setnames(skeleton,c("cyear","cNorwegianStatusNB","cAlgr"))
  
  pd <- merge(skeleton,pd,by=c("cyear","cNorwegianStatusNB","cAlgr"),all.x=TRUE)
  pd[is.na(isActive), isActive:=0]
  
  pd <- pd[cyear==max(cyear)]
  
  xLabs <- sort(unique(pd$cAlgr))
  xVals <- 1:length(xLabs)
  pd[,x:=as.numeric(factor(cAlgr,levels=xLabs))]
  
  if(language=="NB"){
    titleX <- "Alder"
    titleY <- "Tuberkulosetilfeller"
    title <- sprintf("Meldte tuberkulosetilfeller etter f\u00F8dested og 10 \u00E5rs aldersgrupper\nMSIS %s",max(pd$cyear))
  } else {
    titleX <- "Age"
    titleY <- "Tuberculosis cases"
    title <- sprintf("Registered TB cases by birthplace and 10 year age groups\nMSIS %s",max(pd$cyear))
  }
  if(!USE_TITLE){
    if(language=="NB"){
      title <- sprintf("Meldte tuberkulosetilfeller etter f\u00F8dested\nog 10 \u00E5rs aldersgrupper, MSIS %s",max(pd$cyear))
    } else {
      title <- sprintf("Registered TB cases by birthplace\nand 10 year age groups, MSIS %s",max(pd$cyear))
    }
  }

  pd[,prettyName:=cNorwegianStatusNB]
  q <- ggplot(pd,aes(x=x,y=isActive,colour=prettyName,group=prettyName))
  q <- q + geom_line(lwd=1.5,colour="black")
  q <- q + geom_line(lwd=1)
  q <- q + theme_gray(THEME_BASE_SIZE)
  q <- q + scale_x_continuous("",breaks=xVals,labels=xLabs, minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  if(USE_TITLE){
    q <- q + labs(title=title)
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(caption=DATA_CAPTION[[language]])
  } else {
    q <- q + scale_y_continuous(title,breaks=pretty_breaks())
  }
  q <- q + expand_limits(y=0)
  q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=2,byrow=T))
  q <- q + scale_fill_brewer(palette=palette)
  q <- q + theme(legend.position="bottom")
  
  tab <- copy(pd)
  tab <- dcast.data.table(tab,cAlgr~prettyName,value.var = "isActive")
  return(list(
    q=q,
    tab=tab
  ))
}

Figure4 <- function(data, language="NB", USE_TITLE=TRUE){

  pd <- data[!is.na(cFylke) & !cFylke %in% c("Utenfor fastlandet","Ukjent fylke"),
              .(isActive=sum(isActive,na.rm=T)),
              by=.(cFylke,cyear)]
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),FYLKE_LEVELS,stringsAsFactors = FALSE))
  setnames(skeleton,c("cyear","cFylke"))
  
  pd <- merge(skeleton,pd,by=c("cyear","cFylke"),all.x=TRUE)
  pd[is.na(isActive), isActive:=0]
  
  if(language=="NB"){
    pd[,prettyName:=sprintf("TB meldt i %s",cyear)]
  } else {
    pd[,prettyName:=sprintf("TB registered %s",cyear)]
  }
  pd <- pd[cyear>max(cyear)-2]
  
  pd[,cFylke:=factor(cFylke, levels = rev(FYLKE_LEVELS))]
  
  if(language=="NB"){
    titleX <- ""
    titleY <- "Tuberkulosetilfeller"
    title <- sprintf("Antall tuberkulosetilfeller meldt etter fylke\nMSIS %s og %s", min(pd$cyear), max(pd$cyear))
  } else {
    titleX <- ""
    titleY <- "Tuberculosis cases"
    title <- sprintf("Tuberculosis cases by county\nMSIS %s and %s", min(pd$cyear), max(pd$cyear))
  }
  
  q <- ggplot(pd,aes(x=cFylke,y=isActive,fill=prettyName,group=prettyName))
  q <- q + geom_bar(stat="identity",position="dodge",width=0.9,alpha=0.7)
  q <- q + geom_text(aes(label=isActive),hjust=-0.1,vjust=0.5, position=position_dodge(width=0.9))
  q <- q + theme_gray(THEME_BASE_SIZE)
  q <- q + scale_x_discrete("")
  q <- q + coord_flip()
  q <- q + scale_colour_brewer("",palette=palette)
  q <- q + scale_fill_brewer("",palette=palette,guide=guide_legend(reverse=TRUE,ncol=2,byrow=T))
  if(USE_TITLE){
    q <- q + labs(title=title)
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(caption=DATA_CAPTION[[language]])
  } else {
    q <- q + scale_y_continuous(gsub("\n",", ",title),breaks=pretty_breaks())
  }
  q <- q + theme(legend.position="bottom")
  
  tab <- copy(pd)
  tab <- dcast.data.table(tab,cFylke~prettyName,value.var = "isActive")
  return(list(
    q=q,
    tab=tab
  ))
}

Figure5 <- function(data, language="NB", USE_TITLE=TRUE){
  
  switch <- c("Ukjent"="Unknown",
              "Under ett \u00E5r i Norge"="<1 year in Norway",
              "Ett til fire \u00E5r i Norge"="1-4 years in Norway",
              "5 \u00E5r eller mer i Norge"="5+ years in Norway",
              "Totalt utenlandsf\u00F8dte m kjent oppholdstid"="Foreign born with known duration")
  legendOptions <- list(
    "NB"=names(switch),
    "EN"=switch
  )[[language]]
  
  pd <- data[isForeignBorn==1]
  RecodeDT(pd,c("Ubesvart"="Ukjent"),"cPernorNB")
  pd <- pd[,.(isTB=sum(isActive,na.rm=T)),by=.(cPernorNB,cyear)]
  pdx <- pd[,.(isTB=sum(isTB)),by=cyear]
  pdx[,cPernorNB:="Totalt utenlandsf\u00F8dte m kjent oppholdstid"]
  setcolorder(pdx,names(pd))
  pd <- rbind(pd,pdx)
  pd <- pd[cyear>max(cyear)-10 & cPernorNB!="Totalt utenlandsf\u00F8dte m kjent oppholdstid"]
  
  if(language=="EN") RecodeDT(pd,switch,"cPernorNB")
  pd[,cPernorNB:=factor(cPernorNB,levels=legendOptions)]
  
  pd[,prettyName:=cPernorNB]
  setorder(pd,-cyear,-prettyName)
  
  pd[,denom:=sum(isTB),by=cyear]
  pd[,perc:=100*isTB/denom]
  pd[,textPos := cumsum(perc)-perc/2,by=cyear]
  pd[,label:=paste0("N=",isTB,"\n",round(perc),"%")]
  
  if(language=="NB"){
    titleX <- "\u00C5r"
    titleY <- "Prosent"
    title <- sprintf("Oppholdstid i Norge f\u00F8r diagnose\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  } else {
    titleX <- "Year"
    titleY <- "Percent"
    title <- sprintf("Residence duration in Norway before diagnosis\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  }
  
  q <- ggplot(pd,aes(x=cyear,y=perc,fill=prettyName))
  q <- q + geom_bar(stat="identity",alpha=0.7)
  q <- q + geom_text(aes(label=label,y=textPos),hjust=0.5,vjust=0.5)
  q <- q + theme_gray(THEME_BASE_SIZE)
  q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + expand_limits(y=0)
  q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=5,byrow=T))
  q <- q + scale_fill_brewer(NULL,palette=palette,guide=guide_legend(ncol=5,byrow=T))
  q <- q + theme(legend.position="bottom")
  if(USE_TITLE){
    q <- q + labs(title=title)
    q <- q + scale_y_continuous(titleY,lim=c(0,100),breaks=pretty_breaks())
    q <- q + labs(caption=DATA_CAPTION[[language]])
  } else {
    q <- q + scale_y_continuous(title,lim=c(0,100),breaks=pretty_breaks())
  }
  
  tab <- copy(pd)
  tab <- dcast.data.table(tab,cyear~prettyName,value.var = "isTB")
  return(list(
    q=q,
    tab=tab
  ))
}


Figure6 <- function(data, language="NB", USE_TITLE=TRUE){
  
  switch <- c(
              "Norge"="Norway",
              "Tidligere Sovjet"="Previously Soviet",
              "Andre europeiske land"="Other European",
              "Afrika"="Africa",
              "Asia"="Asia")
  legendOptions <- list(
    "NB"=names(switch),
    "EN"=switch
  )[[language]]
  
  pd1 <- data[,
              .(isMDR=sum(isMDR,na.rm=T)),
              by=.(cBirthPlaceSovietNB,cyear)]
  
  # pd2 <- data[,
  #             .(isMDR=sum(isMDR,na.rm=T)),
  #             by=.(cyear)]
  # pd2[,cBirthPlaceSovietNB:="Totalt"]
  # setcolorder(pd2,names(pd1))
  # pd <- na.omit(rbind(pd1,pd2))
  pd <- pd1
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),unique(pd$cBirthPlaceSovietNB),stringsAsFactors = FALSE))
  setnames(skeleton,c("cyear","cBirthPlaceSovietNB"))
  
  pd <- merge(skeleton,pd,by=c("cyear","cBirthPlaceSovietNB"),all.x=TRUE)
  pd[is.na(isMDR), isMDR:=0]
  
  pd <- pd[cyear>max(cyear)-15]
  pd[,totalCases:=sum(isMDR),by=cBirthPlaceSovietNB]
  pd <- pd[totalCases>0]
  
  if(language=="NB"){
    titleX <- "\u00C5r"
    titleY <- "Multiresistente tuberkulosetilfeller"
    title <- sprintf("Antall multiresistente tuberkulosetilfeller etter f\u00F8dested\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  } else {
    titleX <- "Year"
    titleY <- "Multidrug-resistent tuberculosis cases"
    title <- sprintf("Multidrug-resistent tuberculosis cases by birthplace\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  }
  
  pd <- pd[cBirthPlaceSovietNB!="Totalt"]

  if(language=="EN") RecodeDT(pd,switch,"cBirthPlaceSovietNB")
  pd[,prettyName:=factor(cBirthPlaceSovietNB,levels=legendOptions)]
  
  q <- ggplot(pd,aes(x=cyear,y=isMDR,fill=prettyName))
  q <- q + geom_bar(stat="identity",alpha=0.7)
  q <- q + theme_gray(THEME_BASE_SIZE)
  q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  if(USE_TITLE){
    q <- q + labs(title=title)
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(caption=DATA_CAPTION[[language]])
  } else {
    q <- q + scale_y_continuous(title,breaks=pretty_breaks())
  }
  q <- q + expand_limits(y=0)
  q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=5,byrow=T))
  q <- q + scale_fill_brewer(NULL,palette=palette,guide=guide_legend(ncol=5,byrow=T))
  q <- q + theme(legend.position="bottom")
  
  tab <- copy(pd)
  tab <- dcast.data.table(tab,cyear~prettyName,value.var = "isMDR")
  return(list(
    q=q,
    tab=tab
  ))
}

Figure7 <- function(data, language="NB", USE_TITLE=TRUE){
  
  switch <- c("Norskf\u00F8dte p\u00E5 forebyggende behandling"="Norw. born on preventative treatment",
              "Utenlandsf\u00F8dte p\u00E5 forebyggende behandling"="Foreign born on preventative treatment",
              "Forebyggende behandling totalt"="Total preventative treatment",
              "Tuberkulosetilfeller"="Tuberculosis cases")

  legendOptions <- list(
    "NB"=names(switch),
    "EN"=switch
  )[[language]]
  
  pd1 <- data[!is.na(isForeignBorn),
              .(val=sum(isLatentTB,na.rm=T)),
              by=.(cyear,isForeignBorn)]
  pd2 <- data[,
              .(val=sum(isLatentTB,na.rm=T)),
              by=.(cyear)]
  pd2[,isForeignBorn:=2]
  setcolorder(pd2,names(pd1))
  
  pd3 <- data[,
              .(val=sum(isActive,na.rm=T)),
              by=.(cyear)]
  pd3[,isForeignBorn:=3]
  setcolorder(pd3,names(pd1))
  
  pd <- rbind(pd1, pd2, pd3)
  pd[,prettyName:=factor(isForeignBorn,levels=c("0","1","2","3"))]
  
  pd <- na.omit(pd[cyear > max(cyear)-15])

  if(language=="NB"){
    titleX <- "\u00C5r"
    titleY <- "Antall"
    title <- sprintf("Forebyggende behandling av latent tuberkulose\nfordelt etter opprinnelse\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  } else {
    titleX <- "Year"
    titleY <- "Number"
    title <- sprintf("Preventative treatment of latent tuberculosis by origin\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  }
  if(!USE_TITLE){
    if(language=="NB"){
      title <- sprintf("Forebyggende behandling av latent tuberkulose\nfordelt etter opprinnelse, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Preventative treatment of latent tuberculosis by origin, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  }
   
  levels(pd$prettyName) <- legendOptions
  
  q <- ggplot(pd,aes(x=cyear,y=val,colour=prettyName,group=prettyName))
  q <- q + geom_line(lwd=1.5,colour="black")
  q <- q + geom_line(lwd=1)
  q <- q + theme_gray(THEME_BASE_SIZE)
  q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  if(USE_TITLE){
    q <- q + labs(title=title)
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(caption=DATA_CAPTION[[language]])
  } else {
    q <- q + scale_y_continuous(title,breaks=pretty_breaks())
  }
  q <- q + expand_limits(y=0)
  q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=2,byrow=T))
  q <- q + scale_fill_brewer(palette=palette)
  q <- q + theme(legend.position="bottom")

  tab <- copy(pd)
  tab <- dcast.data.table(tab,cyear~prettyName,value.var = "val")
  return(list(
    q=q,
    tab=tab
  ))
}



Figure8 <- function(data, language="NB", USE_TITLE=TRUE){
  
  switch <- c(
    "Totalt"="Total",
    "Utenlandsf\u00F8dte"="Foreign born",
    "Norskf\u00F8dt med to norskf\u00F8dte foreldre"="Nor. born: 2 Nor. born parents",
    "Norskf\u00F8dt med minst en utenlandsf\u00F8dt forelder"="Nor. born: >=1 foreign born parents"
  )
  legendOptions <- list(
    "NB"=names(switch),
    "EN"=switch
  )[[language]]
  
  pd1 <- data[,
              .(isLatentTB=sum(isLatentTB,na.rm=T)),
              by=.(cAlgr,cNorwegianStatusNB,cyear)]
  pd2 <- data[,
              .(isLatentTB=sum(isLatentTB,na.rm=T)),
              by=.(cAlgr,cyear)]
  pd2[,cNorwegianStatusNB:="Totalt"]
  setcolorder(pd2,names(pd1))
  pd <- na.omit(rbind(pd1,pd2))
  
  yearMin <- min(pd$cyear,na.rm=T)
  yearMax <- max(pd$cyear,na.rm=T)
  skeleton <- data.table(expand.grid(c(yearMin:yearMax),unique(pd$cNorwegianStatusNB),unique(pd$cAlgr),stringsAsFactors = FALSE))
  setnames(skeleton,c("cyear","cNorwegianStatusNB","cAlgr"))
  
  pd <- merge(skeleton,pd,by=c("cyear","cNorwegianStatusNB","cAlgr"),all.x=TRUE)
  pd[is.na(isLatentTB), isLatentTB:=0]
  
  pd <- pd[cyear==max(cyear)]
  if(language=="EN") RecodeDT(pd,switch,"cNorwegianStatusNB")
  
  xLabs <- sort(unique(pd$cAlgr))
  xVals <- 1:length(xLabs)
  pd[,x:=as.numeric(factor(cAlgr,levels=xLabs))]
  
  if(language=="NB"){
    titleX <- "Alder"
    titleY <- "Antall med forebyggende behandling"
    title <- sprintf("Forebyggende behandling etter aldersgruppe, for norskf\u00F8dte og utenlandsf\u00F8dte\nMSIS %s",min(pd$cyear))
  } else {
    titleX <- "Age"
    titleY <- "Number with preventative treatment"
    title <- sprintf("Preventative treatment by age, for Norwegian and foreign born\nMSIS %s ",min(pd$cyear))
  }
  if(!USE_TITLE){
    if(language=="NB"){
      title <- sprintf("Forebyggende behandling etter aldersgruppe,\nfor norskf\u00F8dte og utenlandsf\u00F8dte, MSIS %s",min(pd$cyear))
    } else {
      title <- sprintf("Preventative treatment by age,\nfor Norwegian and foreign born, MSIS %s ",min(pd$cyear))
    }
  }
  
  pd[,prettyName:=cNorwegianStatusNB]
  q <- ggplot(pd,aes(x=x,y=isLatentTB,colour=prettyName,group=prettyName))
  q <- q + geom_line(lwd=1.5,colour="black")
  q <- q + geom_line(lwd=1)
  q <- q + theme_gray(THEME_BASE_SIZE)
  q <- q + scale_x_continuous("",breaks=xVals,labels=xLabs, minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  if(USE_TITLE){
    q <- q + labs(title=title)
    q <- q + scale_y_continuous(titleY,breaks=pretty_breaks())
    q <- q + labs(caption=DATA_CAPTION[[language]])
  } else {
    q <- q + scale_y_continuous(title,breaks=pretty_breaks())
  }
  q <- q + expand_limits(y=0)
  q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=2,byrow=T))
  q <- q + scale_fill_brewer(palette=palette)
  q <- q + theme(legend.position="bottom")

  tab <- copy(pd)
  tab <- dcast.data.table(tab,cAlgr~prettyName,value.var = "isLatentTB")
  return(list(
    q=q,
    tab=tab
  ))
}

Figure9 <- function(data, language="NB", USE_TITLE=TRUE){
  
  switch <- c(
    "Ubesvart/ukjent"="Unanswered/unknown",
    "Annen indikasjon"="Other indication",
    "Symptomer eller tegn"="Symptoms or signs",
    "Smitteoppsporing"="Contact tracing",
    "Screening, arbeid"="Screening, work",
    "Screening, immunsvekkelse"="Screening, immunocompromised",
    "Screening, innvandring"="Screening, immigrant"
  )
  legendOptions <- list(
    "NB"=names(switch),
    "EN"=switch
  )[[language]]
  
  pd <- copy(data)
  pd[,newIndik:=as.character(NA)]
  pd[cIndikNB %in% c(
    "Arbeid med pasienter eller barn"
  ), newIndik:="Screening, arbeid"]
  
  pd[cIndikNB %in% c(
    "Immunsvekkende tilstand/behandling"
  ), newIndik:="Screening, immunsvekkelse"]
  
  pd[cIndikNB %in% c(
    "Rutineunders\u00F8kelse av innvandrer"
  ), newIndik:="Screening, innvandring"]
  
  pd[cIndikNB %in% c(
    "Smitteoppsporing (milj\u00F8unders\u00F8kelse)"
  ), newIndik:="Smitteoppsporing"]
  
  pd[cIndikNB %in% c(
    "Symptomer eller tegn"
  ), newIndik:="Symptomer eller tegn"]
  
  pd[cIndikNB %in% c(
    "Ubesvart",
    "Ukjent"
  ), newIndik:="Ubesvart/ukjent"]
  
  pd[is.na(newIndik) & !is.na(cIndikNB), newIndik:="Annen indikasjon"]
  
  if(language=="EN") RecodeDT(pd,switch,"newIndik")
  pd[,newIndik := factor(newIndik,levels=legendOptions)]
  
  pd <- pd[cyear>max(cyear)-10,.(isTB=sum(isActive)),by=.(newIndik,cyear)]
  setorder(pd,-cyear,newIndik)
  setcolorder(pd,c("cyear","newIndik","isTB"))
  
  pd[,prettyName:=newIndik]
  setorder(pd,-cyear,-newIndik)
  
  pd[,denom:=sum(isTB),by=cyear]
  pd[,perc:=100*isTB/denom]
  pd[,textPos := cumsum(perc)-perc/2,by=cyear]
  pd[,label:=paste0("N=",isTB,"\n",round(perc),"%")]
  pd[perc<10,label:=paste0(round(perc),"%")]
  pd[perc<=2,label:=""]
  
  if(language=="NB"){
    titleX <- "\u00C5r"
    titleY <- "Prosent"
    title <- sprintf("Indikasjon for tuberkuloseunders\u00F8kelse\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  } else {
    titleX <- "Year"
    titleY <- "Percent"
    title <- sprintf("Indication for tubuerculosis examination\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
  }
  
  q <- ggplot(pd,aes(x=cyear,y=perc,fill=prettyName))
  q <- q + geom_bar(stat="identity",alpha=0.7)
  q <- q + geom_text(aes(label=label,y=textPos),hjust=0.5,vjust=0.5)
  q <- q + theme_gray(THEME_BASE_SIZE)
  q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  if(USE_TITLE){
    q <- q + labs(title=title)
    q <- q + scale_y_continuous(titleY,lim=c(0,100),breaks=pretty_breaks())
    q <- q + labs(caption=DATA_CAPTION[[language]])
  } else {
    q <- q + scale_y_continuous(title,lim=c(0,100),breaks=pretty_breaks())
  }
  q <- q + expand_limits(y=0)
  q <- q + scale_fill_manual(NULL,values=c(
      "#1a9850",
      "#cab2d6",
      "#abd9e9",
      "#fee090",
      "#fdae61",
      "#f46d43",
      "#d73027"
    ),guide=guide_legend(ncol=3,byrow=T))
  q <- q + theme(legend.position="bottom")
  #q
  tab <- copy(pd)
  tab <- dcast.data.table(tab,cyear~prettyName,value.var = "isTB",fill=0)
  return(list(
    q=q,
    tab=tab
  ))
}



Figure10 <- function(data, language="NB", USE_TITLE=TRUE){
  
switchNB <- c(
    "isActive"="Meldte TB",
    "isCulturePos"="Dyrkningspos",
    "isRRes"="Rifampicin",
    "isHRes"="Isoniazid",
    "isPRes"="Pyrazinamide",
    "isERes"="Ethambutol",
    "isSRes"="Streptomycin",
    "isMDR"="MDR-TB"
  )
  
  switchEN <- c(
    "isActive"="Registered TB",
    "isCulturePos"="Culture positive",
    "isRRes"="Rifampicin",
    "isHRes"="Isoniazid",
    "isPRes"="Pyrazinamide",
    "isERes"="Ethambutol",
    "isSRes"="Streptomycin",
    "isMDR"="MDR-TB"
  )
  
  switch <- list(
    "NB"=switchNB,
    "EN"=switchEN
  )[[language]]
  
  pd <- copy(data)
  pd <- pd[isActive==1 & cyear>max(cyear)-10,.(
    isActive=sum(isActive,na.rm=T),
    isCulturePos=sum(cDyrkResAllOrganNB=="Positivt",na.rm=T),
    isRRes=sum(isRRes,na.rm=T),
    isHRes=sum(isHRes,na.rm=T),
    isPRes=sum(isPRes,na.rm=T),
    isERes=sum(isERes,na.rm=T),
    isSRes=sum(isSRes,na.rm=T),
    isMDR=sum(isMDR,na.rm=T)
  ),by=cyear]
  
  pd[cyear>2015,isSRes:=NA]
  
  tab <- copy(pd)
  
  pd[,isRRes:=isRRes/isCulturePos*100]
  pd[,isHRes:=isHRes/isCulturePos*100]
  pd[,isPRes:=isPRes/isCulturePos*100]
  pd[,isERes:=isERes/isCulturePos*100]
  pd[,isSRes:=isSRes/isCulturePos*100]
  pd[,isMDR:=isMDR/isCulturePos*100]
  
  # manual cleaning for not yet reported stuff
  #pd[cyear>=2016,isSRes:=NA]
  
  pd <- melt.data.table(pd,id="cyear")
  
  pd <- pd[!variable%in%c("isActive","isCulturePos")]
  
  if(language=="NB"){
    title <- sprintf("Prosentvis resistens av totalt antall dyrkningspositive meldte tuberkulosetilfeller\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
    titleY <- "Prosent"
  } else {
    title <- sprintf("Percent resistance in total culture positive registered TB cases\nMSIS %s - %s",min(pd$cyear),max(pd$cyear))
    titleY <- "Percent"
  }
  if(!USE_TITLE){
    if(language=="NB"){
      title <- sprintf("Prosentvis resistens av totalt antall dyrkningspositive\nmeldte tuberkulosetilfeller, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    } else {
      title <- sprintf("Percent resistance in total culture positive\nregistered TB cases, MSIS %s - %s",min(pd$cyear),max(pd$cyear))
    }
  }
  
  RecodeDT(pd,switch,"variable")
  
  q <- ggplot(pd,aes(x=cyear,y=value,colour=variable,group=variable))
  q <- q + theme_gray(THEME_BASE_SIZE)
  q <- q + geom_line(lwd=1.5,colour="black")
  q <- q + geom_line(lwd=1)
  q <- q + expand_limits(y=c(0,25))
  q <- q + scale_x_continuous("",breaks=seq(min(pd$cyear),max(pd$cyear),1),minor_breaks=NULL)
  q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
  q <- q + scale_colour_brewer(NULL,palette=palette,guide=guide_legend(ncol=3,byrow=T))
  if(USE_TITLE){
    q <- q + labs(title=title)
    q <- q + scale_y_continuous(titleY)
    q <- q + labs(caption=DATA_CAPTION[[language]])
  } else {
    q <- q + scale_y_continuous(title)
  }
  q <- q + theme(legend.position="bottom")
  
  setnames(tab,"isActive","isActiveTB")
  return(list(
    q=q,
    tab=tab
  ))
}

