mydataLL <- function (d1,d2)  {
  meanLL=NULL
  if (d1 < d2) {
    meanLL=d1
  } else {
    meanLL=d2
  }
  return(meanLL)
}
################################################################
today <- function () {
  as.Date(Sys.time(), "%Y-%m-%d")
}

################################################################
current.month <- function (d) {
  format(d, "%Y-%m")
}

################################################################
add.remove.years <- function (date,n) {
  seq(date, by = paste (n, "years"), length =2)[2]
}

add.remove.month <- function (date,n) {
  seq(date, by = paste (n, "months"), length =2)[2]
}
################################################################
change.diag.names <- function (data,g) {
  if (g=="SMSB") {
    data[Smstoff=="Hep B-virus, akutt", Smstoff:="HepBakutt"]
    data[Smstoff=="Hep B-virus, kronisk", Smstoff:="HepBkron"]
    data[Smstoff=="Hep C-virus", Smstoff:="Hepc"]
    
    # } else  if (g=="SMSH") {
    #   data[,Smstoff:=iconv(data$Smstoff, "latin1","ASCII//TRANSLIT")]
    #   data[,Infeksjonsstatus:=iconv(data$Infeksjonsstatus, "latin1","ASCII//TRANSLIT")]
    #   
    #   data[Diag=="MRSA"& Infeksjonsstatus=="Barerskap", Smstoff:="Staph aureus meticillinres, baring"]
    #   data[Diag=="MRSA"& Infeksjonsstatus=="Infeksjon", Smstoff:="Staph aureus meticillinres, inf"]
    #   data[Diag=="MRSA"& Infeksjonsstatus=="Ukjent", Smstoff:="Staph aureus meticillinres, ukjentstatus"]
    #   data[Diag=="MRSA"& is.na(Infeksjonsstatus)==T, Smstoff:="Staph aureus meticillinres, ukjentstatus"]
    #   
  }
  
  return(data)
  
}

################################################################
create.microbe.var <- function (data,g) {
  
  if (g=="SMSH") {
    data[,vre:=0]
    data$vre[data$Smstoff=="Enterokokker vankomycinres"]<-1
    data[,lre:=0]
    data$lre[data$Smstoff=="Enterokokker linezolidresistent"]<-1
    data[,lvre:=0]
    data$lvre[data$Smstoff=="Enterokokker van- og linezolidresistent"]<-1
    data[,esbl:=0]
    data$esbl[data$Diag=="Resistent gram negativ stav "]<-1
    data[,mrsa:=0]
    data$mrsa[data$Smstoff=="Staph aureus meticillinres, bæring" | 
                data$Smstoff =="Staph aureus meticillinres, inf" | data$Diag == "MRSA"]<-1
    data[,strep:=0]
    data$esbl[data$Smstoff=="Str pn penicillinres"]<-1
    
  } else if (g=="SMSM") {
    data[,dengue:=0]
    data$dengue[data$Diag=="Denguefeber"]<-1
    data[,crypto:=0]
    data$crypto[data$Diag=="Cryptosporidose"]<-1
    data[,qfeb:=0]
    data$crypto[data$Diag=="Q-feber"]<-1
  }
  
  return(data)
}

################################################################
calculate.means <- function() {
  
  dataset <- dataset[,c("Paar", "Pmnd","Smstoff", "Diag", "Pdato", "Month_Yr", "i"), with=F]
  dataset <- dataset[order(Smstoff, Paar, Pmnd)]
  setkey(dataset,Smstoff, Paar, Pmnd)
  dataset[,montot:= sequence(.N), by = key(dataset)]
}

################################################################
total.years.for.means <- function(data, group) {
  if (group=="SMSM") {
    data$totyrs[data$dengue==1] <-3
    data$totyrs[data$crypto==1] <-3
    data$totyrs[data$qfeb==1] <-3
  } else if (group=="SMSH") {
    data$totyrs[data$vre==1] <-5   
    data$totyrs[data$lre==1] <-5   
    data$totyrs[data$lvre==1] <-5   
    data$totyrs[data$esbl==1] <-5   
    data$totyrs[data$mrsa==1] <-5   
    data$totyrs[data$strep==1] <-5   
    
  }
  return(data)
}
################################################################

Get.Months.Order <- function(today) {
  if (today==1) {
    a=1:12
  } else {
    a=c(today:12,1:(today-1))
  }
  
  return(a)
}

#################################################################

get.each.Diag <- function(data) {
  Smstoff=NULL
  Pmnd=NULL
  for (d in 1:length(unique(levels(data)))) {
    Smstoff=c(Smstoff,rep(as.character(unique(levels(data))[d]),12))
    Pmnd=c(Pmnd,rep(1:12))
  }
  n=data.table(Smstoff=as.character(Smstoff),Pmnd=as.numeric(Pmnd))
  return(n)
} 

#################################################################
get.tub.akti <- function (data) {
  data[,numkat:=0]
  
  data$numkat[data$Kategori=="Forebyggende behandling"]<-10
  data$numkat[data$Kategori == "Innflyttet under behandling"]<-1
  data$numkat[data$Kategori == "Tidligere tuberkulose kjemoterapibehandlet"]<-2
  data$numkat[data$Kategori == "Tidligere tuberkulose ??ikke kjemoterapibehandlet "]<-3
  data$numkat[data$Kategori == "Tuberkulose for f????e gang"]<-4
  data$numkat[data$Kategori == "Tuberkulose usikker kategori"]<-5
  data$numkat[data$Kategori == "Vurderes"]<-6
  data$numkat[data$Kategori == "Feil diagnose"]<-7
  
  data <- data[numkat!=7]
  data <- data[numkat!=6]
  data[,TB:="NA"]
  data$TB[data$numkat!=10]<-"Aktivt TB"
  data$TB[data$numkat==10]<-"Latent TB"
  
  return(data)
}

#################################################################

create.age.groups <- function (data, numbertable){
  
  if (numbertable==1) {
    
    data[,agegroups:=0]
    data[,agegroupsN:=0]
    
    data$agegroups[data$Alaar <= 4] <-"<4"
    data$agegroupsN[data$Alaar <= 4] <-1
    
    data$agegroups[data$Alaar >=5 & data$Alaar <=14 ] <-"5-14"
    data$agegroupsN[data$Alaar >=5 & data$Alaar <=14 ] <-2
    
    data$agegroups[data$Alaar >=15 & data$Alaar <=34 ] <-"15-34"
    data$agegroupsN[data$Alaar >=15 & data$Alaar <=34 ] <-3
    
    data$agegroups[data$Alaar >=35 & data$Alaar <=64 ] <-"35-64"
    data$agegroupsN[data$Alaar >=35 & data$Alaar <=64 ] <-4
    
    data$agegroups[data$Alaar >=65] <-">65"
    data$agegroupsN[data$Alaar >=65] <-5
    
  } else if (numbertable==2){
    
    data[,agegroups:=0]
    data[,agegroupsN:=0]
    
    data$agegroups[data$Ald <= 365] <-"0-1"
    data$agegroupsN[data$Ald <= 365] <-1
    
    data$agegroups[data$Ald >= 366] <-">1"
    data$agegroupsN[data$Ald >= 366] <-2
    
    data <- data[data$agegroups!=0]
    
  }
  return(data)
}

######

order.age.groups <- function (data) {
  a=unique(data.frame(data$agegroups,data$agegroupsN))
  a=a[order(a[,2]),]
  a=a$data.agegroups
  a=gsub("-", "' - ",a)
  a=gsub("<", "< ",a)
  a=gsub(">", "> ",a)
  
  return(a)
}




#################################################################
select.diag.SMSL <- function (data,numbertable) {
  if (numbertable==1) {
    data <- data[data$Diag=="Syst. pneumokokksykdom"]
  } else if (numbertable==2) {
    data <- data[data$Diag=="Kikhoste"]
  }
  return(data)
}

#################### MAIN FUNCTIONS##############################
## 1. clean data
Data.Clean <- function (data, g) {
  
  data[,Pdato:=as.Date(data$Pdato)]
  
  ## this is to ensure ej. "2000-01-31" is YM "2000-01" and not "2000-02"
  data[,Month_Yr:=format(as.Date(paste(data$Paar,data$Pmnd,"01", sep = "-")),"%Y-%m")]
  
  
  ## this is to ensure no entries from the "future"
  t=suppressWarnings(today())
  
  data[,today:=t]
  data[,futureobs:=data$today-data$Pdato]
  data <- data[futureobs>=0]
  
  ## month.index()
  ## indexes by year and month in order
  data <- data[order(Paar, Pmnd)]
  setkey(data,Paar, Pmnd)
  data[,monthindex:=.GRP,by=key(data)]
  
  ## remove entries from current months
  
  data <- data[Month_Yr!=current.month(t)]
  
  ## dates index for upper and lower limits of data
  
  lastmonthdate=add.remove.month(t,-1)
  todaylastyear=add.remove.years(t,-1)
  lastmonthminusfiveyear=add.remove.years(lastmonthdate,-5)
  todaylastyearminusfiveyear=add.remove.years(todaylastyear,-5)
  
  
  dataUL=format(lastmonthdate, "%Y-%m")
  dataLL=format(mydataLL(lastmonthminusfiveyear,todaylastyearminusfiveyear), "%Y-%m")
  countUL=format(lastmonthdate, "%Y-%m")
  countLL=format(todaylastyear, "%Y-%m")
  
  
  ## keep only data for the last 5 years
  data <- data[data$Month_Yr<=dataUL]
  data <- data[data$Month_Yr>=dataLL]
  
  ## indexes last month
  data[,one:=1]
  data[,lastmonth:=0]
  
  data$lastmonth[data$Month_Yr<=countUL & data$Month_Yr>=countLL]<-1
  
  data$one[data$lastmonth==1]<-0
  
  
  ##6. change.change.diag.names()
  #data <- change.diag.names(data,group)
  
  ##7. Only For SMSH
  
  data <-create.microbe.var(data,group)
  
  
  return(data)
  
}  

################################################################
## 2. calculate.totals.means()
calculate.totals.mean <- function(data) {
  data <- data[order(Paar, Pmnd)]
  setkey(data, Smstoff, Paar, Pmnd)
  
  data[, i := sequence(.N), by = key(data)]
  data[, montotal := .N, by = key(data)]
  data[, montotal_ones := .N, by = key(data)]
  data$montotal_ones[data$lastmonth==1] <-0
  
  setkey(data, Smstoff, Pmnd)
  
  data[, meanmontotal:= mean(montotal), by = key(data)]
  data[, meanmmontotal_ones:= mean(montotal_ones), by = key(data)]
  
  data <- data[order(Paar, Pmnd ,Smstoff)]
  data <- data[,c("Smstoff","Paar", "monthindex","Month_Yr", "lastmonth", "Pmnd","montotal", "montotal_ones", "meanmontotal", "meanmmontotal_ones", "Diag"),with=F]
  
  
  data<- data[order(Paar,Pmnd,Smstoff)]
  setkey(data, Paar,Pmnd,Smstoff)
  
  data[, i := sequence(.N), by = key(data)]
  data[, withinmonthindex := sequence(.N), by = key(data)]
  data[, montot_onesv := montotal_ones]
  data$montot_onesv[data$withinmonthindex!=1] <-NA
  
  data<- data[order(Smstoff, Pmnd)]
  setkey(data, Smstoff, Pmnd)
  data[, meanmontot_onesv2:= mean(montot_onesv,na.rm = T), by = key(data)]
  
  data[, totyrs := 5]

  data<- data[order(Smstoff, Pmnd)]
  setkey(data, Smstoff, Pmnd)
  data[, tablemean:= .N, by = key(data)]
  data[lastmonth!=1,tablemeanTest1:= .N, by = key(data)]
  data[lastmonth==1,tablemeanTest2:= .N, by = key(data)]
  data[lastmonth==1,tablemeanTest1:=tablemean-tablemeanTest2]
  
  ###
  data[, tablemeansv2:= tablemeanTest1/totyrs]
  
  data<- data[order(Smstoff)]
  setkey(data, Smstoff)
  data[, lastmonthmean:= mean(lastmonth), by = key(data)]
  
  return(data)
  
}



################################################################
## 3. prep.to.table()
prep.to.table <- function(data, group) {
  #data <- data[monthindex>max(data$monthindex)-12]
  means <- unique(data[,c("Diag","Smstoff","Pmnd","tablemeansv2", "lastmonthmean"),with=F])
  counts <- data[monthindex>max(data$monthindex)-12]
  counts <- unique(counts[,c("Smstoff","Pmnd","montotal"),with=F])
  
  index <- get.each.Diag(as.factor(data$Smstoff))
  
  setkey(means,Smstoff,Pmnd)
  means <- merge(means, index,all=T)
  setkey(counts,Smstoff,Pmnd)
  counts <- merge(counts, index,all=T)
  
  data <- merge(counts, means,all=T)
  
  if (group== "SMSL" | group== "SMSH" | group== "SMSM") {
    data <- data[lastmonthmean!=0]
  }
  if  (group!= "SMSH") {
    #data <- data[,Smstoff:=droplevels(Smstoff)] ### may not work with SMSH
    index <- get.each.Diag(as.factor(data$Smstoff))
  } else if  (group== "SMSH") {
    #data <- data[,Smstoff:=droplevels(Smstoff)]
    index <- get.each.Diag(as.factor(data$Smstoff))
  }
  setkey(data,Smstoff,Pmnd)
  data=merge(data, index,all=T)
  setkey(data, Smstoff,Pmnd)
  data <- unique(data,by = key(data))
  
  t=suppressWarnings(today())  
  myindex= data.table(ord=c(12,1:11),Pmnd=Get.Months.Order(as.numeric(format(t-30,"%m"))))
  
  setkey(data, Pmnd)
  setkey(myindex, Pmnd)
  
  data <-  merge(data,myindex,all = T)
  data <-  data[order(Smstoff,ord)]
  ############################################################
  final = data.table(Diag=data$Diag, Smstoff=data$Smstoff, Pmnd=data$Pmnd, Total=data$montotal, Means=data$tablemeansv2)
  final <- final[is.na(Smstoff)!=T]
  
  return(final)
  
  
}

################################################################
## 4. create.table()
create.table <- function(data, group) {
  
  if (group=="SMSL"){
    Smstoff=unique(data$Smstoff)
  } else {
    Smstoff=sort(unique(data$Smstoff))
  }
  
  months = unique(data$Pmnd)
  table=NULL
  Names=NULL
  Names1=NULL
  Names2=NULL
  diag=NULL
  for (d in Smstoff) {
    out=data[Smstoff==d]
    print(d) 
    print(dim(out))
    
    table=rbind(table,out$Total, out$Means)
    mydiag=rep(levels(droplevels(out$Diag)),2)
    #print(levels(droplevels(out$Diag)))
    diag=c(diag,mydiag)
    Names=c(Names,c(paste(d,"(Total/month)",sep = " "),paste("","(Mean/month)",sep = " ")))
    Names1=c(Names1,rep(d,2))
    Names2=c(Names2,c("Total","Mean"))
    
  }
  
  if (group=="SMSM" | group=="SMSL"){
    table[is.na(table)] <-0
    table=format(table,digits=1, decimal.mark=",")
    table=cbind(diag,Names1,Names2,table)
    colnames(table)=c("Diag","Smstoff"," ",month.abb[months]);
    #rownames(table)=diag  
    #table=table[order(row.names(table)),]
    table=table[order(diag),]
    
  } else {
    #colnames(table)=months
    #rownames(table)=Names
    table[is.na(table)] <-0
    table=format(table,digits=1, decimal.mark=",")
    table=cbind(Names1,Names2,table)
    colnames(table)=c("Diag"," ",month.abb[months])
  }

  
  return(table)
}


######################################
## 1. clean data TUB
Data.Clean.TUB <- function (data, g, tablenumer) {
  
  #data=dataset
  if (tablenumer==1) {
    data <- data[data$TB=="Aktivt TB"]
    ## keep only data from 2009
    data <- data[Paar>2009] ## ask when this is change e.g01/01/18
  } else if (tablenumer==2) {
    ## keep only data from 2012 ## ask when this is change e.g01/01/18
    data <- data[Paar>2012]
  }
  
  ## data into dates
  data[,Month_Yr:=format(as.Date(paste(data$Paar,data$Pmnd,"01", sep = "-")),"%Y-%m")]
  data[,pdato:=copy(data$Pdato)]
  data[,regdato:=copy(data$Regdato)]
  data[,behdate:=copy(data$Startdato_beh)]
  
  ## this is to ensure no entries from the "future"
  t=suppressWarnings(today())
  data[,today:=t]
  data[,futureobs:=t-as.Date(data$Pdato)]
  data <- data[futureobs>0] 
  
  ##get.first.data
  data[,firstdate:=pmin(pdato, regdato, behdate, na.rm=T)]
  
  data[,firstmonth:=format(data$firstdate,"%m")]
  data[,firstyear:=format(data$firstdate,"%Y")]
  data[,Month_YrTest:=format(data$firstdate, "%Y-%m")]
  
  ##
  data[,futureobs2:=t-as.Date(data$firstdate)]
  data <- data[futureobs2>0]
  
  ## month.index()
  ## indexes by year and month in order
  data <- data[order(firstyear, firstmonth)]
  setkey(data,firstyear, firstmonth)
  data[,monthindex:=.GRP,by=key(data)]
  
  ## remove entries from current months
  if (tablenumer==1) {
    data$Month_Yr <- copy(data$Month_YrTest)
    data <- data[Month_Yr!=current.month(t)]
  }
  
  ## dates index for upper and lower limits of data
  
  lastmonthdate=add.remove.month(t,-1)
  todaylastyear=add.remove.years(t,-1)
  lastmonthminusfiveyear=add.remove.years(lastmonthdate,-5)
  todaylastyearminusfiveyear=add.remove.years(todaylastyear,-5)
  
  
  dataUL=format(lastmonthdate, "%Y-%m")
  dataLL=format(mydataLL(lastmonthminusfiveyear,todaylastyearminusfiveyear), "%Y-%m")
  countUL=format(lastmonthdate, "%Y-%m")
  countLL=format(todaylastyear, "%Y-%m")
  
  
  ## keep only data for the last 5 years <- does not apply to TUB
  #data <- data[data$Month_Yr<=dataUL]
  #data <- data[data$Month_Yr>=dataLL]
  
  ## indexes last month
  data[,one:=1]
  data[,lastmonth:=0]
  
  data$lastmonth[data$Month_Yr<=countUL & data$Month_Yr>=countLL]<-1
  #data$lastmonth[data$Month_Yr==format(add.remove.month(t,-1), "%Y-%m")]<-1
  
  data$one[data$lastmonth==1]<-0

  
  ## create.quaters
  if (tablenumer==2) {
    data[,Pdateq:=paste(format(data$firstdate, "%Y"), "q", quarter(data$firstdate), sep="")]
    data[,quater:=quarter(data$firstdate)]
    
    data <- data[order(Pdateq)]
    setkey(data, Pdateq)
    data[, iquater:=sequence(.N), by = key(data)]
    
  }
  
  ## create.quaters
  if (tablenumer==3) {
    data[,MDR:= "Følsom"]
    
    data[R_res=="Resistent" & H_res =="Resistent", MDR:="Resistent"]
    data[R_res=="Resistent" & H_res =="Lavgradig resistens", MDR:="Resistent"]
    data[is.na(R_res) & is.na(H_res), MDR:="Ukjent"]
    
  }
  
  return(data)
  
} 


################################################################
## 2. calculate means TUB
calculate.totals.mean.TUB <- function(data,tablenumer) {
  if (tablenumer==1) {
    data <- data[order(TB, firstyear, firstmonth)]
    setkey(data, TB, firstyear, firstmonth)
    ##############################################
    data[, i := sequence(.N), by = key(data)]
    data[, montotal := .N, by = key(data)]
    data[, montotal_ones := .N, by = key(data)]
    data$montotal_ones[data$lastmonth==1] <-0
    ##############################################
    data <- data[order(firstyear,firstmonth,TB)]
    setkey(data, firstyear, firstmonth,TB)
    data[, i := sequence(.N), by = key(data)]
    data[, withinmonthindex := sequence(.N), by = key(data)]
    data <- data[withinmonthindex==1] 
    ##############################################
    #data<- data[order(TB, Pmnd)]
    #setkey(data, TB, Pmnd)
    data<- data[order(TB, firstmonth)]
    setkey(data, TB, firstmonth)
    data[, meanmonthtb:= mean(montotal_ones), by = key(data)]
    data$montotal_ones[data$montotal_ones==0 & data$lastmonth==1] <- NA
    data[, meanmonthtb:= mean(montotal_ones, na.rm=T), by = key(data)]
    
    ##############################################
    #data <- data[data$monthindex>max(data$monthindex)-12] 
    data <- data[data$Month_Yr > format(add.remove.month(max(data$firstdate),-12),"%Y-%m")]
    data<- data[order(monthindex)]
    data[,cumtot:=cumsum(montotal)]
    data[,cumtot_avg:=cumsum(meanmonthtb)]
    
    data <- data[,c("TB","Smstoff","Paar", "monthindex","Month_Yr", "lastmonth", "Pmnd","montotal", "meanmonthtb", "cumtot", "cumtot_avg"),with=F]
    
    
  }
  else if (tablenumer==2) {
    
    ##############################################
    data <- data[order(TB, Pdateq)]
    setkey(data, TB, Pdateq)
    
    data[, totalquarter := .N, by = key(data)]
    data[, indexquarter := sequence(.N), by = key(data)]
    data$totalquarter[data$indexquarter!=1] <-NA
    ##############################################
    
    data <- data[order(TB, quater)]
    setkey(data, TB, quater)
    data[, meanquarter := mean(totalquarter, na.rm=T), by = key(data)]
    ##############################################
    
    
    data <- data[,c("TB","Paar", "monthindex","Pdateq", 
                    "lastmonth", "Pmnd","totalquarter", "meanquarter"),with=F]
    
  }
  
  
  return(data)
}


######################################
## 3. create table TUB
create.table.TUB <- function(data, tablenumber) {
  if (tablenumber==1) {
    table=format(rbind(data$montotal, data$meanmonthtb, data$cumtot,data$cumtot_avg),digits = 0)
    
    table[is.na(table)] <-0
    table=format(table,digits=1, decimal.mark=",")
    
    names1=c("Aktivt TB", "Aktivt TB","Aktivt TB","Aktivt TB")
    names2=c("Total", "Mean","Cumulative Total","Cumulative Mean")
    table=cbind(names1,names2,table)
    colnames(table)=c(" "," ",month.abb[data$Pmnd])

    } else if (tablenumber==2) {
    t=suppressWarnings(today())
    lastyear=add.remove.years(t,-1)
    lastyearq=paste(format(lastyear, "%Y"), "q", quarter(lastyear), sep="")
    
    data <-data[data$Pdateq>=lastyearq,]
    data <-data[data$totalquarter!="NA",]
    data <- data[order(monthindex)]
    #########################################       
    sel1 <-data[data$TB=="Aktivt TB",]
    sel2 <-data[data$TB!="Aktivt TB",]
    
    
    t1=format(rbind(sel1$totalquarter, sel1$meanquarter),digits = 0)
    t2=format(rbind(sel2$totalquarter, sel2$meanquarter),digits = 0)
    
    table=rbind(t1,t2)
    table[is.na(table)] <-0
    table=format(table,digits=1, decimal.mark=",")
    
    colnames(table)=sort(unique(data$Pdateq))
    names1=c("Aktiv TB", "Aktiv TB","Latent TB","Latent TB")
    names2=c("Total/quarter", "Mean/quarter","Total/quarter","Mean/quarter")
    table=cbind(names1,names2,table)
    colnames(table)=c(" "," ",sort(unique(data$Pdateq)))
    
  } else if (tablenumber==3) {
    data <-data[data$firstyear==2017,]
    data <-data[data$TB=="Aktivt TB",]
    table <- table(data$MDR, data$firstmonth)
    table[is.na(table)] <-0
    table=format(table,digits=1, decimal.mark=",")
    colnames(table)=month.abb[1:12]
    table <- cbind(row.names(table),table)
    colnames(table)=c(" ",colnames(table)[2:length(colnames(table))])
    
  } 
  
  
  return(table)
}

######################################
## 3. create table Pneum
create.table.Pneum <- function(data, tablenumber,subtablenumber) {
  
  if (tablenumber==1) {
    data <-data[data$monthindex>max(dataset$monthindex)-12,]
    if (subtablenumber==1) {
      table=table(data$agegroupsN, data$month)
      colnames(table) <- month.abb[unique(data$Pmnd)]
      rownames(table) <- order.age.groups(data)
      table=rbind(table,apply(table,2,sum))
      table=cbind(table,apply(table,1,sum))
      table[is.na(table)] <-0
      table=format(table,digits=1, decimal.mark=",")
      
      rownames(table)[nrow(table)] <-"Total"
      colnames(table)[ncol(table)] <-"Total"
      table <- cbind(row.names(table),table)
      colnames(table)[1] <-" "
      
    } else  if (subtablenumber==2) {
      data <-data[data$Alaar<=4,]
      table=table(data$Pneusero, data$month)
      table=table[rowSums(table[,1:ncol(table)])>0,]
      colnames(table) <- month.abb[unique(data$Pmnd)]
      table=rbind(table,apply(table,2,sum))
      table=cbind(table,apply(table,1,sum))
      rownames(table)[nrow(table)] <-"Total"
      colnames(table)[ncol(table)] <-"Total"
      table <- cbind(row.names(table),table)
      colnames(table)[1] <-" "
    }
    
  } else if (tablenumber==2) {
    data <- data[data$monthindex>max(dataset$monthindex)-12,]
    
    if (subtablenumber==1) {
      table <- table(data$agegroupsN, data$month)
      colnames(table) <- month.abb[unique(data$Pmnd)]
      rownames(table) <- order.age.groups(data)
      table=rbind(table,apply(table,2,sum))
      table=cbind(table,apply(table,1,sum))
      rownames(table)[nrow(table)] <-"Total"
      colnames(table)[ncol(table)] <-"Total"
      table <- cbind(row.names(table),table)
      colnames(table)[1] <-" "
    }  else  if (subtablenumber==2) {  
      
      data <-data[data$agegroupsN==1,]
      table=table(data$Innlagt, data$month)
      table=table[rowSums(table[,1:ncol(table)])>0,]
      colnames(table) <- month.abb[unique(data$Pmnd)]
      table=rbind(table,apply(table,2,sum))
      table=cbind(table,apply(table,1,sum))
      rownames(table)[nrow(table)] <-"Total"
      colnames(table)[ncol(table)] <-"Total"
      table <- cbind(row.names(table),table)
      colnames(table)[1] <-" "
    }    
  }
  
  return(table)
}

###########################################################
AddTitle <- function(d,title){
  d <- as.data.frame(d)
  for(i in 1:ncol(d)) d[,i] <- as.character(d[,i])
  dtop <- d[1,,drop=F]
  dtop <- rbind(dtop,dtop,dtop)
  for(i in 1:ncol(dtop)){
    dtop[1:2,i] <- ""
    dtop[3,i] <- names(d)[i]
  }
  dtop[1,1] <- title
  return(rbind(dtop,d))
}

###########################################################




###########################################################
savetables <- function(table, folder,g, title, saverow) {
          table <- AddTitle(table,title=title)
            t <- suppressWarnings(today())
            write.table(table, paste(folder,"/",title,"_",t,".csv", sep=""),
            row.names=saverow,
            col.names=F,
            sep=";",
            dec=",",
            qmethod="double", 
            na="0", quote=F)
  
      
  }






