CleanRawNumbers <- function(masterData, ageDef, vaxDef){
  data <- vector("list",length=length(vaxDef))
  for(i in names(vaxDef)) data[[i]] <- copy(masterData)
  data <- rbindlist(data,idcol="vaccine")
  
  data[,keep:=FALSE]
  for(i in names(vaxDef)) data[vaccine==i & Pneusero %in% vaxDef[[i]], keep:=TRUE]
  data <- data[keep==TRUE]
  data[,keep:=NULL]
  
  masterData <- copy(data)
  
  
  data <- vector("list",length=length(ageDef))
  for(i in names(ageDef)) data[[i]] <- copy(masterData)
  data <- rbindlist(data,idcol="age")
  data[,yr_wk:=as.numeric(format.Date(Pdato,"%G"))]
  data[,wk:=as.numeric(format.Date(Pdato,"%V"))]
  
  data[,keep:=FALSE]
  for(i in names(ageDef)) data[age==i & Alaar %in% ageDef[[i]], keep:=TRUE]
  data <- data[keep==TRUE]
  data[,keep:=NULL]
  #data[,season:=sprintf("%s/%s",Paar,Paar+1)]
  #data[Pmnd<=6,season:=sprintf("%s/%s",Paar-1,Paar)]
  
  # FIXING POPULATION
  
  
  rawNumbersYear <- data[,.(num=.N),by=.(age,Paar,vaccine)]
  rawNumbersWeek <- data[,.(num=.N),by=.(age,yr_wk,wk,vaccine)]
  rawNumbersYear[,time:="year"]
  rawNumbersWeek[,time:="week"]
  
  rawNumbersYear[,week:=as.numeric(NA)]
  
  setnames(rawNumbersYear,c("age","year","vaccine","num","time","week"))
  setnames(rawNumbersWeek,c("age","year","week","vaccine","num","time"))
  
  rawNumbers <- rbind(rawNumbersWeek,rawNumbersYear[,names(rawNumbersWeek),with=F])
  
  
  skeleton <- data.table(expand.grid(age=unique(rawNumbers$age),
                                     year=1976:TODAYS_YEAR,
                                     week=c(NA,1:53),
                                     vaccine=unique(rawNumbers$vaccine),
                                     time=c("week","year"),
                                     stringsAsFactors = FALSE))
  skeleton <- skeleton[(time=="week" & !is.na(week)) | (time=="year" & is.na(week))]
  skeleton <- skeleton[!(year==TODAYS_YEAR & week>TODAYS_WEEK & time=="week")]
  rawNumbers <- merge(skeleton, rawNumbers, by=c("age","year","week","vaccine","time"), all.x=TRUE)
  rawNumbers[is.na(num), num:=0]
  
  rawNumbers[time=="week",season:=sprintf("%s/%s",year,year+1)]
  rawNumbers[time=="week" & week<=26,season:=sprintf("%s/%s",year-1,year)]
  
  rawNumbers[!is.na(week),scum_num:=cumsum(num),by=.(vaccine,age,season)]
  rawNumbers[!is.na(week),s_week:=(week+26)%%53+1]
  
  rawNumbers[,num_all:=as.numeric(NA)]
  rawNumbers[vaccine=="All IPD",num_all:=as.numeric(num)]
  rawNumbers[,num_all:=sum(num_all,na.rm=T),by=.(age,year,week,time)]
  
  return(rawNumbers)
}
