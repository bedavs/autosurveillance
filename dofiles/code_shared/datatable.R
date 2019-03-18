RecodeDT <- function(d,switch,var){
  if(is.numeric(d[[var]])){
    switch <- data.table::data.table(orig=as.numeric(names(switch)),replace=as.numeric(switch))
  } else {
    switch <- data.table::data.table(orig=names(switch),replace=switch)
  }
  
  txt <- sprintf("d[switch, on=.(%s=orig), %s:=replace]",var,var)
  eval(parse(text=txt))
}

FixNorwegian <- function(d,var){
  txt <- sprintf("d[, %s:=as.character(%s)]",var,var)
  eval(parse(text=txt))
  
  #ae
  txt <- sprintf('d[, %s := gsub("\\xe6","\u01E3",%s)]',var,var)
  eval(parse(text=txt))
  
  #oe
  txt <- sprintf('d[, %s := gsub("\\xf8","\u00F8",%s)]',var,var)
  eval(parse(text=txt))
  
  #aa
  txt <- sprintf('d[, %s := gsub("\\xe5","\u00E5",%s)]',var,var)
  eval(parse(text=txt))
  
  #OE
  txt <- sprintf('d[, %s := gsub("\\xd8","\u00D8",%s)]',var,var)
  eval(parse(text=txt))
}

FixTrondelag <- function(d,var){
  data[get(var)=="S\u00F8r-Tr\u00F8ndelag",(var):="Tr\u00F8ndelag"]
  data[get(var)=="Nord-Tr\u00F8ndelag",(var):="Tr\u00F8ndelag"]
}

AddTitleToDF <- function(d,title=NULL,copyColumnNames=FALSE, columnNames=NULL){
  retval <- copy(d)
  if(copyColumnNames){
    topRow <- d[1]
    for(i in 1:ncol(d)) topRow[[i]] <- names(d)[i]
    retval <- rbindlist(list(topRow,retval))
  }
  if(!is.null(columnNames)){
    topRow <- d[1]
    for(i in 1:ncol(d)) topRow[[i]] <- columnNames[i]
    retval <- rbindlist(list(topRow,retval))
  }
  
  if(!is.null(title)){
    topRow <- retval[1]
    for(i in 1:ncol(d)) topRow[[i]] <- ""
    topRow[[1]] <- title
    retval <- rbindlist(list(topRow,retval))
  }
  
  return(retval)
}