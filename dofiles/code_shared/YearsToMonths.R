
YearToMonthsStart <- function(x){
  return(x*12)
}

YearToMonthsEnd <- function(x){
  return(x*12-1)
}

SeasonList <- function(minSeason="2000/2001", maxSeason=TODAYS_SEASON){
  minYear <- as.numeric(substr(minSeason,1,4))
  maxYear <- as.numeric(substr(maxSeason,1,4))
  retval <- c(minYear:maxYear)
  retval <- sprintf("%s/%s",retval,retval+1)
  return(retval)
}