ShortenYear <- function(s){
  return(substr(s,3,4))
}

ShortenSeason <- function(s){
  return(sprintf("%s/%s",substr(s,1,4),substr(s,8,9)))
}