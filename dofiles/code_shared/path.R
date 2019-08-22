path <- function(type="year",language, superfolder, time, ...){
  stopifnot(type %in% c("year","season"))
  
  f <- NULL
  if(type=="year"){
    f <- file.path(
      org::PROJ$SHARED_TODAY,
      language,
      superfolder,
      time,
      ...
    )
  } else if(type=="season"){
    f <- file.path(
      org::PROJ$SHARED_TODAY,
      language,
      superfolder,
      gsub("/","_",time),
      ...
    )
  }
  
  return(f)
}