Titles <- function(LANGUAGE,SUPERFOLDER,TITLE_MAIN,TITLE_Y,substitutions_y=NULL){
  if(SUPERFOLDER %in% "ALL_WITHOUT_TITLES"){
    TEMP_TITLE <- NULL
    TEMP_TITLE_Y <- TITLE_MAIN[[LANGUAGE]]
  } else {
    TEMP_TITLE <- TITLE_MAIN[[LANGUAGE]]
    TEMP_TITLE_Y <- TITLE_Y[[LANGUAGE]]
  }
  if(!is.null(substitutions_y)) TEMP_TITLE_Y <- gsub(substitutions_y[1],substitutions_y[2],TEMP_TITLE_Y)
  return(list(
    "TITLE"=TEMP_TITLE,
    "TITLE_Y"=TEMP_TITLE_Y
    ))
}