CreateFolders <- function(language, superfolder, yearOfInterest=NULL,seasonOfInterest=NULL){
  folder_year_language <- path(
    type = "year",
    language = language,
    superfolder = superfolder,
    time = yearOfInterest)
  
  folder_season_language <- path(
    type = "season",
    language = language,
    superfolder = superfolder,
    time = seasonOfInterest)
  
  if(!is.null(yearOfInterest)){
    unlink(file.path(folder_year_language,"Tables"),recursive = TRUE, force=TRUE)
    Sys.sleep(1)
    dir.create(file.path(folder_year_language,"Tables"), recursive = T)
    
    unlink(file.path(folder_year_language,"Figures"),recursive = TRUE, force=TRUE)
    Sys.sleep(1)
    dir.create(file.path(folder_year_language,"Figures"), recursive = T)
  }
  if(!is.null(seasonOfInterest)){
    unlink(file.path(folder_season_language,"Figures"),recursive=TRUE, force=TRUE)
    Sys.sleep(1)
    dir.create(file.path(folder_season_language,"Figures"),recursive=TRUE)
  }
}