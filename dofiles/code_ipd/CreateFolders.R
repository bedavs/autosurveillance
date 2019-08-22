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
    Sys.sleep(1)
    
    dir.create(file.path(folder_year_language),recursive=TRUE)
    
    DeleteAndCreateFolder(file.path(folder_year_language, "Tables"))
    
    if(!superfolder %in% c("SHAREPOINT")){
      DeleteAndCreateFolder(file.path(
        folder_year_language,
        "Figures_serotype_funnel_singleyearlast"
      ))
      
      DeleteAndCreateFolder(file.path(
        folder_year_language,
        "Figures_serotype_funnel_4yearbaseline"
      ))
      
      DeleteAndCreateFolder(file.path(
        folder_year_language,
        "Figures_serotype_diversity"
      ))
    }
    
    DeleteAndCreateFolder(file.path(
      folder_year_language,
      "Figures_incidence_vax"
    ))
    
    DeleteAndCreateFolder(file.path(
      folder_year_language,
      "Figures_incidence_age"
    ))
    
  } else if(!is.null(seasonOfInterest)){
    dir.create(file.path(folder_season_language),recursive=TRUE)
    
    DeleteAndCreateFolder(file.path(
      folder_season_language,
      "Figures_cumulative"
    ))
    
    if(!superfolder %in% c("SHAREPOINT")){
      DeleteAndCreateFolder(file.path(
        folder_season_language,
        "Figures_serotype_funnel_singleyearlast"
      ))
      
      DeleteAndCreateFolder(file.path(
        folder_season_language,
        "Figures_serotype_funnel_4yearbaseline"
      ))
    }
  }
}