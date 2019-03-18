CreateFolders <- function(SUPERFOLDER, FOLDERS,yearOfInterest=NULL,seasonOfInterest=NULL,LANGUAGE){
  BASE_FOLDER <- BaseFolder(SUPERFOLDER = SUPERFOLDER, FOLDERS = FOLDERS)
  
  if(!is.null(yearOfInterest)){
    Sys.sleep(1)
    dir.create(file.path(BASE_FOLDER,yearOfInterest),recursive=TRUE)
    
    DeleteAndCreateFolder(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE))
    DeleteAndCreateFolder(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Tables"))
    
    
    if(!SUPERFOLDER %in% c("SHAREPOINT")){
      DeleteAndCreateFolder(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Figures_serotype_funnel_singleyearlast"))
      DeleteAndCreateFolder(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Figures_serotype_funnel_4yearbaseline"))
      DeleteAndCreateFolder(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Figures_serotype_diversity"))
    }
    
    DeleteAndCreateFolder(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Figures_incidence_vax"))
    DeleteAndCreateFolder(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Figures_incidence_age"))
    
  } else if(!is.null(seasonOfInterest)){
    DeleteAndCreateFolder(file.path(BASE_FOLDER,gsub("/","_",seasonOfInterest),LANGUAGE,"Figures_cumulative"))
    
    if(!SUPERFOLDER %in% c("SHAREPOINT")){
      DeleteAndCreateFolder(file.path(BASE_FOLDER,gsub("/","_",seasonOfInterest),LANGUAGE,"Figures_serotype_funnel_singleyearlast"))
      DeleteAndCreateFolder(file.path(BASE_FOLDER,gsub("/","_",seasonOfInterest),LANGUAGE,"Figures_serotype_funnel_4yearbaseline"))
    }
  }
}