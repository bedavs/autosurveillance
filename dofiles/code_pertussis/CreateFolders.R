CreateFolders <- function(SUPERFOLDER, FOLDERS,yearOfInterest=NULL,seasonOfInterest=NULL,LANGUAGE){
  BASE_FOLDER <- BaseFolder(SUPERFOLDER = SUPERFOLDER, FOLDERS = FOLDERS)
  
  if(!is.null(yearOfInterest)){
    Sys.sleep(1)
    dir.create(file.path(BASE_FOLDER,yearOfInterest),recursive=TRUE)
    
    unlink(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE),recursive = TRUE, force=TRUE)
    Sys.sleep(1)
    dir.create(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE),recursive=TRUE)
    
    unlink(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Tables"),recursive = TRUE, force=TRUE)
    Sys.sleep(1)
    dir.create(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Tables"))
    
    unlink(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Figures"),recursive = TRUE, force=TRUE)
    Sys.sleep(1)
    dir.create(file.path(BASE_FOLDER,yearOfInterest,LANGUAGE,"Figures"))
  }
  if(!is.null(seasonOfInterest)){
    Sys.sleep(1)
    dir.create(file.path(BASE_FOLDER,gsub("/","_",seasonOfInterest)),recursive=TRUE)
    
    unlink(file.path(BASE_FOLDER,gsub("/","_",seasonOfInterest),LANGUAGE),recursive=TRUE, force=TRUE)
    Sys.sleep(1)
    dir.create(file.path(BASE_FOLDER,gsub("/","_",seasonOfInterest),LANGUAGE),recursive=TRUE)
    
    unlink(file.path(BASE_FOLDER,gsub("/","_",seasonOfInterest),LANGUAGE,"Figures"),recursive=TRUE, force=TRUE)
    Sys.sleep(1)
    dir.create(file.path(BASE_FOLDER,gsub("/","_",seasonOfInterest),LANGUAGE,"Figures"),recursive=TRUE)
  }
}