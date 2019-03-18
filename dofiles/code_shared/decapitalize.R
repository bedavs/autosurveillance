Decapitalize <- function(string){
  capped <- grep("^[A-Z]", string, invert = F)
  substr(string[capped], 1, 1) <- tolower(substr(string[capped], 
                                                 1, 1))
  string <- gsub("^pCV","PCV",string)
  string <- gsub("^pPV","PPV",string)
  string <- gsub("^nVT","NVT",string)
  return(string)
}