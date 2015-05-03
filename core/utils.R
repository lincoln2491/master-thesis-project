getSeasonsName <- function(seasonStartYear){
  endYear = seasonStartYear + 1
  if(nchar(as.character(seasonStartYear)) < 2){
    seasonStartYear = paste("0", seasonStartYear, sep = "")
  } 
  if(nchar(as.character(seasonStartYear)) > 2){
    seasonStartYear = str_sub(seasonStartYear, start = -2)
  } 
  
  if(nchar(as.character(endYear)) < 2){
    endYear = paste("0", endYear, sep = "")
  }
  if(nchar(as.character(endYear)) > 2){
    endYear = str_sub(endYear, start = -2)
  } 
  
  return(c(as.character(seasonStartYear), as.character(endYear)))
}
