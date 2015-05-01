removeUnUsedColumns <- function(data){
  columnsToSelect <- names(a) %in%  c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "HTHG", "HTAG", "Attendance", 
                                     "Referee", "HS" ,"AS" ,"HST" ,"AST" ,"HHW" ,"AHW" ,"HC" ,"AC" ,"HF","AF","HO" ,"AO" ,"HY","AY","HR" ,"AR")
  data <- data[, columnsToSelect]
  return(data)
}