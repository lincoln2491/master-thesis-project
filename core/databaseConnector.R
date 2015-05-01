library("RMySQL")
library("stringr")

createConnection <- function(){
  if(! exists("connection") ){
    config =  read.table("config.txt")
    user = as.character(  config[1,2])
    database = as.character(  config[2,2])
    host= as.character(  config[3,2])
    connection <<- dbConnect(MySQL(), user = user, dbname = database, host = host)
  }
  return(connection)
}

writeDataToDatabase <- function(tableName, data){
  connection = createConnection()
  dbWriteTable(connection, tableName ,value = data, append = TRUE, row.names = FALSE)
}

getIdForSeason <-function(startYear){
  connection = createConnection()
  endYear = startYear + 1
  if(nchar(as.character(startYear)) < 2){
    startYear = paste("0", startYear, sep = "")
  } 
  if(nchar(as.character(endYear)) < 2){
    endYear = paste("0", endYear, sep = "")
  }
  if(nchar(as.character(endYear)) > 2){
    endYear = str_sub(endYear, start = -2)
  } 
  
  seasonName = paste(startYear, "/", endYear , sep = "")
  query = paste("SELECT idSeasons FROM Seasons WHERE name = \"", seasonName, "\";", sep = "")
  result = dbSendQuery(connection, query)
  return(fetch(result)[1,1])
}