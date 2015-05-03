library("RMySQL")
library("stringr")
source("core/utils.R")

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

getIdForSeason <-function(seasonStartYear){
  connection = createConnection()
  tmp = getSeasonsName(seasonStartYear)
  seasonStartYear = tmp[1]
  seasonEndYear = tmp[2]
  
  seasonName = paste(seasonStartYear, "/", seasonEndYear , sep = "")
  query = paste("SELECT idSeasons FROM Seasons WHERE name = \"", seasonName, "\";", sep = "")
  result = dbSendQuery(connection, query)
  id = fetch(result)[1,1]
  dbClearResult(result)
  return(id)
}

getIdForLeague <-function(leagueName){
  connection = createConnection()
  query = paste("SELECT idLeague FROM Leagues WHERE nameInFootbalData = \"", leagueName, "\";", sep = "")
  result = dbSendQuery(connection, query)
  id = fetch(result)[1,1]
  dbClearResult(result)
  return(id)
}