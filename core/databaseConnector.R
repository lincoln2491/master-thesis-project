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


#TODO do better
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
  query = paste("SELECT idLeague FROM Leagues WHERE nameInFootballData = \"", leagueName, "\";", sep = "")
  result = dbSendQuery(connection, query)
  id = fetch(result)[1,1]
  dbClearResult(result)
  return(id)
}

getCountryLeague <-function(leagueName){
  connection = createConnection()
  query = paste("SELECT country FROM Leagues WHERE nameInFootballData = \"", leagueName, "\";", sep = "")
  result = dbSendQuery(connection, query)
  id = fetch(result)[1,1]
  dbClearResult(result)
  return(id)
}

getClubId <-function(nameInFootballData){
  connection = createConnection()
  query = paste("SELECT idClubs FROM Clubs WHERE name_in_football_data = \"", nameInFootballData, "\";", sep = "")
  result = dbSendQuery(connection, query)
  id = fetch(result)[1,1]
  dbClearResult(result)
  return(id)
}

getRefereeId <-function(nameInFootballData){
  connection = createConnection()
  query = paste("SELECT idReferees FROM Referees WHERE name_in_football_data = \"", nameInFootballData, "\";", sep = "")
  result = dbSendQuery(connection, query)
  id = fetch(result)[1,1]
  dbClearResult(result)
  return(id)
}