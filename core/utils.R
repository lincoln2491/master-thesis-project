source("core/databaseConnector.R")


getSeasonsName <- function(seasonStartYear) {
    endYear = seasonStartYear + 1
    if (nchar(as.character(seasonStartYear)) < 2) {
        seasonStartYear = paste("0", seasonStartYear, sep = "")
    }
    if (nchar(as.character(seasonStartYear)) > 2) {
        seasonStartYear = str_sub(seasonStartYear, start = -2)
    }
    
    if (nchar(as.character(endYear)) < 2) {
        endYear = paste("0", endYear, sep = "")
    }
    if (nchar(as.character(endYear)) > 2) {
        endYear = str_sub(endYear, start = -2)
    }
    
    return(c(as.character(seasonStartYear), as.character(endYear)))
} 

getResult <- function(home_goals, away_goals){ 
  if (home_goals > away_goals){
    return("H")}
  else if (home_goals < away_goals){
    return("A") 
  } 
  else{
    return("D")   
  }  
}
  
getFromSeasonRange <- function(data, seasonStartYearFor, seasonStartYearTo){
  if(!missing(seasonStartYearFor) && !is.na(seasonStartYearFor)){
    tmp = getSeasonsName(seasonStartYearFor)
    seasonStartYearFor = paste(tmp[1], "/", tmp[2], sep = "")
    seasonStartYearFor = getSeasonIdByName (seasonStartYearFor)
    data = data[data$season_fk >= seasonStartYearFor,]
  }
  
  if(!missing(seasonStartYearTo) && !is.na(seasonStartYearTo)){
    tmp = getSeasonsName(seasonStartYearTo)
    seasonStartYearTo = paste(tmp[1], "/", tmp[2], sep = "")
    seasonStartYearTo = getSeasonIdByName (seasonStartYearTo)
    data = data[data$season_fk <= seasonStartYearTo,]
  }
  
  return(data)
}



readTableFromMatches <- function(){
  results = readDataFromDatabase("Matches")
  clubs = readDataFromDatabase("Clubs")
  results$home_team_fk <- with(clubs,  name[match(results$home_team_fk, idClubs)])
  results$away_team_fk <- with(clubs,  name[match(results$away_team_fk, idClubs)])
  results = separate(data = results, col = match_date, into = c("year", "month", "day"), sep = "-")
  results = Filter(function(x) !all(is.na(x)), results)
  results$result =  as.factor(mapply(getResult, results$home_goals, results$away_goals))
  return(results);
}

removeColumnsWhereAllIsNa <-function(data){
  data = Filter(function(x) !all(is.na(x)), data)
  return(data)
}

removeColumnsWhereAnyIsNa <-function(data){
  data = Filter(function(x) !any(is.na(x)), data)
  return(data)
}

getData <- function(){
  tmp = readTableFromMatches()
  tmp = getFromSeasonRange(tmp, 2005)
  tmp = removeColumnsWhereAnyIsNa(tmp)
  tmp$league_fk = sapply(tmp$league_fk, getLeagueNameById)
  return(tmp)
}


