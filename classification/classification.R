source("core/utils.R")
source("core/databaseConnector.R")

getData <- function(){
  tmp = readTableFromMatches()
  tmp = getFromSeasonRange(tmp, 2005)
  tmp = removeColumnsWhereAnyIsNa(tmp)
  tmp$league_fk = sapply(tmp$league_fk, getLeagueNameById)
  return(tmp)
}


prepareDataForClassification <- function(data){
  newData = data.frame(  data$idMatch )

  #newData$league_fk = as.factor(data$league_fk)  
  newData$season_fk = as.factor(data$season_fk) 
  newData$home_team_fk = as.factor(data$home_team_fk) 
  newData$away_team_fk = as.factor(data$away_team_fk) 
  newData$year = as.factor(data$year) 
  newData$month = as.factor(data$month) 
  newData$day = as.factor(data$day) 
  newData$result = as.factor(data$result) 
  
  newData$avHG10 = sapply(tmp$idMatch, function(x) getMean(x, data, "home_goals"))
  newData$avAG10 = sapply(tmp$idMatch, function(x) getMean(x, data, "away_goals", forWho = "away"))
  
  return(newData)
}