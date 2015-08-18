source("core/utils.R")
source("core/databaseConnector.R")

getData <- function(){
  results = readTableFromMatches()
  results = results[ results$league_fk ==2 & results$season_fk >= 8, ]
  results = results[order(results$year, results$month, results$day),]
  results = Filter(function(x) !any(is.na(x)), results)
  rownames(results) = 1:nrow(matches)
  results$idMatch = 1:nrow(matches)
  return(results)
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


getPreviosuMatchesOfTeam <- function(idMatch, data, howManyPreviousMatches = 10, forWho = "home"){
  thisMatch = data[ data$idMatch == idMatch,]
  matchId = thisMatch$idMatch
  clubId = NA
  
  if(forWho == "home"){
    clubId = thisMatch$home_team_fk
  }
  else if(forWho == "away"){
    clubId = thisMatch$away_team_fk
  }else{
    stop("You must specify fow who I should searching")
  }
  
  data = data[ data$idMatch < matchId,]
  data = data[data$home_team_fk == clubId | data$away_team_fk == clubId,]
  
  return(tail(data, howManyPreviousMatches))
}


getMean <- function(id, data, columnName, howManyPreviousMatches = 10, forWho = "home"){
  thisMatch = data[ data$idMatch == id,]
  matchId = thisMatch$idMatch
  clubId = NA
  
  if(forWho == "home"){
    clubId = thisMatch$home_team_fk
  }
  else if(forWho == "away"){
    clubId = thisMatch$away_team_fk
  }else{
    stop("You must specify fow who I should searching")
  }
  
  tmp = getPreviosuMatchesOfTeam(id, data, howManyPreviousMatches, forWho)
  home = tmp[tmp$home_team_fk == clubId,] 
  away = tmp[tmp$away_team_fk == clubId,] 
  
  homeName = paste("home_", columnName, sep = "")
  awayName = paste("away_", columnName, sep = "")
  values = c(home[[homeName]], away[[awayName]])
  return(mean(values))
}