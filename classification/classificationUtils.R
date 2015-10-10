source("core/utils.R")
source("core/databaseConnector.R")
library("data.table")

getData <- function(){
  results = readTableFromMatches()
  
  results$date = results$match_date
  
  seasons = readDataFromDatabase("Seasons")
  results$seasonStart = with(seasons, startDate[match(results$season_fk, idSeasons)])
  results$seasonEnd = with(seasons, endDate[match(results$season_fk, idSeasons)])
  results$date = as.Date(results$date)
  results$seasonStart = as.Date(results$seasonStart)
  results$seasonEnd = as.Date(results$seasonEnd)
  results$day_of_season = results$date - results$seasonStart
  
  #results$date = NULL
  results$seasonStart = NULL
  results$seasonEnd = NULL
  
  results = separate(data = results, col = match_date, into = c("year", "month", "day"), sep = "-")
  results = results[ results$league_fk ==2 & results$season_fk >= 8, ]
  results = results[order(results$year, results$month, results$day),]
  results = Filter(function(x) !any(is.na(x)), results)
  rownames(results) = 1:nrow(results)
  results$idMatch = 1:nrow(results)
  
  
  
  for(i in 8:22){
    tmp = results[ results$season_fk ==i,]
    tmp = addTablePlace(tmp)
    results$home_pos[ results$season_fk ==i] = tmp$home_pos
    results$away_pos[ results$season_fk ==i] = tmp$away_pos
  }
  results$season_fk <- with(seasons,  name[match(results$season_fk, idSeasons)])
  results$result = factor(results$result, levels = c("H", "D", "A"))
  
  return(results)
}


prepareDataForClassification <- function(data){
  newData = data.frame(  data$idMatch )
  setnames(newData, "data.idMatch", "idMatch")
  newData$season_fk = as.factor(data$season_fk) 
  newData$home_team_fk = as.factor(data$home_team_fk) 
  newData$away_team_fk = as.factor(data$away_team_fk)
    
  newData$home_pos = as.integer(data$home_pos) 
  newData$away_pos = as.integer(data$away_pos)
  
  newData$home_couch_fk = as.factor(data$home_couch_fk) 
  newData$away_couch_fk = as.factor(data$away_couch_fk)
  
  newData$day_of_season = as.integer(data$day_of_season)
  newData$date = data$date
  newData$year = as.integer(data$year) 
  newData$month = as.integer(data$month) 
  newData$day = as.integer(data$day) 
  attributes = c("goals", "goals_half_time", "shots", "shots_on_target",
               "corners", "fouls", "yellows", "reds")
  
  for(attr in attributes){
    attrName = paste("home_", attr,"_av10", sep="")
    newData[[attrName]] = as.numeric(sapply(data$idMatch, function(x) getMean(x, data, attr)))
    attrName2 = paste("away_", attr,"_av10", sep="")
    newData[[attrName2]] = as.numeric(sapply(data$idMatch, function(x) getMean(x, data, attr, forWho = "away")))
    attrName3 = paste("diff_", attr,"_av10", sep="")
    newData[[attrName3]] = as.numeric(newData[[attrName]] - newData[[attrName2]])
  }
  
  newData$result = as.factor(data$result) 
  
  #newData$idMatch = NULL
  
  newData$month = newData$month - 7
  newData$month[newData$month < 0] = newData$month[newData$month < 0] + 12
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
  if(forWho == "home"){
    data = data[data$home_team_fk == clubId,]
  }
  else if(forWho == "away"){
    data = data[data$away_team_fk == clubId,]
  }
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

addTablePlace <-function(data){
  data$home_pos = 0
  data$away_pos = 0

  leagueTable = data.table(pos = 1:20, team = unique(data$home_team_fk), p =0, win = 0,
                           draw = 0, lose = 0, gf = 0, ga = 0, gd = 0, point = 0)
  leagueTable = leagueTable[order(leagueTable$team),]  
  leagueTable$pos = 1:20
  lastDay = data$day[1]
  matchesToCount = c()
  
  for(i in data$idMatch){
    match = data[ data$idMatch == i,]
    if(match$day == lastDay){
      matchesToCount = c(matchesToCount, list(match))
    }
    else{
      #TODO do it better (export to other function)
      leagueTable = updateLeagueTable(leagueTable, matchesToCount)
      
      matchesToCount = c(list(match))
      lastDay = match$day
      
    }
    
    
    data$home_pos[ data$idMatch == i] = leagueTable$pos[ leagueTable$team == match$home_team_fk]
    data$away_pos[ data$idMatch == i] = leagueTable$pos[ leagueTable$team == match$away_team_fk]
  }
  leagueTable = updateLeagueTable(leagueTable, matchesToCount)
  
  return(data)
}


updateLeagueTable <-function(leagueTable, matchesToCount){
  for(savedMatch in matchesToCount){
    leagueTable$p[ leagueTable$team == savedMatch$home_team_fk] = leagueTable$p[ leagueTable$team == savedMatch$home_team_fk] + 1
    leagueTable$p[ leagueTable$team == savedMatch$away_team_fk] = leagueTable$p[ leagueTable$team == savedMatch$away_team_fk] + 1
    
    leagueTable$gf[ leagueTable$team == savedMatch$home_team_fk] = leagueTable$gf[ leagueTable$team == savedMatch$home_team_fk] + savedMatch$home_goals
    leagueTable$gf[ leagueTable$team == savedMatch$away_team_fk] = leagueTable$gf[ leagueTable$team == savedMatch$away_team_fk] + savedMatch$away_goals
    leagueTable$ga[ leagueTable$team == savedMatch$home_team_fk] = leagueTable$ga[ leagueTable$team == savedMatch$home_team_fk] + savedMatch$away_goals
    leagueTable$ga[ leagueTable$team == savedMatch$away_team_fk] = leagueTable$ga[ leagueTable$team == savedMatch$away_team_fk] + savedMatch$home_goals
    
    if(savedMatch$result == "H"){
      leagueTable$win[ leagueTable$team == savedMatch$home_team_fk] = leagueTable$win[ leagueTable$team == savedMatch$home_team_fk] + 1
      leagueTable$lose[ leagueTable$team == savedMatch$away_team_fk] = leagueTable$lose[ leagueTable$team == savedMatch$away_team_fk] + 1
      leagueTable$point[ leagueTable$team == savedMatch$home_team_fk] = leagueTable$point[ leagueTable$team == savedMatch$home_team_fk] + 3
    }
    else if(savedMatch$result == "A"){
      leagueTable$win[ leagueTable$team == savedMatch$away_team_fk] = leagueTable$win[ leagueTable$team == savedMatch$away_team_fk] + 1
      leagueTable$lose[ leagueTable$team == savedMatch$home_team_fk] = leagueTable$lose[ leagueTable$team == savedMatch$home_team_fk] + 1
      leagueTable$point[ leagueTable$team == savedMatch$away_team_fk] = leagueTable$point[ leagueTable$team == savedMatch$away_team_fk] + 3
    }
    else{
      leagueTable$draw[ leagueTable$team == savedMatch$away_team_fk] = leagueTable$draw[ leagueTable$team == savedMatch$away_team_fk] + 1
      leagueTable$draw[ leagueTable$team == savedMatch$home_team_fk] = leagueTable$draw[ leagueTable$team == savedMatch$home_team_fk] + 1
      leagueTable$point[ leagueTable$team == savedMatch$home_team_fk] = leagueTable$point[ leagueTable$team == savedMatch$home_team_fk] + 1
      leagueTable$point[ leagueTable$team == savedMatch$away_team_fk] = leagueTable$point[ leagueTable$team == savedMatch$away_team_fk] + 1
    }
  }
  leagueTable$gd =leagueTable$gf - leagueTable$ga 
  leagueTable = leagueTable[order(leagueTable$point, leagueTable$gd, decreasing = TRUE),] 
  leagueTable$pos = 1:20
  return(leagueTable)
}


jaccardIndex <-function(vec1, vec2){
  n1 = length(vec1)
  n2 = length(vec2)
  ni = length(intersect(vec1, vec2))
  return(ni/(n1 + n2 - ni))
}

lengthOfIntersect <- function(vec1, vec2){
  ni = length(intersect(vec1, vec2))
  return(ni)
}
