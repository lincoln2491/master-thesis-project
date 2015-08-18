library("stringr")
library("dplyr")
library("hash")
library("XML")
source("core/utils.R")
source("core/databaseConnector.R")


apiKey = "QULJJPHESUGHJJENBKKKDFOKSUZYXMMADREQVDHJEUZYDBSBMR"
urlBase = "http://www.xmlsoccer.com/FootballData.asmx/"

getMatchExtensionString <- function(seasonString, leagueString){
  resultString = paste("GetHistoricMatchesByLeagueAndSeason?ApiKey=", apiKey, 
                       "&league=", leagueString, "&seasonDateString=", seasonString, sep = "")
  return(resultString)
}


readXMLFromXMLSoccer <- function(seasonString, leagueString){
  url = paste(urlBase, getMatchExtensionString(seasonString, leagueString), sep = "")
  print(url)
  doc = xmlTreeParse(url, useInternalNodes = TRUE)
  return(doc)
}



readXMLFromXMLFileSystem <-function(seasonString, leagueString){
  fileName = paste("xmlsoccer_data/", leagueString, seasonString, ".xml", sep = "")
  doc = xmlTreeParse(fileName)
  return(doc)
}

parseXMLDocument <-function(document){
  rootNode = xmlRoot(document)
  nodes = getNodeSet(rootNode, "//Match")
  nodes = lapply(nodes, extractInformation)
  data = nodes[[1]][FALSE,]
  for(tmp in nodes){
    data = rbind(data, tmp)
  }
  return(data)
}

extractInformation <- function(match){
  #match = match[[1]]
  result = c()
  result$xmlSoccerId =  xmlValue(xmlChildren(match)["Id"][[1]])
  result$fixtureMatch_Id = xmlValue(xmlChildren(match)["FixtureMatch_Id"][[1]])
  
  result$Date = xmlValue(xmlChildren(match)["Date"][[1]])
  result$Round = xmlValue(xmlChildren(match)["Round"][[1]])
#   result$Spectators = xmlValue(xmlChildren(match)["Spectators"][[1]])
  result$League = xmlValue(xmlChildren(match)["League"][[1]])
  
    result$HomeTeam = xmlValue(xmlChildren(match)["HomeTeam"][[1]])
    result$AwayTeam = xmlValue(xmlChildren(match)["AwayTeam"][[1]])
    result$HomeTeam_Id = xmlValue(xmlChildren(match)["HomeTeam_Id"][[1]])
    result$AwayTeam_Id = xmlValue(xmlChildren(match)["AwayTeam_Id"][[1]])
  
    result$HomeGoals = xmlValue(xmlChildren(match)["HomeGoals"][[1]])
    result$AwayGoals = xmlValue(xmlChildren(match)["AwayGoals"][[1]])
    result$HalfTimeHomeGoals = xmlValue(xmlChildren(match)["HalfTimeHomeGoals"][[1]])
    result$HalfTimeAwayGoals = xmlValue(xmlChildren(match)["HalfTimeAwayGoals"][[1]])
#     
#     result$HomeShots = xmlValue(xmlChildren(match)["HomeShots"][[1]])
#     result$AwayShots = xmlValue(xmlChildren(match)["AwayShots"][[1]])
#     result$HomeCorners = xmlValue(xmlChildren(match)["HomeCorners"][[1]])
#     result$AwayCorners = xmlValue(xmlChildren(match)["AwayCorners"][[1]])
#     result$HomeShotsOnTarget = xmlValue(xmlChildren(match)["HomeShotsOnTarget"][[1]])
#     result$AwayShotsOnTarget = xmlValue(xmlChildren(match)["AwayShotsOnTarget"][[1]])
#     result$HomeFouls = xmlValue(xmlChildren(match)["HomeFouls"][[1]])
#     result$AwayFouls = xmlValue(xmlChildren(match)["AwayFouls"][[1]])
#     result$HomeYellowCards = xmlValue(xmlChildren(match)["HomeYellowCards"][[1]])
#     result$AwayYellowCards = xmlValue(xmlChildren(match)["AwayYellowCards"][[1]])
#     result$HomeRedCards = xmlValue(xmlChildren(match)["HomeRedCards"][[1]])
#     result$AwayRedCards = xmlValue(xmlChildren(match)["AwayRedCards"][[1]])
    
#     result$HomeTeamYellowCardDetails = xmlValue(xmlChildren(match)["HomeTeamYellowCardDetails"][[1]])
#     result$AwayTeamYellowCardDetails = xmlValue(xmlChildren(match)["AwayTeamYellowCardDetails"][[1]])
#     result$HomeTeamRedCardDetails = xmlValue(xmlChildren(match)["HomeTeamRedCardDetails"][[1]])
#     result$AwayTeamRedCardDetails = xmlValue(xmlChildren(match)["AwayTeamRedCardDetails"][[1]])
#     
#     result$HomeGoalDetails = xmlValue(xmlChildren(match)["HomeGoalDetails"][[1]])
#     result$AwayGoalDetails = xmlValue(xmlChildren(match)["AwayGoalDetails"][[1]])
# 
    result$HomeLineupCoach = xmlValue(xmlChildren(match)["HomeLineupCoach"][[1]])
    result$AwayLineupCoach = xmlValue(xmlChildren(match)["AwayLineupCoach"][[1]])
#     
#     result$HomeTeamFormation = xmlValue(xmlChildren(match)["HomeTeamFormation"][[1]])
#     result$AwayTeamFormation = xmlValue(xmlChildren(match)["AwayTeamFormation"][[1]])
#     
#     result$HomeLineupGoalkeeper = xmlValue(xmlChildren(match)["HomeLineupGoalkeeper"][[1]])
#     result$AwayLineupGoalkeeper = xmlValue(xmlChildren(match)["AwayLineupGoalkeeper"][[1]])
#     result$HomeLineupDefense = xmlValue(xmlChildren(match)["HomeLineupDefense"][[1]])
#     result$AwayLineupDefense = xmlValue(xmlChildren(match)["AwayLineupDefense"][[1]])
#     result$HomeLineupMidfield = xmlValue(xmlChildren(match)["HomeLineupMidfield"][[1]])
#     result$AwayLineupMidfield = xmlValue(xmlChildren(match)["AwayLineupMidfield"][[1]])
#     result$HomeLineupForward = xmlValue(xmlChildren(match)["HomeLineupForward"][[1]])
#     result$AwayLineupForward = xmlValue(xmlChildren(match)["AwayLineupForward"][[1]])
#     
#     result$HomeLineupSubstitutes = xmlValue(xmlChildren(match)["HomeLineupSubstitutes"][[1]])
#     result$AwayLineupSubstitutes = xmlValue(xmlChildren(match)["AwayLineupSubstitutes"][[1]])
#     result$HomeSubDetails = xmlValue(xmlChildren(match)["HomeSubDetails"][[1]])
#     result$AwaySubDetails = xmlValue(xmlChildren(match)["AwaySubDetails"][[1]])
#   
  result = data.frame(result)
  return(result)
}

#TODO better join
updateClubs <- function(matches){
  clubs = readDataFromDatabase("Clubs")
  posClubs = matches[, c("HomeTeam", "HomeTeam_Id")]
  posClubs = unique(posClubs)
  posClubs$HomeTeam_Id = as.integer(as.character(posClubs$HomeTeam_Id)) 
  posClubs$HomeTeam = as.character(posClubs$HomeTeam)
  
  clubs$id_in_xml_soccer=  apply(clubs, 1, 
                                 function(x) x$id_in_xml_soccer = 
                                   ifelse(x["name_in_football_data"] %in% posClubs$HomeTeam, posClubs$HomeTeam_Id[x["name_in_football_data"] == posClubs$HomeTeam], NA))
  
  clubs$name_in_xml_soccer=  apply(clubs, 1, 
                                 function(x) x$name_in_xml_soccer = 
                                   ifelse(x["id_in_xml_soccer"] %in% posClubs$HomeTeam_Id, posClubs$HomeTeam[x["id_in_xml_soccer"] == posClubs$HomeTeam_Id], NA))
  
  
  return(clubs)
}

mergeWithDatabase <-function(matchesFromDatabase, matches, clubs){
  
  
}

fillHomeTeam <- function(x){
  id = x["HomeTeam_Id"]
  newId = clubs$idClubs[ clubs$id_in_xml_soccer == id]
  return(newId)
}

fillAwayTeam <- function(x){
  id = x["AwayTeam_Id"]
  newId = clubs$idClubs[ clubs$id_in_xml_soccer == id]
  return(newId)
}