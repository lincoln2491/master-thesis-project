library("stringr")
library("plyr")
library("hash")
source("core/utils.R")
source("core/databaseConnector.R")

# add 'Referee'
removeUnUsedColumns <- function(data) {
    columnsToSelect <- names(data) %in% c("Div", "Date", "HomeTeam", "AwayTeam", "FTHG", "FTAG", "HTHG", "HTAG", 
        "Attendance", "HS", "AS", "HST", "AST", "HHW", "AHW", "HC", "AC", "HF", "AF", "HO", "AO", "HY", "AY", 
        "HR", "AR")
    data <- data[, columnsToSelect]
    return(data)
}


removeEmptyLines <- function(data) {
    data = data[!(data$HomeTeam == "" | data$AwayTeam == ""), ]
    return(data)
}

loadLeagueData <- function(league, seasonStartYear) {
    tmp = getSeasonsName(seasonStartYear)
    seasonStartYear = tmp[1]
    seasonEndYear = tmp[2]
    
    dirName = paste("data/", seasonStartYear, "-", seasonEndYear, "/", league, ".csv", sep = "")
    
    data = read.csv(dirName, header = TRUE, sep = ",", stringsAsFactors = FALSE)
    
    
    data = removeUnUsedColumns(data)
    data = removeEmptyLines(data)
    return(data)
}


loadLeagueDataForAllSeasons <- function(league) {
    result = hash()
    idLeague = getIdForLeague(league)
    for (i in 93:114) {
        tmpData = loadLeagueData(league, i)
        tmpYears = getSeasonsName(i)
        seasonName = paste(tmpYears[1], "/", tmpYears[2], sep = "")
        
        idSeason = getIdForSeason(i)
        tmpData$season_fk = idSeason
        tmpData$Div = idLeague
        
        tmpData = renameColumns(tmpData)
        
        tmpData$match_date <- unlist(lapply(tmpData$match_date, changeDateFormat))
        
        result[seasonName] = tmpData
    }
    return(result)
}



#'Referee' = 'referee_fk',
renameColumns <- function(data) {
    data = rename(data, c(Div = "league_fk", HomeTeam = "home_team_fk", AwayTeam = "away_team_fk", Date = "match_date", 
        Attendance = "attendance", FTHG = "home_goals", FTAG = "away_goals", HTHG = "home_goals_half_time", 
        HTAG = "away_goals_half_time", HS = "home_shots", AS = "away_shots", HST = "home_shots_on_target", AST = "away_shots_on_target", 
        HHW = "home_shots_hit_woodwork", AHW = "away_shots_hit_woodwork", HC = "home_corners", AC = "away_corners", 
        HF = "home_fouls", AF = "away_fouls", HO = "home_offsides", AO = "away_offsides", HY = "home_yellows", 
        AY = "away_yellows", HR = "home_reds", AR = "away_reds"), warn_missing = FALSE)
    return(data)
}

getAllTeamsFromData <- function(data) {
    allTeams = list()
    for (key in keys(data)) {
        tmpData = data[[key]]
        allTeams = c(allTeams, tmpData$away_team_fk, recursive = TRUE)
    }
    allTeams = unique(allTeams)
    return(allTeams)
}

getAllReferees <- function(data) {
    allReferees = list()
    for (key in keys(data)) {
        tmpData = data[[key]]
        allReferees = c(allReferees, tmpData$referee_fk, recursive = TRUE)
    }
    allReferees = unique(allReferees)
    return(allReferees)
}

etl <- function(league) {
    leagueData = loadLeagueDataForAllSeasons(league)
    name = getAllTeamsFromData(leagueData)
    country = getCountryLeague(league)
    clubsTable = data.frame(name, city = NA, country, name_in_football_data = name)
    writeDataToDatabase("Clubs", clubsTable)
    
    nameR = getAllReferees(leagueData)
    if (!is.null(nameR)) {
        refeeresTable = data.frame(name = nameR, NA, NA, country, name_in_football_data = nameR)
    }
    
    
    
    for (key in keys(leagueData)) {
        tmpData = leagueData[[key]]
        for (club in name) {
            id = getClubId(club)
            tmpData$home_team_fk[tmpData$home_team_fk == club] = id
            tmpData$away_team_fk[tmpData$away_team_fk == club] = id
        }
        # if(!all(is.na(tmpData$referee_fk))){ for(referee in nameR){ id = getRefereeId(referee)
        # tmpData$referee_fk[tmpData$referee_fk == referee] = id }
        writeDataToDatabase("Matches", tmpData)
    }
    
    
}



changeDateFormat <- function(oldDate) {
    splited = unlist(strsplit(oldDate, "/"))
    year = splited[3]
    if (substr(year, 1, 1) == "9") {
        year = paste("19", year, sep = "")
    } else {
        year = paste("20", year, sep = "")
    }
    newDate = paste(year, splited[2], splited[1], sep = "-")
    return(newDate)
} 

# for england season 02/03
changeDateFormat2 <- function(oldDate) {
  splited = unlist(strsplit(oldDate, "/"))
  year = splited[3]
  newDate = paste(year, splited[2], splited[1], sep = "-")
  return(newDate)
} 

