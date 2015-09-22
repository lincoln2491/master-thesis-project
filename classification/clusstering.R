source("classification/classificationUtils.R")



getDataForCluster <- function(data, clusters, clusterNumber){
  cl = clusters[ clusters == clusterNumber]
  cl = names(cl)
  cl = data[rownames(data) %in% cl,]
  return(cl)
}

createCountTableInClusters <- function(c1, c2, c3, c4, c5, clubs){
  c1H = sapply(clubs, function(x) table(c1$home_team_fk)[x])
  c1A = sapply(clubs, function(x) table(c1$away_team_fk)[x])
  c1S = c1H+c1A
  
  c2H = sapply(clubs, function(x) table(c2$home_team_fk)[x])
  c2A = sapply(clubs, function(x) table(c2$away_team_fk)[x])
  c2S = c2H+c2A
  
  c3H = sapply(clubs, function(x) table(c3$home_team_fk)[x])
  c3A = sapply(clubs, function(x) table(c3$away_team_fk)[x])
  c3S = c3H+c3A
  
  c4H = sapply(clubs, function(x) table(c4$home_team_fk)[x])
  c4A = sapply(clubs, function(x) table(c4$away_team_fk)[x])
  c4S = c4H+c4A
  
  c5H = sapply(clubs, function(x) table(c5$home_team_fk)[x])
  c5A = sapply(clubs, function(x) table(c5$away_team_fk)[x])
  c5S = c5H+c5A
  
  #df = data.frame(c1H,c1A,c2H,c2A,c3H,c3A,c4H,c4A,c5H,c5A,c1S,c2S,c3S,c4S,c5S)
  df = data.frame(c1S,c2S,c3S,c4S,c5S)
  return(df)
}


splitDataToPeriods <- function(data, yearsInPeriod){
  seasons = unique(data$season_fk)
  resultData = list()
  for(i in 1:(length(seasons) -yearsInPeriod + 1)){
    end = i + yearsInPeriod -1
    tmpData = data[ data$season_fk %in% seasons[i:end],]
    name = paste(seasons[i], seasons[end], sep = "-")
    resultData[[name]] = tmpData
  }
  
  return(resultData)
}

createTable <-function(c1, c2, c3, c4, c5, clubs, n){
  return(createCountTableInClusters(c1[[n]], c2[[n]], c3[[n]], c4[[n]], c5[[n]], clubs ))
}



createSortTable <- function(t){
  tmpT =  t[order(t$c1S, decreasing = T),1:5]
  df = data.frame(rownames(tmpT), tmpT$c1S)
  tmpT =  t[order(t$c2S, decreasing = T),1:5]
  df = data.frame(df, rownames(tmpT), tmpT$c2S)
  
  tmpT =  t[order(t$c3S, decreasing = T),1:5]
  df = data.frame(df, rownames(tmpT), tmpT$c3S)
  
  tmpT =  t[order(t$c4S, decreasing = T),1:5]
  df = data.frame(df, rownames(tmpT), tmpT$c4S)

  tmpT =  t[order(t$c5S, decreasing = T),1:5]
  df = data.frame(df, rownames(tmpT), tmpT$c5S)
}


clustering <-function(data, nClusters){

  dataForClustering = getFilteredData(data)
  
  dataForClustering = dataForClustering[complete.cases(dataForClustering),]
  
  splitedData = splitDataToPeriods(data, 3)
  splitedData[["all"]] = data
  
  splitedDataForClustering = splitDataToPeriods(dataForClustering, 3)
  splitedDataForClustering[["all"]]  = dataForClustering
  splitedDataForClustering = lapply(splitedDataForClustering, function(x) {x$season_fk =NULL; x})
  
#   splitedDataForClustering = lapply(splitedDataForClustering, normalize, byrow = FALSE)
  distances = sapply(splitedDataForClustering, dist)
  hc = lapply(distances, hclust)
  clusters = sapply(hc, function(x) cutree(x, k =nClusters))
  
  for(i in 1:length(splitedData)){
    tmpData = splitedData[[i]]
    tmpClust = clusters[[i]]
    df = data.frame(tmpClust)
    tmpData$cluster = df$tmpClust[match(rownames(tmpData), rownames(df))]
    splitedData[[i]] = tmpData
  }
  return(splitedData)
}

clustering2 <-function(data, nClusters, typeOf){
  splitedData = splitDataToPeriods(data, 3)
  splitedData[["all"]] = data
  
  splitedData = lapply(splitedData, clusteringOnePart, nClusters = nClusters, typeOf = typeOf)
  
  return(splitedData)
}

clusteringOnePart <-function(data, nClusters, typeOf = "hc"){
  
  data = data[complete.cases(data),]
  
  dataForClustering = getFilteredData(data)
  
#   dataForClustering = dataForClustering[complete.cases(dataForClustering),]
  
  dataForClustering$season_fk = NULL
  

  
  dataForClustering = data.frame(sapply(dataForClustering, normalize01))
  clusters = NULL  

  if(typeOf == "hc"){
    rownames(dataForClustering) = rownames(data)
    distances = dist(dataForClustering)
    hc = hclust(distances)
    clusters = cutree(hc, k =nClusters)
    df = data.frame(clusters)
    data$cluster = df$clusters[match(rownames(data), rownames(df))]
  }else if( typeOf == "km"){
    km = kmeans(dataForClustering, centers = nClusters)
    clusters = km$cluster
    df = data.frame(clusters)
    rownames(df) = rownames(data)
    data$cluster = df$clusters[match(rownames(data), rownames(df))]
  }
  
  
#   data$cluster = df$clusters[match(rownames(data), rownames(df))]
  
  
  return(data)
}

normalize01 <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

getFilteredData <- function(data){
  #   labelsToEclude = c("idMatch", "home_team_fk", "away_team_fk", "home_couch_fk", 
  #                      "away_couch_fk", "day_of_season", "date", "year", "month", "day")  
  #   dataForClustering = data[ !(names(data) %in% labelsToEclude)]
  #   
  labelsToInlcude = c( "season_fk", "home_shots_av10",
                       "away_shots_av10",
                       "diff_shots_av10",
                       "home_shots_on_target_av10",
                       "away_shots_on_target_av10",
                       "diff_shots_on_target_av10",
                       "home_corners_av10",
                       "away_corners_av10",
                       "diff_corners_av10",
                       "home_fouls_av10",
                       "away_fouls_av10",
                       "diff_fouls_av10",
                       "home_yellows_av10",
                       "away_yellows_av10",
                       "diff_yellows_av10",
                       "home_reds_av10",
                       "away_reds_av10",
                       "diff_reds_av10" )
  dataForClustering = data[ (names(data) %in% labelsToInlcude)]
  return(dataForClustering)
}


getMostCommonClusterForClub <- function(data){
  tab = table(data$home_team_fk, data$cluster)
  tabCl = table(data$cluster)
  tab = data.frame(tab)
  result = data.frame(unique(data$home_team_fk))
  colnames(result) = "team"
  result$MCCluster = sapply(result$team, function(x){
    club = x
    tab2 = tab[tab$Var1 == club, ]
    var = tab2$Var2[tab2$Freq == max(tab2$Freq)]
    if(length(var) == 1){
      var = var[1]
    }
    else{
      min = 100000
      minCl = 0
      for(tVar in var){
        if(tabCl[tVar] < min){
          minCl = tVar
        }
      }
      var = minCl
    }
    return(var)
  }) 
  return(result)
}

#TODO what with ==
matchClusters <-function(clusteredData){
  allTransitions = list()
  for(i in 1:12){
    df1 = clusteredData[[i]]
    df2 = clusteredData[[i+1]]
    df1 = data.frame(rownames(df1), df1$cluster)
    df2 = data.frame(rownames(df2), df2$cluster)
    colnames(df1) = c("id", "cluster")
    colnames(df2) = c("id", "cluster")
    s1 = split(df1, df1$cluster)
    s2 = split(df2, df2$cluster)
    
    l1 = length(s1)
    l2 = length(s2)
    
    clusterTransitions = list()
    
    for(j in 1:l1){
      nextCluster = j
      jaccard = -1
      for(k in 1:l2){
        newJaccard = jaccardIndex(s1[[j]]$id, s2[[k]]$id)
        if(newJaccard > jaccard){
          jaccard = newJaccard
          nextCluster = k
        }else if(newJaccard == jaccard & newJaccard > 0){
          print("check")
        }
      }
      clusterTransitions[[j]] = c(j, nextCluster)
    }
    
    allTransitions[[i]]=clusterTransitions
  }
  result = list(transitions = allTransitions)
  
  nClusters = length(allTransitions[[1]])
  
  trRows = matrix(NA, nrow = nClusters, ncol = 13, byrow = FALSE)
  trRows[,1] = seq(1:nClusters)
  trRows = as.data.frame(trRows)
  
  for(i in 1:12){
    tmp = allTransitions[[i]]
    for(j in 1:nClusters){
      first = tmp[[j]][[1]]
      last = tmp[[j]][[2]]
      if(nrow(trRows[trRows[i] == first,]) == 0){
        trRows = rbind(trRows, rep(NA))
        trRows[nrow(trRows), i] = first
        trRows[nrow(trRows), i + 1] = last
      }
      else{
        trRows[trRows[i] == first & !is.na(trRows[i]),i + 1] = last
      }
    }
  }
  result$trRows = trRows
  return(result)
}

printTopNTeams <- function(clusteredData, trRow, n){
  commonsClubs = list()
  for(i in 1: 13){
    tmp = clusteredData[[i]]
    cNumber = as.numeric(trRow[i])
    if(is.na(cNumber)){
      next
    }
    tmp = tmp[ tmp$cluster == cNumber, ]
    tab = table(tmp$home_team_fk)
    tab = tab[order(tab, decreasing = TRUE)]
    tab = data.frame(tab[1:n])
    colnames(tab) = c("count")
    tab = data.frame(rownames(tab), tab$count)
    colnames(tab) = c("club", "count")
    rownames(tab) = NULL
    tab = tab[ tab$count > 0, ]
    for(i in 1:nrow(tab)){
      key = as.character(tab[i, 1])
      if(key %in% names(commonsClubs)){
        commonsClubs[key] = as.numeric(commonsClubs[key]) + 1
      }
      else{
        commonsClubs[key] = 1
      }
    }
    print(i)
    print(tab)
  }
  
  print("Often top clubs")
  commonsClubs = data.frame(unlist(commonsClubs))
  colnames(commonsClubs) = c("count")
  commonsClubs = data.frame(rownames(commonsClubs), commonsClubs$count)
  colnames(commonsClubs) = c("club", "count")
  commonsClubs = commonsClubs[ order(commonsClubs$count, decreasing = TRUE), ]
  print(commonsClubs)
}

jaccardForTransitions <- function(clusteredData, trRow){
  for(i in 1:12){
    tmp = clusteredData[[i]]
    tmp2 = clusteredData[[i+1]]
    if(is.na(trRow[i])){
      print(NA)
      next
    }
    cNumber = as.numeric(trRow[i])
    cNumber2 = as.numeric(trRow[i+1])
    tmp = tmp[ tmp$cluster == cNumber, ]
    tmp2 = tmp2[ tmp2$cluster == cNumber2, ]
    cat(cNumber, "->", cNumber2, ": ", jaccardIndex(tmp$idMatch, tmp2$idMatch), 
        "(", length(tmp$idMatch), "/", lengthOfIntersect(tmp$idMatch, tmp2$idMatch),
        "/", length(tmp2$idMatch), ")", "\n")
  }
}
