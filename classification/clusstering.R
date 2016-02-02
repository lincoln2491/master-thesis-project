source("classification/classificationUtils.R")
library(gtools)
library(cluster)
library(stats)
library("Kendall")

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
    set.seed(5)
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
  labelsToInlcude = c( "home_shots_av10",
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

#TODO change to most newCluster
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
  
  used = unique(trRows$V13)
  toUse = c(1, 2, 3, 4, 5)
  toUse = setdiff(toUse, used)
  
  for(t in toUse){
    trRows = rbind(trRows, NA)
    trRows[ nrow(trRows), 13 ] = t
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

normalizeClusterNames <-function(clusteredData, tr){
  transitions = tr$transitions
  trRows = tr$trRows
  clusterNumberToUse = 1
  
  
  fromPrev = numeric()
  prevPrev = numeric()
  
  for(i in 1:12){
    prevPrev = fromPrev
    fromPrev = numeric();
    
    tmpTr = transitions[[i]]
    curData = clusteredData[[i]]
    nextData = clusteredData[[i+1]]
    nextData$newCluster = NA
    if(!("newCluster" %in% colnames(curData))){
      curData$newCluster = NA
    }
    
    for(j in 1:length(tmpTr)){
      curTr = tmpTr[[j]]
      t1 = curTr[1]
      t2 = curTr[2]
      
      fromPrev = append(fromPrev, t2)
      
      newCluster = unique(nextData$newCluster[ nextData$cluster == t2 ]) 
      newClusterPrev = NA
      
      if(is.na(newCluster)){
        newCluster = unique(curData$newCluster[ curData$cluster == t1 ]) 
        newClusterPrev = unique(curData$newCluster[ curData$cluster == t1 ])
      }
      
      if(is.na(newCluster)){
        newCluster = paste("c", clusterNumberToUse, sep = "")
        clusterNumberToUse = clusterNumberToUse + 1
        newClusterPrev = newCluster
      }
      else if(!(t1 %in% prevPrev)){
        newClusterPrev =  paste("c", clusterNumberToUse, sep = "")
        clusterNumberToUse = clusterNumberToUse + 1
      }
      
      if(is.na(unique(curData$newCluster[ curData$cluster == t1 ]))){
        curData$newCluster[ curData$cluster == t1 ] = newClusterPrev
      }
      
      if(is.na(unique(nextData$newCluster[ nextData$cluster == t2 ]))){
        nextData$newCluster[ nextData$cluster == t2 ] = newCluster
      }
      
      
    }
    clusteredData[[i]] = curData
    clusteredData[[i+1]] = nextData
  }
  
  
  curData = clusteredData[[13]]
  clusters = unique(curData$cluster)
  for(i in clusters){
    newCluster = unique(curData$newCluster[ curData$cluster == i ]) 
    if(is.na(newCluster)){
      newCluster = paste("c", clusterNumberToUse, sep = "")
      clusterNumberToUse = clusterNumberToUse + 1
      curData$newCluster[ curData$cluster == i ] = newCluster
    }
  }
  
  clusteredData[[13]] = curData
    
  newTrRows = trRows
  
  for(i in 1:13){
    tmp = clusteredData[[i]]
    for(j in 1:nrow(newTrRows)){
      oldCluster = newTrRows[j, i]
      if(is.na(oldCluster)){
        next
      }
      
      newCluster = unique(tmp$newCluster[ tmp$cluster == oldCluster])
      newTrRows[j, i] = newCluster
    }
  }
  
  tr$newTrRows = newTrRows
  return(list( data = clusteredData, newTr = tr))
}

calculateImportance <- function(clusteredData){
  labels = c( "home_shots_av10",
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
  importance = list()
  for(k in 1:13){
    data = clusteredData[[k]]
    tmpRes = numeric()
    for(l in 1:length(labels)){
      label = labels[l]
      if(k ==8){
        print("p")
      }
      df = data.frame(data$newCluster, data[label])
      colnames(df) = c("newCluster", label)
      means = aggregate(df[2], by=list(data$newCluster), mean)
      means = as.list(means[2])
      means = unlist(means)
      std.devs = aggregate(df[2], by=list(data$newCluster), sd)
      std.devs = as.list(std.devs[2])
      std.devs= unlist(std.devs)
      pairwise.score = matrix(nrow = length(means), ncol = length(means))
      for (i in 1:length(means)){
        for (j in 1:length(means)){
          if (i != j){
            pairwise.score[i,j] = abs(means[[i]] - means[[j]])^2 / (std.devs[[i]] * std.devs[[j]])
          }
        }
      }
      attribute.importance = sum(pairwise.score, na.rm = TRUE)
      if(k ==8){
        print("p")
      }
      tmpRes[label] = attribute.importance
    }
    tmpRes = data.frame(feature = names(tmpRes), importance = tmpRes)
    tmpRes = tmpRes[ order(tmpRes$importance, decreasing = TRUE), ]
    rownames(tmpRes) = NULL
    importance[[k]] = tmpRes
  }
  return(importance)
}

mergeImportance <-function(importance){
  mergedData = data.frame(feature = numeric(0), imporatnce = numeric(0), 
                          position = numeric(0), period = numeric(0))
  for(i in 1:13){
    tmp = importance[[i]]
    tmp$position = 1:nrow(tmp)
    tmp$period = i
    mergedData = rbind(mergedData, tmp)
  }
  return(mergedData)
}

calculateMeansAndSDForFeatures <-function(clusteredData){
  labels = c( "home_shots_av10",
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
  nrows = length(labels)
  means = data.frame(matrix(0, ncol = nrows, nrow = 13))
  stdDevs = data.frame(matrix(0, ncol = nrows, nrow = 13))
  colnames(means) = labels
  colnames(stdDevs) = labels
  rownames(means) = 1:13
  rownames(stdDevs) = 1:13
  for(label in labels){
    for(i in 1:13){
      tmp = clusteredData[[i]]
      data = tmp[[label]]
      tmpMean = mean(data)
      tmpStdDev = sd(data)
      means[i, label] = tmpMean
      stdDevs[i, label] = tmpStdDev
    }
  }
  result = list()
  result$means = means
  result$stdDevs = stdDevs
  return(result)
}

calculateMeasnAndSDOfClusters <-function(data){
  labels = c( "home_shots_av10",
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
              "diff_reds_av10",
              "home_pos",
              "away_pos")
  clusters = list()
  for(i in 1:13){
    tmp = data[[i]]
    clusters = append(clusters, unique(tmp$newCluster))
  }
  
  clusters = mixedsort(unique(unlist(clusters)))
  
  res = list()
  for(label in labels){
    means = data.frame(matrix(NA, ncol = length(clusters), nrow = 13))
    rownames(means) = 1:13
    colnames(means) = clusters
    stdDevs = data.frame(matrix(NA, ncol = length(clusters), nrow = 13))
    rownames(stdDevs) = 1:13
    colnames(stdDevs) = clusters
    
    tmpRes = list()
    for(i in 1:13){
      tmpData = data[[i]]
      for(cluster in clusters){
        mean = mean(tmpData[tmpData$newCluster == cluster, label])
        stdDev = sd(tmpData[tmpData$newCluster == cluster, label])
        means[i, cluster] = mean
        stdDevs[i, cluster] = stdDev
      }
    }
    
    tmpRes$means = means
    tmpRes$stdDevs = stdDevs
    res[[label]] = tmpRes
  }
  return(res)
}

getDistances <-function(data){
  data = data[complete.cases(data),]
  dataForClustering = getFilteredData(data)
  dataForClustering = data.frame(sapply(dataForClustering, normalize01))
  distances = dist(dataForClustering)
  return(distances)
}

estimatingNumberOfClusters <- function(data, alg){
  splitedData = splitDataToPeriods(data, 3)
  sil = rep(0, 13)
  df = as.data.frame(matrix(0, ncol = 13, nrow = 0))
  for(j in 5:20){
    set.seed(5)
    for(i in 1:13){
      tmp = splitedData[[i]]
      distances = getDistances(tmp)
      cl = clusteringOnePart(tmp, j, alg )
      sl = silhouette(cl$cluster, distances)
      sil[[i]] = mean(sl[,"sil_width"])
    }
    df = rbind(df, sil)
  }
  df = t(df)
  colnames(df) = 5:20
  rownames(df) = 1:13
  df = as.data.frame(df)
  return(df)
}

crossCorrelation <- function(val1, val2){
  cc = ccf(val1, val2, plot = FALSE)
  cc = cc[0]
  cc = unlist(cc)
  cc = cc["acf"]
  cc = as.numeric(cc)
  return(cc)
  
  if(length(val1) != length(val2)){
    stop("Different lenghts")
  }
  m1 = mean(val1)
  m2 = mean(val2)
  sd1 = sd(val1)
  sd2 = sd(val2)
  l1 = vector(mode="numeric", length=0)
  l2 = vector(mode="numeric", length=0)
  for(i in val1){
    tmp = (i - m1)/sd1
    l1 = append(l1, tmp)
  }
  
  for(i in val2){
    tmp = (i - m2)/sd2
    l2 = append(l2, tmp)
  }
  len = length(l1)
  mul = l1 * l2
  result = sum(mul)/(len-1)
  return(result)
}

calculateCrossCorrelations <- function(data){
  shotLabels = c( "home_shots_av10",
                  "away_shots_av10",
                  "diff_shots_av10")
  shotOnTargetLabels = c("home_shots_on_target_av10",
                         "away_shots_on_target_av10",
                         "diff_shots_on_target_av10")
  cornerLabels = c("home_corners_av10",
                   "away_corners_av10",
                   "diff_corners_av10")
  foulsLabels = c("home_fouls_av10",
                  "away_fouls_av10",
                  "diff_fouls_av10")
  yellowLabels = c("home_yellows_av10",
                   "away_yellows_av10",
                   "diff_yellows_av10")
  redLabels = c("home_reds_av10",
                "away_reds_av10",
                "diff_reds_av10")
  labels = list(shotLabels, shotOnTargetLabels, cornerLabels, 
             foulsLabels, yellowLabels, redLabels)
  res = list()
  for(tmpLabels in labels){
    label1 = tmpLabels[1]
    label2 = tmpLabels[2]
    
    tmp1 = lapply(data, function(x) mean(x[[label1]]))
    tmp2 = lapply(data, function(x) mean(x[[label2]]))
  
    tmp1$all = NULL
    tmp2$all = NULL
    
    tmp1 = unlist(tmp1)
    tmp2 = unlist(tmp2)
    
    cc = crossCorrelation(tmp1, tmp2)
    newName = paste(label1, label2, sep = "-")
    res[newName] = cc
    
  }
  return(res)
  
}


percentMean <-function(data, feature){
  min = lapply(KMclusteredData, function(x) min(x[[feature]]))
  max = lapply(KMclusteredData, function(x) max(x[[feature]]))
  mean = lapply(KMclusteredData, function(x) mean(x[[feature]]))
  min$all = NULL
  max$all = NULL
  mean$all = NULL
  max = unlist(max)
  min = unlist(min)
  mean = unlist(mean)
  
  res = (mean - min)/(max - min)
  return(res)
}

calculateKendallAndSpaermanPeriods <-function(importance){
  df = data.frame(periods= numeric(0), method = numeric(0), kendall= integer(0))
  for(i in 1:12){
    tmp1 = importance[[i]]
    tmp2 = importance[[i+1]]
    val = Kendall(tmp1$feature, tmp2$feature)
    val = val$tau
    val = val[[1]]
    df = rbind(df, list(i, 1, val))
  }
  
  for(i in 1:12){
    tmp1 = importance[[i]]
    tmp2 = importance[[i+1]]
    val = cor(as.numeric(tmp1$feature), 
              as.numeric(tmp2$feature), method = "spearman")
    df = rbind(df, list(i, 2, val))
  }
  colnames(df) = c("periods", "method", "correlation")
  
  df$method[df$method == 1] = "kendall"
  df$method[df$method == 2] = "spearman"
  df$periods = as.factor(paste(df$periods, df$periods + 1, sep = "-"))
  levels(df$periods) = c("1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8",
                          "8-9", "9-10", "10-11", "11-12", "12-13")
  
  return(df)
}

calculateKendallAndSpaermanAlg <-function(importance1, importance2){
  df = data.frame(period= numeric(0), method = numeric(0), kendall= integer(0))
  for(i in 1:13){
    tmp1 = importance1[[i]]
    tmp2 = importance2[[i]]
    val = Kendall(tmp1$feature, tmp2$feature)
    val = val$tau
    val = val[[1]]
    df = rbind(df, list(i, 1, val))
  }
  colnames(df) = c("period", "method", "correlation")
  
  for(i in 1:13){
    tmp1 = importance1[[i]]
    tmp2 = importance2[[i]]
    val = cor(as.numeric(tmp1$feature), 
              as.numeric(tmp2$feature), method = "spearman")
    df = rbind(df, list(i, 2, val))
  }
  
  df$method[df$method == 1] = "kendall"
  df$method[df$method == 2] = "spearman"
  return(df)
}


