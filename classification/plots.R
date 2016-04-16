library("ggplot2")
library("reshape2")
library("data.table")
generatePlot <- function(p, saveToFile = FALSE, name){
  if(saveToFile){
    png(filename = name, width = 1024, height = 1024)
    print(p)
    dev.off()
  }else{
    print(p)
  }
}

#generating scatter plots of features
#TODO do it better
generateScatterPlots <- function(data, n, saveToFile = FALSE){
  labels = c( "av_goals",
              "av_goals_half_time",
              "av_shots",
              "av_shots_on_target",
              "av_corners",
              "av_fouls",
              "av_yellows",
              "av_reds",
              "av_shots_outside_target",
              "av_op_goals",
              "av_op_goals_half_time",
              "av_op_shots",
              "av_op_shots_on_target",
              "av_op_corners",
              "av_op_fouls",
              "av_op_yellows",
              "av_op_reds",
              "av_op_shots_outside_target",
              "wins",
              "draws",
              "loses",
              "av_points",
              "av_op_points")
  combination = combn(labels, 2)
  res = "newCluster"
  data$newCluster = as.factor(data$newCluster)
  numberOfClusters = length(unique(data$newCluster))
  for(i in 1:ncol(combination)){
    first = combination[1, i]
    second = combination[2, i]
    fileName = paste("plots/featuresPlots/", i,"-", n, ".png", sep = "")
    png(filename = fileName, width = 1024, height = 1024)
    plot(data[[first]], data[[second]], col = data[[res]], xlab = first, ylab = second) 
    legend(x = "topleft", legend = levels(data$newCluster), col = c(1:numberOfClusters), pch = 1)
    dev.off()
  }
}

createAndSaveHeatmaps <-function(c1, c2, c3, c4, c5, clubs, m, saveToFile = FALSE){
  for(i in 1:14){
    png(filename = paste("plots/", i, ".png", sep = ""), width = 1600, height = 900)
    df = createTable(c1, c2, c3, c4, c5, clubs, i)
    m = as.matrix(df)
    heatmap(prop.table(m, margin = m), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    dev.off()
  }
}

generateFreqPlots <-function(data, n, isProp = FALSE, isFacets = TRUE, isLine = FALSE, saveToFile = FALSE){
  labelsToInlcude = c( "av_goals",
                       "av_goals_half_time",
                       "av_shots",
                       "av_shots_on_target",
                       "av_corners",
                       "av_fouls",
                       "av_yellows",
                       "av_reds",
                       "av_shots_outside_target",
                       "av_op_goals",
                       "av_op_goals_half_time",
                       "av_op_shots",
                       "av_op_shots_on_target",
                       "av_op_corners",
                       "av_op_fouls",
                       "av_op_yellows",
                       "av_op_reds",
                       "av_op_shots_outside_target",
                       "wins",
                       "draws",
                       "loses",
                       "av_points",
                       "av_op_points")
  plotLimit = ifelse(isProp == TRUE, 1, 60)
  for(i in 1:length(labelsToInlcude)){
    label = labelsToInlcude[i]
    tab = table(data[[label]], data$newCluster)
    if(isProp == TRUE){
      tab = prop.table(tab, 1)
    }
    tab = data.frame(tab)
    if(label == "season_fk"){
      tab$Var1 = as.factor(as.character(tab$Var1))
    }
    else{
      tab$Var1 = as.numeric(as.character(tab$Var1)) 
    }
    #tab$Var1 = as.numeric(as.character(tab$Var1))
    
    fileName = paste("plots/frequencyPlots/", i,"-", n, ".png", sep = "")
    if(isFacets){
      tab = tab[ tab$Freq > 0, ]
      p = qplot(tab$Var1, tab$Freq, data = tab, facets = .~ Var2, main = label) + 
        expand_limits(y = c(0, plotLimit))
      generatePlot(p, saveToFile, fileName)
      
    }
    else{
      p = qplot(tab$Var1, tab$Freq, data = tab, group = tab$Var2, color = tab$Var2, main = label) 
      if(isLine ==  TRUE){
        p = p + geom_line()
      }
      p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        expand_limits(y = c(0, plotLimit))
      generatePlot(p, saveToFile, fileName)
    }
  }
}

generateClubsFreqPlots <- function(data, n, isProp = FALSE, side = "home", saveToFile = FALSE){
  tab = table(data$team, data$newCluster)
  if(isProp == TRUE){
    tab = prop.table(tab, 1)
  }
  tab = data.frame(tab)
  if(isProp == TRUE){
    tab = tab[ complete.cases(tab), ]
  }
  plotLimit = ifelse(isProp == TRUE, 1, 60)
  #   tab = tab[ tab$Freq > 0, ]
  
  # if(side == "home" || side == "both"){
  fileName = paste("plots/clubFrequencyPlots/", n, "-home", ".png", sep = "")
  p = qplot(tab$Var1, tab$Freq, data = tab, group = tab$Var2, color = tab$Var2, main = "home_team_fk") + 
    geom_line() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
    expand_limits(y = c(0, plotLimit))
  generatePlot(p, saveToFile, fileName)
}

generateMostCommonCLusterPlot <- function(data, n, saveToFile = FALSE){
  tmp = getMostCommonClusterForClub(data)
  fileName = paste("plots/mostCommonClubPlots//", n, ".png", sep = "")
  p = qplot(x = tmp$team, y = tmp$MCCluster) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  generatePlot(p, saveToFile, fileName)
}

plotTransitions <- function(trRows, saveToFile = FALSE){
  fileName = "plots/plotTransitions.png"
  png(filename = fileName, width = 1024, height = 1024)
  matplot(t(trRows), type = "b")
  dev.off()
}

plotNewTransitions <- function(trRows, saveToFile = FALSE){
  for(i in 1:13){
    for(j in 1:nrow(trRows)){
      if(is.na(trRows[j, i])){
        next
      }
      trRows[j, i] = as.numeric(gsub("c", "", trRows[j, i]))
    }
  }
  fileName = "plots/plotNewTransitions.png"
  png(filename = fileName, width = 1024, height = 1024)
  matplot(t(trRows), type = "b")
  dev.off()
}

plotClustDistribution <- function(clustDistribution, saveToFile = FALSE){
  fileName = "plots/plotClusterDistribution.png"
  png(filename = fileName, width = 1024, height = 1024)
  matplot(clustDistribution, type = "b")
  dev.off()
}

plotNewClustDistribution <- function(clustDistribution, saveToFile = FALSE){
  fileName = "plots/plotNewClusterDistribution.png"
  png(filename = fileName, width = 1024, height = 1024)
  matplot(clustDistribution, type = "b")
  dev.off()
}

generateDensityPlots <- function(tmp, n, saveToFile = FALSE){
  labelsToInlcude = c( "av_goals",
                       "av_goals_half_time",
                       "av_shots",
                       "av_shots_on_target",
                       "av_corners",
                       "av_fouls",
                       "av_yellows",
                       "av_reds",
                       "av_shots_outside_target",
                       "av_op_goals",
                       "av_op_goals_half_time",
                       "av_op_shots",
                       "av_op_shots_on_target",
                       "av_op_corners",
                       "av_op_fouls",
                       "av_op_yellows",
                       "av_op_reds",
                       "av_op_shots_outside_target",
                       "wins",
                       "draws",
                       "loses",
                       "av_points",
                       "av_op_points")
  for(i in 1:length(labelsToInlcude)){
    lab = as.character(labelsToInlcude[i] )
    fileName = paste("plots/densityPlots/", i,"-", n, ".png", sep = "")
    p = ggplot(data = tmp, aes_string(color = "newCluster", x =  lab)) +
      geom_density() + xlab(lab)
    generatePlot(p, saveToFile, fileName)
  }
}

generatePlotsOfMeansAndStandardDeviations <- function(means, stdDevs, saveToFile = FALSE){
  shotLabels = c( "av_shots",
                   "av_op_shots")
  shotOnTargetLabels = c("av_shots_on_target",
                         "av_op_shots_on_target")
  shotOutsideTargetLabels = c("av_shots_outside_target",
                         "av_op_shots_outside_target")
  cornerLabels = c("av_corners",
                  "av_op_corners")
  foulsLabels = c("av_fouls",
                  "av_op_fouls")
  yellowLabels = c("av_yellows",
                   "av_op_yellows")
  redLabels = c("av_reds",
                "av_op_reds")
  
  means$id = 1:13
  means = melt(means, id.vars = "id")
  colnames(means) = c("period", "feature", "mean")
  
  stdDevs$id = 1:13
  stdDevs = melt(stdDevs, id.vars = "id")
  colnames(stdDevs) = c("period", "feature", "stdDev")
  df = merge(means, stdDevs, sort = FALSE)
  
  #shots
  fileName = "plots/meansAndSDFeaturesPlots/shots.png"
  p = ggplot(df[ df$feature %in% shotLabels, ], aes(x = period, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  generatePlot(p, saveToFile, fileName)

  #shots on target  
  fileName = "plots/meansAndSDFeaturesPlots/shotsOnTarget.png"
  p = ggplot(df[ df$feature %in% shotOnTargetLabels, ], aes(x = period, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  generatePlot(p, saveToFile, fileName)
  
  #shots outside target  
  fileName = "plots/meansAndSDFeaturesPlots/shotsOnTarget.png"
  p = ggplot(df[ df$feature %in% shotOutsideTargetLabels, ], aes(x = period, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  generatePlot(p, saveToFile, fileName)
  
  #corners
  fileName = "plots/meansAndSDFeaturesPlots/corners.png"
  p = ggplot(df[ df$feature %in% cornerLabels, ], aes(x = period, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  generatePlot(p, saveToFile, fileName)
  
  #fouls
  fileName = "plots/meansAndSDFeaturesPlots/fouls.png"
  p = ggplot(df[ df$feature %in% foulsLabels, ], aes(x = period, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  generatePlot(p, saveToFile, fileName)
  
  #yellow cards 
  fileName = "plots/meansAndSDFeaturesPlots/yellows.png"
  p = ggplot(df[ df$feature %in% yellowLabels, ], aes(x = period, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  generatePlot(p, saveToFile, fileName)
  
  #red cards 
  fileName = "plots/meansAndSDFeaturesPlots/reds.png"
  p = ggplot(df[ df$feature %in% redLabels, ], aes(x = period, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  generatePlot(p, saveToFile, fileName)
}

generatePlotsOfMeansAndStandardDeviationsForClusters <-function(statistics, saveToFile = FALSE){
  labelsToInlcude = c( "av_goals",
                       "av_goals_half_time",
                       "av_shots",
                       "av_shots_on_target",
                       "av_corners",
                       "av_fouls",
                       "av_yellows",
                       "av_reds",
                       "av_shots_outside_target",
                       "av_op_goals",
                       "av_op_goals_half_time",
                       "av_op_shots",
                       "av_op_shots_on_target",
                       "av_op_corners",
                       "av_op_fouls",
                       "av_op_yellows",
                       "av_op_reds",
                       "av_op_shots_outside_target")
  for(label in labelsToInlcude){
    tmp = statistics[[label]]
    means = tmp$means
    means$id = 1:13
    means = melt(means, id.vars = "id")
    colnames(means) = c("season", "cluster", "mean")
    
    stdDevs = tmp$stdDevs
    stdDevs$id = 1:13
    stdDevs = melt(stdDevs, id.vars = "id")
    colnames(stdDevs) = c("season", "cluster", "stdDev")
    
    df = merge(means, stdDevs, sort = FALSE)
    
    fileName = paste("plots/meansAndSDClustersPlots/", label, ".png", sep = "")
   
    p = ggplot(df, aes(x = season, y = mean, colour = cluster)) + geom_line() + 
      geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
    generatePlot(p, saveToFile, fileName)
  }
}

generateImportancePlots <-function(importanceData, saveToFile = FALSE){
  df = mergeImportance(importanceData)
  
  fileName = "plots/importanceValuesPlot.png"
  p = ggplot(df, aes(x = period, y = importance, colour = feature)) + 
    geom_point() + geom_line()
  generatePlot(p, saveToFile, fileName)
  
  fileName = "plots/importancePositionPlot.png"
  p = ggplot(df, aes(x = period, y = position, colour = feature)) + 
    geom_point() + geom_line()
  generatePlot(p, saveToFile, fileName)
}

silhouettePlot <- function(df, alg, saveToFile = FALSE){
  df$id = 1:13
  df = melt(df, id.vars = "id")
  colnames(df) = c("period", "number_of_clusters", "silhouette_coefficient")
  fileName = paste("plots/silhouette", alg, ".png", sep = "")
  
  p = ggplot(df, aes(x = period, y = silhouette_coefficient, colour = number_of_clusters)) + 
    geom_line() + geom_point() + ylab("silhouette coefficient") + 
    guides(fill=guide_legend(title="number of clusters"))
  generatePlot(p, saveToFile, fileName)
}

correlationPlot <-function(df, saveToFile = FALSE){
  fileName = "plots/correlation.png"
  p = ggplot(df, aes(x = period, y = correlation, color = method)) + 
    geom_line() + geom_point()
  generatePlot(p, saveToFile, fileName)
}

correlationPeriodsPlot <-function(df, saveToFile = FALSE){
  fileName = "plots/correlationPeriods.png"
  p = ggplot(df, aes(x = periods, y = correlation, color = method)) + 
    geom_line(aes(group = method)) + geom_point()
  generatePlot(p, saveToFile, fileName)
}

generatePlacesPerClusterPlot <- function(data, i, saveToFile = FALSE){
  tab = table(data$newCluster, data$leaguePosition )
  tab = as.data.frame(tab)
  colnames(tab) = c("newCluster", "leaguePos", "count")
  
  p = ggplot(tab, aes(x = newCluster, y = count, colour = leaguePos, group = leaguePos) ) + 
    geom_point() + geom_line() + ggtitle(as.character(i))
  fileName = paste("plots/placesPerCLusterPlot/", i, ".png", sep = "")
  generatePlot(p, saveToFile, fileName)
}

createImportanceMovingAveragePlots <- function(data, saveToFile = FALSE){
  data$feature = rownames(data)
  data = melt(data, id.vars = "feature")
  colnames(data) = c("feature", "periods", "ma")
  
  fileName = "plots/importanceMovingAveragePlot.png"
  p = ggplot(data, aes(x = periods, y = ma, group = feature, colour = feature)) + geom_line() + geom_point()
  
  generatePlot(p, saveToFile, fileName)
}

generatePlacesPlot <- function(data, saveToFile = FALSE, removeNotComplete = FALSE, maxNumberOfNA = 0){
  if(removeNotComplete){
    data = data[complete.cases(data),]
  }else{
    numNAs <- apply(data, 1, function(z) sum(is.na(z)))
    data = data[!(numNAs> maxNumberOfNA),]
  }
  data = melt(data, id.vars = "team")
  colnames(data) = c("team", "season", "place")
  fileName = "plots/leaguePlacesPlot.png"
  
  p = ggplot(data, aes(x = season, y = place, group = team, colour = team)) + geom_line() + geom_point()
  generatePlot(p, saveToFile, fileName)
}

generateTeamPlacesPlot <- function(data, saveToFile = FALSE){
  teams = data$team
  for(teamName in teams){
    tmp = data[data$team == teamName]
    tmp = melt(tmp, id.vars = "team")
    colnames(tmp) = c("team", "season", "place")
    fileName = paste("plots/places/", teamName, ".png", sep = "")

    p = ggplot(tmp, aes(x = season, y = place, group = 1), title()) + ylim(1,20) + geom_line() + geom_point() + ggtitle(teamName)
    generatePlot(p, saveToFile, fileName)
  }
}
