library("ggplot2")
library("reshape2")

#generating scatter plots of features
#TODO do it better
generateScatterPlots <- function(data, n){
  #   labels = c("home_goals_av10", "away_goals_av10", "diff_goals_av10", "home_goals_half_time_av10", "away_goals_half_time_av10",
  #              "diff_goals_half_time_av10", "home_shots_av10", "away_shots_av10",
  #              "diff_shots_av10", "home_shots_on_target_av10", "away_shots_on_target_av10",
  #              "diff_shots_on_target_av10", "home_corners_av10", "away_corners_av10",
  #              "diff_corners_av10", "home_fouls_av10", "away_fouls_av10", "diff_fouls_av10",
  #              "home_yellows_av10", "away_yellows_av10", "diff_yellows_av10", "home_reds_av10",
  #              "away_reds_av10","diff_reds_av10")  
  labels = c( "season_fk", "home_shots_av10",
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

createAndSaveHeatmaps <-function(c1, c2, c3, c4, c5, clubs, m){
  for(i in 1:14){
    png(filename = paste("plots/", i, ".png", sep = ""), width = 1600, height = 900)
    df = createTable(c1, c2, c3, c4, c5, clubs, i)
    m = as.matrix(df)
    heatmap(prop.table(m, margin = m), cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
    dev.off()
  }
}


generateFreqPlots <-function(data, n, isProp = FALSE, isFacets = TRUE, isLine = FALSE){
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
    png(filename = fileName, width = 1024, height = 1024)
    if(isFacets){
      tab = tab[ tab$Freq > 0, ]
      p = qplot(tab$Var1, tab$Freq, data = tab, facets = .~ Var2, main = label) + 
        expand_limits(y = c(0, plotLimit))
      print(p)
      
    }
    else{
      p = qplot(tab$Var1, tab$Freq, data = tab, group = tab$Var2, color = tab$Var2, main = label) 
      if(isLine ==  TRUE){
        p = p + geom_line()
      }
      p = p + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        expand_limits(y = c(0, plotLimit))
      print(p)
    }
    dev.off()
  }
}

generateClubsFreqPlots <- function(data, n, isProp = FALSE, side = "home"){
  tab = table(data$home_team_fk, data$newCluster)
  tab2 = table(data$away_team_fk, data$newCluster)
  if(isProp == TRUE){
    tab = prop.table(tab, 1)
    tab2 = prop.table(tab2, 1)
  }
  tab = data.frame(tab)
  tab2 = data.frame(tab2)
  if(isProp == TRUE){
    tab = tab[ complete.cases(tab), ]
    tab2 = tab[ complete.cases(tab2), ]
  }
  plotLimit = ifelse(isProp == TRUE, 1, 60)
  #   tab = tab[ tab$Freq > 0, ]
  
  if(side == "home" || side == "both"){
    fileName = paste("plots/clubFrequencyPlots/", n, "-home", ".png", sep = "")
    png(filename = fileName, width = 1024, height = 1024)
    p = qplot(tab$Var1, tab$Freq, data = tab, group = tab$Var2, color = tab$Var2, main = "home_team_fk") + 
      geom_line() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      expand_limits(y = c(0, plotLimit))
    print(p) 
    dev.off()
  }
  
  if(side == "away" || side == "both"){
    fileName = paste("plots/clubFrequencyPlots/", n, "-away", ".png", sep = "")
    png(filename = fileName, width = 1024, height = 1024)
    p = qplot(tab2$Var1, tab2$Freq, data = tab2, group = tab2$Var2, color = tab2$Var2, main = "away_team_fk") + 
      geom_line() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
      expand_limits(y = c(0, plotLimit))
    print(p)
    dev.off()
  }
}


generateMostCommonCLusterPlot <- function(data, n){
  tmp = getMostCommonClusterForClub(data)
  fileName = paste("plots/mostCommonClubPlots//", n, ".png", sep = "")
  png(filename = fileName, width = 1024, height = 1024)
  p = qplot(x = tmp$team, y = tmp$MCCluster) + 
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  print(p)
  dev.off()
}

plotTransitions <- function(trRows){
  fileName = "plots/plotTransitions.png"
  png(filename = fileName, width = 1024, height = 1024)
  matplot(t(trRows), type = "l")
  dev.off()
}

plotNewTransitions <- function(trRows){
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
  matplot(t(trRows), type = "l")
  dev.off()
}


plotClustDistribution <- function(clustDistribution){
  fileName = "plots/plotClusterDistribution.png"
  png(filename = fileName, width = 1024, height = 1024)
  matplot(clustDistribution, type = "l")
  dev.off()
}

plotNewClustDistribution <- function(clustDistribution){
  fileName = "plots/plotNewClusterDistribution.png"
  png(filename = fileName, width = 1024, height = 1024)
  matplot(clustDistribution, type = "l")
  dev.off()
}


generateDensityPlots <- function(tmp, n){
  #   labels = c("home_goals_av10", "away_goals_av10", "diff_goals_av10", "home_goals_half_time_av10", "away_goals_half_time_av10",
  #              "diff_goals_half_time_av10", "home_shots_av10", "away_shots_av10",
  #              "diff_shots_av10", "home_shots_on_target_av10", "away_shots_on_target_av10",
  #              "diff_shots_on_target_av10", "home_corners_av10", "away_corners_av10",
  #              "diff_corners_av10", "home_fouls_av10", "away_fouls_av10", "diff_fouls_av10",
  #              "home_yellows_av10", "away_yellows_av10", "diff_yellows_av10", "home_reds_av10",
  #              "away_reds_av10","diff_reds_av10")  
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
  for(i in 1:length(labelsToInlcude)){
    lab = as.character(labelsToInlcude[i] )
    fileName = paste("plots/densityPlots/", i,"-", n, ".png", sep = "")
    png(filename = fileName, width = 1024, height = 1024)
    p = ggplot(data = tmp, aes_string(color = "newCluster", x =  lab)) +
      geom_density() + xlab(lab)
    print(p)
    dev.off()
  }
}

generatePlotsOfMeansAndStandardDeviations <- function(means, stdDevs){
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
  
  means$id = 1:13
  means = melt(means, id.vars = "id")
  colnames(means) = c("season", "feature", "mean")
  
  stdDevs$id = 1:13
  stdDevs = melt(stdDevs, id.vars = "id")
  colnames(stdDevs) = c("season", "feature", "stdDev")
  df = merge(means, stdDevs, sort = FALSE)
  
  #shots
  png(filename = "plots/meansAndSDFeaturesPlots/shots.png", width = 1024, height = 1024)
  p = ggplot(df[ df$feature %in% shotLabels, ], aes(x = season, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  print(p)
  dev.off()

  #shots on target  
  png(filename = "plots/meansAndSDFeaturesPlots/shotsOnTarget.png", width = 1024, height = 1024)
  p = ggplot(df[ df$feature %in% shotOnTargetLabels, ], aes(x = season, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  print(p)
  dev.off()
  
  #corners
  png(filename = "plots/meansAndSDFeaturesPlots/corners.png", width = 1024, height = 1024)
  p = ggplot(df[ df$feature %in% cornerLabels, ], aes(x = season, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  print(p)
  dev.off()
  
  #fouls
  png(filename = "plots/meansAndSDFeaturesPlots/fouls.png", width = 1024, height = 1024)
  p = ggplot(df[ df$feature %in% foulsLabels, ], aes(x = season, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  print(p)
  dev.off()
  
  #yellow cards 
  png(filename = "plots/meansAndSDFeaturesPlots/yellows.png", width = 1024, height = 1024)
  p = ggplot(df[ df$feature %in% yellowLabels, ], aes(x = season, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  print(p)
  dev.off()
  
  #red cards 
  png(filename = "plots/meansAndSDFeaturesPlots/reds.png", width = 1024, height = 1024)
  p = ggplot(df[ df$feature %in% redLabels, ], aes(x = season, y = mean, colour = feature)) + 
    geom_line() + geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
  print(p)
  dev.off()
}

generatePlotsOfMeansAndStandardDeviationsForClusters <-function(statistics){
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
                       "diff_reds_av10",
                       "home_pos",
                       "away_pos")
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
    
    filename = paste("plots/meansAndSDClustersPlots/", label, ".png", sep = "")
    png(filename = filename, width = 1024, height = 1024)
    p = ggplot(df, aes(x = season, y = mean, colour = cluster)) + geom_line() + 
      geom_point() + geom_errorbar(aes(ymin = mean - stdDev, ymax = mean+stdDev))
    print(p)
    dev.off()
  }
}

generateImportancePlots <-function(importanceData){
  df = mergeImportance(importanceData)
  
  png(filename = "plots/importanceValuesPlot.png", width = 1024, height = 1024)
  p = ggplot(df, aes(x = period, y = importance, colour = feature)) + 
    geom_point() + geom_line()
  print(p)
  dev.off()
  
  png(filename = "plots/importancePositionPlot.png", width = 1024, height = 1024)
  p = ggplot(df, aes(x = period, y = position, colour = feature)) + 
    geom_point() + geom_line()
  print(p)
  dev.off()
}

silhouettePlot <- function(df){
  df$id = 1:13
  df = melt(df, id.vars = "id")
  png(filename = "plots/silhouette.png", width = 1024, height = 1024)
  p = ggplot(df, aes(x = id, y = value, colour = variable)) + geom_line() + geom_point()
  print(p)
  dev.off()
  
}