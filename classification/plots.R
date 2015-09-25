library("ggplot2")
generateScatterPlots <- function(data, n){
  #   labels = c("home_goals_av10", "away_goals_av10", "diff_goals_av10", "home_goals_half_time_av10", "away_goals_half_time_av10",
  #              "diff_goals_half_time_av10", "home_shots_av10", "away_shots_av10",
  #              "diff_shots_av10", "home_shots_on_target_av10", "away_shots_on_target_av10",
  #              "diff_shots_on_target_av10", "home_corners_av10", "away_corners_av10",
  #              "diff_corners_av10", "home_fouls_av10", "away_fouls_av10", "diff_fouls_av10",
  #              "home_yellows_av10", "away_yellows_av10", "diff_yellows_av10", "home_reds_av10",
  #              "away_reds_av10","diff_reds_av10")  
  labels = c(  "home_goals_av10", "away_goals_av10","diff_goals_av10",          
               "home_goals_half_time_av10","away_goals_half_time_av10",
               "diff_goals_half_time_av10" )
  combination = combn(labels, 2)
  res = "cluster"
  data$cluster = as.factor(data$cluster)
  
  for(i in 1:ncol(combination)){
    first = combination[1, i]
    second = combination[2, i]
    fileName = paste("plots/featuresPlots/", i,"-", n, ".png", sep = "")
    png(filename = fileName, width = 1024, height = 1024)
    plot(data[[first]], data[[second]], col = data[[res]], xlab = first, ylab = second) 
    legend(x = "topleft", legend = levels(data$cluster), col = c(1:5), pch = 1)
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


generateFreqPlots <-function(data, n, isProp = FALSE, isFacets = TRUE){
  labelsToInlcude = c( "home_goals_av10", "away_goals_av10","diff_goals_av10",          
                       "home_goals_half_time_av10","away_goals_half_time_av10",
                       "diff_goals_half_time_av10" )
  plotLimit = ifelse(isProp == TRUE, 1, 60)
  for(i in 1:length(labelsToInlcude)){
    label = labelsToInlcude[i]
    tab = table(data[[label]], data$cluster)
    if(isProp == TRUE){
      tab = prop.table(tab, 1)
    }
    tab = data.frame(tab)
    tab$Var1 = as.numeric(as.character(tab$Var1))
    
    fileName = paste("plots/frequencyPlots/", i,"-", n, ".png", sep = "")
    png(filename = fileName, width = 1024, height = 1024)
    if(isFacets){
      tab = tab[ tab$Freq > 0, ]
      p = qplot(tab$Var1, tab$Freq, data = tab, facets = .~ Var2, main = label) + 
        expand_limits(y = c(0, plotLimit))
      print(p)
      
    }
    else{
      p = qplot(tab$Var1, tab$Freq, data = tab, group = tab$Var2, color = tab$Var2, main = label) + 
        geom_line() +  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + 
        expand_limits(y = c(0, plotLimit))
      print(p)
    }
    dev.off()
  }
}

generateClubsFreqPlots <- function(data, n, isProp = FALSE, side = "home"){
  tab = table(data$home_team_fk, data$cluster)
  tab2 = table(data$away_team_fk, data$cluster)
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