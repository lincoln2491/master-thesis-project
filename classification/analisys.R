source("classification/clusstering.R")
source("classification/plots.R")
#params
nClusters = 5
clusterAlg = "km"
freqProp = TRUE
freqFacets = TRUE
clubFreqProp = TRUE
clubFreqSide = "home"
topNClubs = 5


#load data if not loaded
if(!exists("data")){
  matches = getData()
  data = prepareDataForClassification(matches)
  backupData = data
}

#clustering
clusteredData = clustering2(data, nClusters, clusterAlg)

#matching clusters
tr = matchClusters(clusteredData)
trRows = tr$trRows
plotTransitions(tr$trRows)

sink("classification/results.txt")
print(trRows)

#printing informations about transitions
for(i in 1:nrow(trRows)){
  row = trRows[i, ]
  print("")
  print("----------------------------")
  print("")
  print(row)
  jaccardForTransitions(clusteredData, row)
  printTopNTeams(clusteredData, row, topNClubs)
}

#calculating cluster distributions
print("----------------------------")
clustDistribution = data.frame(c1 = numeric(0), c2 = numeric(0), 
                               c3 = numeric(0), c4 = numeric(0), c5 = numeric(0))

for(i in 1:13){
  tmp = clusteredData[[i]]
  clustDistribution = rbind(clustDistribution, table(tmp$cluster))
}

print("clustDistribution")
print(clustDistribution)

sink()

plotClustDistribution(clustDistribution)

#calculating new cluster names
newClusteredData = normalizeClusterNames(clusteredData, tr)
tr = newClusteredData$newTr
clusteredData = newClusteredData$data

plotNewTransitions(tr$newTrRows)


#calculating new cluster distribution
clustDistribution = as.data.frame(matrix(NA, ncol = nrow(tr$newTrRows), nrow = 13))

for(i in 1:13){
  tmp = clusteredData[[i]]
  tab = table(tmp$newCluster)
  for(j in names(tab)){
    x = as.numeric(gsub("c", "", j))
    val = tab[[j]]
    clustDistribution[i, x] = val
  }
}

#creating plots 
for(i in 1:13){
  generateScatterPlots(clusteredData[[i]], i)
  generateFreqPlots(clusteredData[[i]], i, freqProp, freqFacets)
  generateClubsFreqPlots(clusteredData[[i]], i, clubFreqProp, clubFreqSide)
  generateMostCommonCLusterPlot(clusteredData[[i]], i)
}
generateScatterPlots(clusteredData[[14]], "all")
generateFreqPlots(clusteredData[[14]], "all", freqProp, freqFacets)
generateClubsFreqPlots(clusteredData[[14]], 14, clubFreqProp, clubFreqSide)
generateMostCommonCLusterPlot(clusteredData[[14]], 14)

