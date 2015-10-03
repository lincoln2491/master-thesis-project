source("classification/clusstering.R")
source("classification/plots.R")
set.seed(5)

#params
nClusters = 5
clusterAlg = "km"
freqProp = TRUE
freqFacets = FALSE
freqIsLine = FALSE
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
plotTransitions(tr$trRows)

sink("classification/results.txt")

#printing informations about transitions
for(i in 1:nrow(tr$trRows)){
  row = tr$trRows[i, ]
  print("")
  print("----------------------------")
  print("")
  print(row)
  jaccardForTransitions(clusteredData, row)
  printTopNTeams(clusteredData, row, topNClubs)
}

print("----------------------------")
print("transitions")
print(tr$trRows)
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
plotClustDistribution(clustDistribution)

#calculating new cluster names
newClusteredData = normalizeClusterNames(clusteredData, tr)
tr = newClusteredData$newTr
clusteredData = newClusteredData$data

#printing new transitions
print("----------------------------")
print("----------------------------")
print("New transitions")
plotNewTransitions(tr$newTrRows)
print(tr$newTrRows)

#calculating new cluster distribution
print("----------------------------")
print("New cluster distribution")
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

plotNewClustDistribution(clustDistribution)
print(clustDistribution)

#means and standard deviations

res = calculateMeansAndSDForFeatures(clusteredData)
print("----------------------------")
print("----------------------------")
print("Means and standard deviation")
print("Mean")
print(res$means)
print("----------------------------")
print("Standard deviation")
print(res$stdDevs)
generatePlotsOfMeansAndStandardDeviations(res$means, res$stdDevs)


#means and standard deviations for clusters
res = calculateMeasnAndSDOfClusters(clusteredData)
print("----------------------------")
print("----------------------------")
print("Means and standard deviation for clusters")
for(label in names(res)){
  tmp = res[[label]]
  print("----------------------------")
  print(label)
  print("Mean")
  print(tmp$means)
  print("Standard deviation")
  print(tmp$stdDevs)
}
generatePlotsOfMeansAndStandardDeviationsForClusters(res)


sink()


#creating plots 
for(i in 1:13){
  generateScatterPlots(clusteredData[[i]], i)
  generateFreqPlots(clusteredData[[i]], i, freqProp, freqFacets, freqIsLine)
  generateClubsFreqPlots(clusteredData[[i]], i, clubFreqProp, clubFreqSide)
  generateMostCommonCLusterPlot(clusteredData[[i]], i)
  generateDensityPlots(clusteredData[[i]], i)
}
# generateScatterPlots(clusteredData[[14]], "all")
# generateFreqPlots(clusteredData[[14]], "all", freqProp, freqFacets, freqIsLine)
# generateClubsFreqPlots(clusteredData[[14]], 14, clubFreqProp, clubFreqSide)
# generateMostCommonCLusterPlot(clusteredData[[14]], 14)

