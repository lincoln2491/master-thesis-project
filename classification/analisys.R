source("classification/clusstering.R")
source("classification/plots.R")

nCLusters = 5
clusterAlg = "km"
freqProp = TRUE
freqFacets = TRUE
clubFreqProp = TRUE
clubFreqSide = "home"
topNClubs = 5

if(!exists("data")){
  matches = getData()
  data = prepareDataForClassification(matches)
  backupData = data
}

clusteredData = clustering2(data, nCLusters, clusterAlg)

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



tr = matchClusters(clusteredData)
trRows = tr$trRows
plotTransitions(tr$trRows)

sink("classification/results.txt")
for(i in 1:nrow(trRows)){
  row = trRows[i, ]
  print("")
  print("----------------------------")
  print("")
  print(row)
  jaccardForTransitions(clusteredData, row)
  printTopNTeams(clusteredData, row, topNClubs)
}

print("----------------------------")
clustDistribution = data.frame(c1 = numeric(0), c2 = numeric(0), 
                               c3 = numeric(0), c4 = numeric(0), c5 = numeric(0))

for(i in 1:13){
  tmp = clusteredData[[i]]
  clustDistribution = rbind(clustDistribution, table(tmp$cluster))
}

print(clustDistribution)
sink()

plotClustDistribution(clustDistribution)