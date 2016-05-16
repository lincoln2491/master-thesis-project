source("classification/clusstering.R")
source("classification/classificationUtils.R")

matches = getData()
data = prepareDataForClassification(matches)
backupData = data

#TODO wczytywanie
getStatistics(matches)

tmp = getMeansAndSdForFeaturesInSeasons(matches)
means = tmp$means
sd = tmp$sd
generateMeanAndSDPlotsOfSeason(means, sd, TRUE)

splitedData = splitDataToPeriods(data, 3)

res = calculateMeansAndSDForFeatures(splitedData)

generatePlotsOfMeansAndStandardDeviations(res$means, res$stdDevs, TRUE)

kmNumberOfClusters = estimatingNumberOfClusters(data, "km")
hcNumberOfClusters = estimatingNumberOfClusters(data, "hc")

silhouettePlot(kmNumberOfClusters, "km", TRUE)
silhouettePlot(hcNumberOfClusters, "hc", TRUE)

kmRes = getSizeAndSDOfCluster(kmClusteredData)
hcRes = getSizeAndSDOfCluster(hcClusteredData)
stats = cbind(kmRes, hcRes)


kmClusteredData = clustering2(data, 5, "km")
hcClusteredData = clustering2(data, 5, "hc")

kmTr = matchClusters(kmClusteredData)
hcTr = matchClusters(hcClusteredData)

kmNewClusteredData = normalizeClusterNames(kmClusteredData, kmTr)
hcNewClusteredData = normalizeClusterNames(hcClusteredData, hcTr)

kmTr = kmNewClusteredData$newTr
hcTr = hcNewClusteredData$newTr

plotNewTransitions(kmTr$newTrRows, "km", TRUE)
plotNewTransitions(hcTr$newTrRows, "hc", TRUE)

kmClusterDistribution = calculateClustedDistribution(kmNewClusteredData$data, kmTr)
hcClusterDistribution = calculateClustedDistribution(hcNewClusteredData$data, hcTr)


plotNewClustDistribution(kmClusterDistribution, "km", TRUE)
plotNewClustDistribution(hcClusterDistribution, "hc", TRUE)

kmImportance = calculateImportance(kmNewClusteredData$data)
hcImportance = calculateImportance(hcNewClusteredData$data)

generateImportancePlots(kmImportance, "km", saveToFile = TRUE)
generateImportancePlots(hcImportance, "hc", saveToFile = TRUE)

corr = calculateKendallAndSpearmanAlg(kmImportance, hcImportance)
correlationPlot(corr, saveToFile = TRUE)

kmCorr = calculateKendallAndSpearmanPeriods(kmImportance)
hcCorr = calculateKendallAndSpearmanPeriods(hcImportance)

correlationPeriodsPlot(kmCorr, "km", saveToFile = TRUE)
correlationPeriodsPlot(hcCorr, "hc", saveToFile = TRUE)
