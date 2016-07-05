source("classification/clusstering.R")
source("classification/classificationUtils.R")

labels = c("av_shots", 
           "av_shots_on_target",
           "av_shots_outside_target",
           "av_corners",
           "av_fouls",
           "av_yellows",
           "av_reds",
           "av_op_shots",
           "av_op_shots_on_target",
           "av_op_shots_outside_target",
           "av_op_corners",
           "av_op_fouls",
           "av_op_yellows",
           "av_op_reds")

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

mb = calculateBelow(splitedData)

res = calculateMeansAndSDForFeatures(splitedData)
means = res$means
round2(cor(means, means))
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

kmImportanceLevels = getImportanceLevels(kmImportance, FALSE)
hcImportanceLevels = getImportanceLevels(hcImportance, FALSE)

corrLevels = calculateKendallAndSpearmanLevelsAlg(kmImportanceLevels, hcImportanceLevels)

tmp = createLevelsStatistic(kmImportanceLevels)
tmp = tmp[match(labels, rownames(tmp)),]
write.csv(tmp, "km.csv", quote = FALSE)
tmp = createLevelsStatistic(hcImportanceLevels)
tmp = tmp[match(labels, rownames(tmp)),]
write.csv(tmp, "hc.csv", quote = FALSE)

kmCorrL = calculateKendallAndSpearmanLevelsPeriods(kmImportance, FALSE)
hcCorrL = calculateKendallAndSpearmanLevelsPeriods(hcImportance, FALSE)

correlationPeriodsLevelsPlot(kmCorrL, "km", TRUE)
correlationPeriodsLevelsPlot(hcCorrL, "hc", TRUE)
correlationLevelsPlot(corrLevels, TRUE)

av_goals = try(kmClusteredData, data, "av_goals", isRegresion = TRUE)
av_goals_half_time = try(kmClusteredData, data, "av_goals_half_time", isRegresion = TRUE)
av_op_goals = try(kmClusteredData, data, "av_op_goals", isRegresion = TRUE)
av_op_goals_half_time = try(kmClusteredData, data, "av_op_goals_half_time", isRegresion = TRUE)
wins = try(kmClusteredData, data, "wins", isRegresion = TRUE)
draws = try(kmClusteredData, data, "draws", isRegresion = TRUE)
loses = try(kmClusteredData, data, "loses", isRegresion = TRUE)
av_points = try(kmClusteredData, data, "av_points", isRegresion = TRUE)
av_op_points = try(kmClusteredData, data, "av_op_points", isRegresion = TRUE)

team = try(kmClusteredData, data, "team", isRegresion = FALSE)
type = try(kmClusteredData, data, "type", isRegresion = FALSE)
leaguePosition = try(kmClusteredData, data, "leaguePosition", isRegresion = FALSE)


classRes = round2(data.frame(team,
            type,
            leaguePosition))

regRes = round2(data.frame(av_goals,
                    av_goals_half_time,
                    av_op_goals,
                    av_op_goals_half_time,
                    wins,
                    draws,
                    loses,
                    av_points,
                    av_op_points))
