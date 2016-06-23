source("classification/clusstering.R")
library("caret")

classification <-function(dataForClassification, label){
  cl = dataForClassification[,label, with = FALSE]
  dataForClassification = getFilteredData(dataForClassification)
  
  dataForClassification$class = cl #data[,label, with = FALSE]
  set.seed(5)
  partitions = as.vector(createDataPartition(dataForClassification$class, p = 0.75, list = FALSE))
  training = dataForClassification[ partitions, ]
  testing = dataForClassification[ -partitions, ]
  set.seed(5)
  trainedModel = train(x = training[,1:14, with = FALSE], y = training$class, method = "rf")
  res = list(model = trainedModel, part = partitions, train = training, test = testing, data = dataForClassification)
  
  return(res)
}


try <- function(clusterdeData, data, label, isRegresion = FALSE){
  acc = numeric()
  for(name in names(clusteredData)){
    tmp = clusteredData[[name]]
    classifier = classification(tmp, label)
    predictions = predict(classifier$model, newdata = classifier$test)
    if(isRegresion == TRUE){
      acc[name] = RMSE(predictions, classifier$test$class)
      # acc = c(acc, RMSE(predictions, classifier$test$class))
      # print(acc)
    }
    else{
      cm = confusionMatrix(predictions, classifier$test$class)
      ov = cm$overall
      acc[name]= (ov[["Accuracy"]])
    }
  }
  m = mean( unlist(acc) )
  sd = sd(unlist(acc))
  # print(m)
  acc["mean"] = m
  acc["sd"] = sd
  classifier = classification(data, label)
  predictions = predict(classifier$model, newdata = classifier$test)
  if(isRegresion == TRUE){
    acc["all"] = RMSE(predictions, classifier$test$class)
    # print(RMSE(predictions, classifier$test$class))
  }
  else{
    cm = confusionMatrix(predictions, classifier$test$class)
    ov = cm$overall
    acc["all"]= (ov[["Accuracy"]])
    # print(ov[["Accuracy"]])
  }
  print(label)
  print(acc)
  return(acc)
  
}


classifyAll <- function(clusteredData, data){
  av_goals = try(clusteredData, data, "av_goals", isRegresion = TRUE)
  av_goals_half_time = try(clusteredData, data, "av_goals_half_time", isRegresion = TRUE)
  av_op_goals = try(clusteredData, data, "av_op_goals", isRegresion = TRUE)
  av_op_goals_half_time = try(clusteredData, data, "av_op_goals_half_time", isRegresion = TRUE)
  wins = try(clusteredData, data, "wins", isRegresion = TRUE)
  draws = try(clusteredData, data, "draws", isRegresion = TRUE)
  loses = try(clusteredData, data, "loses", isRegresion = TRUE)
  av_points = try(clusteredData, data, "av_points", isRegresion = TRUE)
  av_op_points = try(clusteredData, data, "av_op_points", isRegresion = TRUE)
 
  team = try(clusteredData, data, "team", isRegresion = FALSE)
  type = try(clusteredData, data, "type", isRegresion = FALSE)
  leaguePosition = try(clusteredData, data, "leaguePosition", isRegresion = FALSE)
  
  
  res = cbind(team,
              type,
              av_goals,
              av_goals_half_time,
              av_op_goals,
              av_op_goals_half_time,
              wins,
              draws,
              loses,
              av_points,
              av_op_points,
              leaguePosition)
  return(res)
  
  
}