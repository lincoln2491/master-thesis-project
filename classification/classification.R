source("classification/clusstering.R")
library("caret")

classification <-function(dataForClassification, label){
  dataForClassification = getFilteredData(dataForClassification)
  
  dataForClassification$class = data[,label, with = FALSE]
  set.seed(654)
  partitions = as.vector(createDataPartition(dataForClassification$class, p = 0.75, list = FALSE))
  training = dataForClassification[ partitions, ]
  testing = dataForClassification[ -partitions, ]
  set.seed(5)
  trainedModel = train(class ~ ., data = training, method = "rf")
  res = list(model = trainedModel, part = partitions, train = training, test = testing, data = dataForClassification)
  
  return(res)
}


try <- function(clusterdeData, data, label, isRegresion = FALSE){
  acc = list()
  for(tmp in clusteredData){
    classifier = classification(tmp, label)
    predictions = predict(classifier$model, newdata = classifier$test)
    if(isRegresion == TRUE){
      acc = c(acc, RMSE(predictions, classifier$test$class))
    }
    else{
      cm = confusionMatrix(predictions, classifier$test$class)
      ov = cm$overall
      acc = c(acc, (ov[["Accuracy"]]))
    }
  }
  m = mean( unlist(acc) )
  print(m)
  classifier = classification(data, label)
  predictions = predict(classifier$model, newdata = classifier$test)
  if(isRegresion == TRUE){
    print(RMSE(predictions, classifier$test$class))
  }
  else{
    cm = confusionMatrix(predictions, classifier$test$class)
    ov = cm$overall
    print(ov[["Accuracy"]])
  }
  
}