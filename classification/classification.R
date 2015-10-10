source("classification/clusstering.R")

classification <-function(data){
  tmp = data[complete.cases(data),]
  dataForClassification = getFilteredData(tmp)
  
  dataForClassification$result = tmp$result
  part = createDataPartition(dataForClassification$result, p = 0.75, list = FALSE)
  training = dataForClassification[ part, ]
  test = dataForClassification[ -part, ]
  fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  model = train(result ~ ., data = training, method = "rf", trControl = fitControl)
  return(model)
}