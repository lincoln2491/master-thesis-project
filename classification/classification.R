source("classification/clusstering.R")
library("caret")

classification <-function(data, label){
  tmp = data[complete.cases(data),]
  dataForClassification = getFilteredData(tmp)
  
  dataForClassification$class = tmp[,label]
  part = createDataPartition(dataForClassification$class, p = 0.75, list = FALSE)
  training = dataForClassification[ part, ]
  test = dataForClassification[ -part, ]
  set.seed(5)
  model = train(class ~ ., data = training, method = "rf")
  res = c(model = model, part = part)
  return(res)
}

