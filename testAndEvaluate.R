createPredictModels <- function(dataset, modellist, packType = "caret"){
  
  ret <- list()
  dataForModel <- dataset
  
  for(model in modellist){
    
    if(packType == "mlr"){
      dataForModel <- makeSurvTask(data = dataset,target = model$task.desc$target)
      modelName <- model$learner$id
    } else {
      modelName <- model$method
      
    }
    
      ret[[modelName]] <- predict(model, dataForModel)
  }
  
  return(ret)
}


evalModelListsAccs <- function(modellist, data, outcomeVar, packType = "caret", evalMetric = "Accuracy"){
  
  ret <- list()
  cnt <- 1
  modelNames <- names(modellist)
  
  for (model in modellist){
    
    if(packType == "caret") {
      confMat <- confusionMatrix(model, data[,outcomeVar])
      ret[[modelNames[cnt]]] = confMat[[c("overall",evalMetric)]]
      
    } else {
      ret[[modelNames[cnt]]] = performance(model, evalMetric)
    }
    
    
    cnt <- cnt + 1
  }
  
  return(ret)
  
}


getSensAndSpecForModels <- function(modellist, data, outcomeVar, packType = "caret"){
  
  Model <- c();
  Specificity <- c();
  Sensitivity <- c();
  Accuracy <- c();
  cnt <- 1
  modelNames <- names(modellist)
  
  for (model in modellist){
      confMat <- confusionMatrix(model, data[,outcomeVar], positive = "YES")
      Model[cnt] <- modelNames[cnt] 
      Accuracy[cnt] <- confMat[[c("overall","Accuracy")]]
      Specificity[cnt] <- confMat$byClass[["Specificity"]]
      Sensitivity[cnt] <- confMat$byClass[["Sensitivity"]]
      cnt <- cnt + 1
  }
  
  return(data.frame(Model,Accuracy, Specificity, Sensitivity))

}



getConfMatrices <- function(modellist, data, outcomeVar, packType = "caret", evalMetric = "Accuracy"){
  
  ret <- list()
  cnt <- 1
  modelNames <- names(modellist)
  for (model in modellist){
    
    if(packType == "caret") {
      confMat <- confusionMatrix(model, data[,outcomeVar])
      ret[[modelNames[cnt]]] = confMat 
      
    } else {
      ret[[modelNames[cnt]]] = performance(model, evalMetric)
    }
    
    
    cnt <- cnt + 1
  }
  
  return(ret)
  
}



evalModelListsAccsDf <- function(modellist, data, outcomeVar, packType = "caret", evalMetric = "Accuracy"){
  
  
  
  Model <- c();
  Accuracy <- c();
  cnt <- 1
  modelNames <- names(modellist)
  
  for (model in modellist){
    
    if(packType == "caret") {
      confMat <- confusionMatrix(model, data[,outcomeVar])
      Model[cnt] <- modelNames[cnt] 
      Accuracy[cnt] <- confMat[[c("overall",evalMetric)]]
      
    } else {
      
      if(evalMetric == "Accuracy"){
        evalMetric <- acc
      }
      
      Model[cnt] <- modelNames[cnt] 
      Accuracy[cnt] <- performance(model, evalMetric)
    }
    
    
    cnt <- cnt + 1
  }
  
  if(packType == "caret"){
    return(data.frame(Model, Accuracy))
  } else {
    Cindex <- Accuracy
    return(data.frame(Model, Cindex))
  }
  
  
}


plotMetricResAccrossModels <- function(data){
  
}


addResultsToOverallResultsCsv <- function(resultsCsv, data, expNumber, expName, dataSet, evalMetric = "Accuracy", prediction = "none") {
  resultsDf <- read.csv(resultsCsv, header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")

  expNumber <- rep(expNumber,nrow(data))
  expName <- rep(expName,nrow(data))
  expPredictionGroup <- rep(prediction,nrow(data))
  dataSetCol <- rep(dataSet,nrow(data))
  
  if(evalMetric == "Accuracy") {
    Cindex <- rep("",nrow(data))
    newResults <- data.frame(Cindex)
  } else if (evalMetric == "cindex") {
    Accuracy <- rep("",nrow(data))
    newResults <- data.frame(Accuracy)
  }
  
  newResults <- cbind(data, newResults)
  newResults <- cbind(dataSetCol, newResults)
  newResults <- cbind(expPredictionGroup, newResults)
  newResults <- cbind(expName, newResults)
  newResults <- cbind(expNumber, newResults)
  
  resultsDf <- rbind(resultsDf, newResults)
  
  fileToSave <- resultsCsv
  write.table(resultsDf, fileToSave, sep = ";", quote = TRUE, dec = ".", row.names = FALSE)
  
}


addResultsToOverallResultsSpecSensCsv <- function(resultsCsv, data, expNumber, expName, dataSet, prediction = "none") {
  resultsDf <- read.csv(resultsCsv, header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  expNumber <- rep(expNumber,nrow(data))
  expName <- rep(expName,nrow(data))
  expPredictionGroup <- rep(prediction,nrow(data))
  dataSetCol <- rep(dataSet,nrow(data))
  
  newResults <- data
  newResults <- cbind(dataSetCol, newResults)
  newResults <- cbind(expPredictionGroup, newResults)
  newResults <- cbind(expName, newResults)
  newResults <- cbind(expNumber, newResults)
  
  resultsDf <- rbind(resultsDf, newResults)
  
  fileToSave <- resultsCsv
  write.table(resultsDf, fileToSave, sep = ";", quote = TRUE, dec = ".", row.names = FALSE)
  
}

