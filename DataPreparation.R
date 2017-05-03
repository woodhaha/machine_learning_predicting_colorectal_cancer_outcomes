genPrep <- function(seed){
  #Clean environment 
  rm(list=ls())
  gc()
  
  source("DataPreparation.R")
  source("trainingModels.R")
  source("testAndEvaluate.R")
  source("plotResults.R")
  
  #include libraries
  library(caret)
  library(randomForest)
  library(doMC)
  library(plyr)
  library(reshape)
  library(randomForestSRC)
  library(mlr)
  
  #register DoMC cores
  registerDoMC(cores = 8)
  set.seed(seed)
}

getColsWithTooManyBadVals <- function(data, cutoff = 200, colsToIgnore = c("")){
  # finds all columns with more NA values than cutoff and returns them
  
  fsColnames <- colnames(data)
  colsToDelete <- c()
  
  for(curColName in fsColnames){
    curCol <- data[[curColName]]
    curColN <-  curCol[is.na(curCol)]
    curColN <-  curCol[curCol == "" | curCol == "NA"]
    lCol<- length(curColN)
    if(lCol > cutoff && ! curColName %in% colsToIgnore  ){
      colsToDelete <- c(colsToDelete, curColName)
    }
    
  }
  
  return(colsToDelete)
}


createInputCsv <- function(data, fileToSave){
  # small wrapper for writing output file
  write.table(data, fileToSave, sep = ";", quote = TRUE, dec = ".", row.names = FALSE, append = FALSE)
}


changeDataFilePostFeatureSelection <- function(myConfig, experimentsToRun) {
  # changes data file post feature selection:
  # - selecting appropriate features
  # - filtering data according to config
  # - and calling the appropriate dcf (data change function) from the config
  
  for(i in experimentsToRun){
    
    modelConfig <- myConfig[i,]
    fileToSave <- ""
    fileToSave <- paste(modelConfig$inputFile)
    colsToSelect <- grepl("inputVar", colnames(modelConfig))
    colsToSelect <- modelConfig[,colsToSelect]
    colsToSelect <- sapply(colsToSelect, function(x) { as.character(x)})
    colsToSelect <- unname(colsToSelect)
    colsToSelect <- colsToSelect[! colsToSelect %in% c("", NA)]
    
    fileToSave <- paste("data/inputData/", fileToSave, sep="", collapse="")
    
    #filter data according to config
    filtColRect <- paste(modelConfig$filter_colRec)
    filtNeoCt <- paste(modelConfig$filter_neoCT)
    filtStage <- paste(modelConfig$filter_stage)
    
    
    fileTrain <- paste(fileToSave,"_train.csv", sep="", collapse="")
    fileTest <- paste(fileToSave,"_test.csv", sep="", collapse="")
    # read csv file and create dataframe from it
    training <- read.csv(fileTrain, header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    test <- read.csv(fileTest, header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    
    curData <- training
    curData <- filterbyGroups(curData, neoCT = filtNeoCt, colRec = filtColRect, stage = filtStage)
    curData <- curData[colsToSelect]
    curData <- call_dcf_function(curData, paste(modelConfig$DataChangeFunction))
    curData<- na.omit(curData)
    createInputCsv(curData, fileTrain)
    
    curData <- test
    curData <- filterbyGroups(curData, neoCT = filtNeoCt, colRec = filtColRect, stage = filtStage)
    curData <- curData[colsToSelect]
    curData <- call_dcf_function(curData, paste(modelConfig$DataChangeFunction))
    curData<- na.omit(curData)
    createInputCsv(curData, fileTest)
    
  }
  
}



buildDataFilesNoFeatureSelection <- function(myConfig, experimentsToRun){
  # builds the files for each experiment:
  # - selecting appropriate features
  # - filtering data according to config
  # - and calling the appropriate dcf (data change function) from the config

  for(i in experimentsToRun){
    
    modelConfig <- myConfig[i,]
    fileToSave <- ""
    fileToSave <- paste(modelConfig$inputFile)
    colsToSelect <- grepl("inputVar", colnames(modelConfig))
    colsToSelect <- modelConfig[,colsToSelect]
    colsToSelect <- sapply(colsToSelect, function(x) { as.character(x)})
    colsToSelect <- unname(colsToSelect)
    colsToSelect <- colsToSelect[! colsToSelect %in% c("", NA)]
    
    fileToSave <- paste("data/inputData/", fileToSave, sep="", collapse="")
    
    #filter data according to config
    filtColRect <- paste(modelConfig$filter_colRec)
    filtNeoCt <- paste(modelConfig$filter_neoCT)
    filtStage <- paste(modelConfig$filter_stage)
    
    #load training and test data
    training <- read.csv("data/inputData/training_all.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    test <- read.csv("data/inputData/test_all.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    
    if(modelConfig$filt_deathByCancer == "yes"){
      training <- filterForDeathByCancer(training)
      test <- filterForDeathByCancer(test)
    }
    
    
    training <- filterbyGroups(training, neoCT = filtNeoCt, colRec = filtColRect, stage = filtStage)
    training <- training[colsToSelect]
    training <- call_dcf_function(training, paste(modelConfig$DataChangeFunction))
    
    test <- filterbyGroups(test, neoCT = filtNeoCt, colRec = filtColRect, stage = filtStage)
    test <- test[colsToSelect]
    test <- call_dcf_function(test, paste(modelConfig$DataChangeFunction))
    
    training <- na.omit(training)
    test <- na.omit(test)
    
    fileToSaveTrain <- paste(fileToSave,"_train.csv", sep="", collapse="")
    fileToSaveTest <- paste(fileToSave,"_test.csv", sep="", collapse="")
    createInputCsv(training, fileToSaveTrain)
    createInputCsv(test, fileToSaveTest)
 
  }
}

buildDataFilesForFeatureSelection <- function(myConfig, experimentsToRun){
  # builds the files for feature selection:
  # - selecting appropriate features
  # - filtering data according to config
  # - and calling the appropriate dcf (data change function) from the config
  # - splits data into training, tset and feature selection set
  
  for(i in experimentsToRun){
    
    modelConfig <- myConfig[i,]
    fileToSave <- ""
    fileToSave <- paste(modelConfig$inputFile)
    colsToSelect <- grepl("inputVar", colnames(modelConfig))
    colsToSelect <- modelConfig[,colsToSelect]
    colsToSelect <- sapply(colsToSelect, function(x) { as.character(x)})
    colsToSelect <- unname(colsToSelect)
    colsToSelect <- colsToSelect[! colsToSelect %in% c("", NA)]
    
    fileToSave <- paste("data/inputData/", fileToSave, sep="", collapse="")
    
    #filter data according to config
    filtColRect <- paste(modelConfig$filter_colRec)
    filtNeoCt <- paste(modelConfig$filter_neoCT)
    filtStage <- paste(modelConfig$filter_stage)
    
    training <-  read.csv("data/inputData/fs_training_all.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    test <- read.csv("data/inputData/fs_test_all.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    fs <- read.csv("data/inputData/fs_fs_all.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
    
    fileList <- list(training, test, fs)
    
    i <- 1
    for(file in fileList){
      curData <- file
      curData <- filterbyGroups(curData, neoCT = filtNeoCt, colRec = filtColRect, stage = filtStage)
      curData <- curData[colsToSelect]
      curData <- call_dcf_function(curData, paste(modelConfig$DataChangeFunction))
      curData<- na.omit(curData)
      fileList[[i]] <- curData
      i <- i+1
    }

    training <- fileList[[1]]
    test <- fileList[[2]]
    fs <- fileList[[3]]
    
    fileToSaveTrain <- paste(fileToSave,"_train.csv", sep="", collapse="")
    fileToSaveTest <- paste(fileToSave,"_test.csv", sep="", collapse="")
    fileToSaveFs <- paste(fileToSave,"_featureSelection.csv", sep="", collapse="")
    createInputCsv(training, fileToSaveTrain)
    createInputCsv(test, fileToSaveTest)
    createInputCsv(fs, fileToSaveFs)
  }
}


prepDataForFeatureSelection <- function(data, colsToIgnore = c("")){
  # prepares data for feature selection excluding :
  # - non-sensical features
  # - near zero variables
  # - variables with too many NA values
  
  training <-  read.csv("data/inputData/fs_training_all.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  test <- read.csv("data/inputData/fs_test_all.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  fs <- read.csv("data/inputData/fs_fs_all.csv", header = TRUE, sep = ";", quote = "\"",dec = ".", fill = TRUE, comment.char = "")
  
  fileList <- list(training, test, fs)
  i <- 1
  
  for(file in fileList){
    selFeatures <- data
    
    noDateCols <- ! grepl("datum", colnames(selFeatures))
    selFeatures <- selFeatures[,noDateCols]
    file <- file[,noDateCols]
  
    noIdCols <- ! grepl("docid", colnames(selFeatures))
    selFeatures <- selFeatures[,noIdCols]
    file <- file[,noIdCols]
    
    noIdCols <- ! grepl("stichtag", colnames(selFeatures))
    selFeatures <- selFeatures[,noIdCols]
    file <- file[,noIdCols]
    
    noNoteCols <- ! grepl("bemerkung", colnames(selFeatures))
    selFeatures <- selFeatures[,noNoteCols]
    file <- file[,noNoteCols]
    
    noNoteCols <- ! grepl("UICC_Stage", colnames(selFeatures))
    selFeatures <- selFeatures[,noNoteCols]
    file <- file[,noNoteCols]
    
    selFeatures <- selFeatures[,!(colnames(selFeatures) %in% c("v971_1_mnppp01status_bloecke_verfueg", "v971_1_mnppp01status_schnitte_verfueg"))]
    file <- file[,!(colnames(file) %in% c("v971_1_mnppp01status_bloecke_verfueg", "v971_1_mnppp01status_schnitte_verfueg"))]
    
    selFeatures <- selFeatures[, !names(selFeatures) %in% c("PatID")]
    file <- file[, !names(file) %in% c("PatID")]
    
    nearZeroVars <- nearZeroVar(selFeatures, names = TRUE)
    selFeatures <- selFeatures[, !names(selFeatures) %in% nearZeroVars]
    file <- file[, !names(file) %in% nearZeroVars]
    
    colsToDelete <- getColsWithTooManyBadVals(selFeatures, 10, colsToIgnore)
    selFeatures <- selFeatures[, !names(selFeatures) %in% colsToDelete]
    file <- file[, !names(file) %in% colsToDelete]
    seed <- 7
    set.seed(seed)
    selFeatures<- na.omit(selFeatures)
    file <- na.omit(file)
    fileList[[i]] <- file
    i <- i +1
  }
  
  training <- fileList[[1]]
  test <- fileList[[2]]
  fs <- fileList[[3]]
  
  return(list(training=training,test=test, fs = fs))
}

changeType <- function(dataFrame, aColumnNames, type){
  # wrapper to change data type of a column from a dataframe
  
  for (cName in aColumnNames){
    print(typeof(dataFrame[,cName]))
    switch(type,
           "numeric"={ 
             if(typeof(dataFrame[,cName]) != "numeric" && typeof(dataFrame[,cName]) != "double" ) {
               dataFrame[,cName] <- lapply(dataFrame[cName],function(x) as.numeric(levels(x)[x]))
             }
           },
           "factor"= { dataFrame[,cName] <- as.factor(dataFrame[,cName])}
    )
  }
  
  return(dataFrame)
}


splitData <- function(seed, data){
  # randomly splits data into training (0.8) and test (0.2) data
  
  set.seed(seed)
  training_index <- createDataPartition(data[[ncol(data)]], p = 0.8, list = F)
  training <- data[training_index,]
  test <- data[-training_index,]
  
  return(list(training=training,test=test))
}

splitDataFs <- function(seed, data){
  # randomly splits data into training (0.8) and test (0.2) data
  selFeatures <- data
  
  training_index <- createDataPartition(selFeatures[[ncol(selFeatures)]], p = 0.6, list = F)
  training <- selFeatures[training_index,]
  
  restData <- selFeatures[-training_index,]
  test_index <- createDataPartition(restData[[ncol(restData)]], p = 0.5, list = F)
  test <- restData[test_index,]
  featureSelection <- restData[-test_index,]
  return(list(training=training,test=test, fs = featureSelection))
}


removeHighlyCorrelated <- function(data, seed, outcomeColName){
  # prepares data for feature selection excluding :
  # - non-sensical features
  # - near zero variables
  # - variables with too many NA values
  
  
  selCorrelMat <-  subset(data, select = -eval(parse(text=outcomeColName)))     ## remove outcomeColFor processing
  #make columns numeric
  selCorrelMat <- sapply( selCorrelMat, as.numeric)
  
  print(selCorrelMat)
  
  #find index of columns that are highly correlated
  correlationMatrix <- cor(selCorrelMat)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated<-findCorrelation(correlationMatrix, cutoff=0.75)
  # remove highly correlated columns
  trainingCorrelRem<-selCorrelMat[,-c(highlyCorrelated)]
  #turn from matrix into data frame 
  trainingCorrelRem<-as.data.frame(trainingCorrelRem)
  #add class back into data
  trainingDataPreSelected <- trainingCorrelRem
  trainingDataPreSelected[,outcomeColName] <- training[,outcomeColName]
  
  return(trainingDataPreSelected)
}

removeHighlyCorrelatedFs <- function(data, testData,trainData, seed, outcomeVec){
  # removes variables highly correlated in data from data, testData and trainData for feature selection
  
  selCorrelMatAll <- data;
  testDataNoOutcomeCols <- testData
  trainDataNoOutcomeCols <- trainData
  
  for(outcomeColName in outcomeVec){
    selCorrelMatAll <-  subset(selCorrelMatAll, select = -eval(parse(text=outcomeColName)))
    testDataNoOutcomeCols<-  subset(testDataNoOutcomeCols, select = -eval(parse(text=outcomeColName)))
    trainDataNoOutcomeCols <- subset(trainDataNoOutcomeCols, select = -eval(parse(text=outcomeColName)))
  }
  
  ## remove outcomeColFor processing
  #make columns numeric
  selCorrelMat <- selCorrelMatAll[,sapply(selCorrelMatAll, is.numeric)]
  selNoCorrelMat_fs <- selCorrelMatAll[,sapply(colnames(selCorrelMatAll), function(x) {ifelse(is.numeric(x), FALSE, TRUE)})]
  selNoCorrelMat_test <- testDataNoOutcomeCols[,sapply(colnames(testDataNoOutcomeCols), function(x) {ifelse(is.numeric(x), FALSE, TRUE)})]
  selNoCorrelMat_train <- trainDataNoOutcomeCols[,sapply(colnames(trainDataNoOutcomeCols), function(x) {ifelse(is.numeric(x), FALSE, TRUE)})]
  
  #find index of columns that are highly correlated
  correlationMatrix <- cor(selCorrelMat)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated<-findCorrelation(correlationMatrix, cutoff=0.75)
  # remove highly correlated columns
  fsCorrelRem<-selCorrelMat[,-c(highlyCorrelated)]
  #turn from matrix into data frame 
  fsCorrelRem<-as.data.frame(fsCorrelRem)
  #add class back into data
  fsDataPreSelected <- fsCorrelRem
  fsDataPreSelected[, names(selNoCorrelMat_fs)] <- selNoCorrelMat_fs[, names(selNoCorrelMat_fs)]
  fsDataPreSelected[,outcomeVec] <- data[,outcomeVec]
  testDataPreSelected <- testDataNoOutcomeCols[,-c(highlyCorrelated)]
  testDataPreSelected[, names(selNoCorrelMat_test)] <- selNoCorrelMat_test[, names(selNoCorrelMat_test)]
  testDataPreSelected[,outcomeVec] <- testData[,outcomeVec]
  trainDataPreSelected <- trainDataNoOutcomeCols[,-c(highlyCorrelated)]
  trainDataPreSelected[, names(selNoCorrelMat_train)] <- selNoCorrelMat_train[, names(selNoCorrelMat_train)]
  trainDataPreSelected[,outcomeVec] <- trainData[,outcomeVec]
  
  return(list(fsDataPreSelected, trainDataPreSelected, testDataPreSelected ))
}

removeHighlyCorrelatedVec <- function(data, testData,trainData, seed, outcomeVec){
  # removes variables highly correlated in data from data, testData and trainData for feature selection
  
  selCorrelMatAll <- data;
  testDataNoOutcomeCols <- testData
  
  for(outcomeColName in outcomeVec){
    selCorrelMatAll <-  subset(selCorrelMatAll, select = -eval(parse(text=outcomeColName)))
    testDataNoOutcomeCols<-  subset(testDataNoOutcomeCols, select = -eval(parse(text=outcomeColName)))
  }
  
  ## remove outcomeColFor processing
  #make columns numeric
  selCorrelMat <- selCorrelMatAll[,sapply(selCorrelMatAll, is.numeric)]
  selNoCorrelMat_training <- selCorrelMatAll[,sapply(colnames(selCorrelMatAll), function(x) {ifelse(is.numeric(x), FALSE, TRUE)})]
  selNoCorrelMat_test <- testDataNoOutcomeCols[,sapply(colnames(testDataNoOutcomeCols), function(x) {ifelse(is.numeric(x), FALSE, TRUE)})]

  
  #find index of columns that are highly correlated
  correlationMatrix <- cor(selCorrelMat)
  # find attributes that are highly corrected (ideally >0.75)
  highlyCorrelated<-findCorrelation(correlationMatrix, cutoff=0.75)
  # remove highly correlated columns
  trainingCorrelRem<-selCorrelMat[,-c(highlyCorrelated)]
  #turn from matrix into data frame 
  trainingCorrelRem<-as.data.frame(trainingCorrelRem)
  #add class back into data
  trainingDataPreSelected <- trainingCorrelRem
  trainingDataPreSelected[, names(selNoCorrelMat_training)] <- selNoCorrelMat_training[, names(selNoCorrelMat_training)]
  trainingDataPreSelected[,outcomeVec] <- data[,outcomeVec]
  testDataPreSelected <- testDataNoOutcomeCols[,-c(highlyCorrelated)]
  testDataPreSelected[, names(selNoCorrelMat_test)] <- selNoCorrelMat_test[, names(selNoCorrelMat_test)]
  testDataPreSelected[,outcomeVec] <- testData[,outcomeVec]
  
  
  
  return(list(trainingDataPreSelected, testDataPreSelected))
}

splitColonRectum <- function(data){
  # split function that splits data according to colon and rectum patients
  
  colonData <- data[data$colon_rectum %in%c("Colon"),]
  rectumData <-data[data$colon_rectum %in%c("Rectum"),]
  
  return(list(colonData, rectumData))
  
}


createColumnAndMatch <- function(data, andCols, andConditions, caseTrueVal = TRUE, caseFalseVal = FALSE, caseTrueColName = FALSE, caseFalseColName =FALSE){
  # helper function, which applies and conditions to multiple columns in a dataframe 
  # and fills a new vector according to the Case true and false values given
  
  newVec <- c()
  
  for(i in 1:nrow(data)){
    
    if(caseTrueColName != FALSE){
      newVec[i] <- data[i,caseTrueColName]
    } else {
      newVec[i] <- caseTrueVal
    }
    
    for(j in 1:length(andCols)){
      
      andCol <- andCols[j]
      
      if(regexpr(andConditions[j],data[i,andCol]) == -1){
        
        if(caseFalseColName != FALSE){
          newVec[i] <- data[i,caseFalseColName]
        } else {
          newVec[i] <- caseFalseVal
        }
        
        
        break;
      }
      
    }
    
  }
  
  return (newVec)
}


createRiskGroupingCols <- function(data){
  # creates risk groups vectors and adds them to data, which is then returned
  
  # create stage II risk groupings
  dataWithRiskGroups <- data
  
  andCols <- c('v971_1_mnppp01_33_patho_infiltrationstiefe', 'def_stadium_I_IV')
  andConditions <- c('[3][1-2]', '^II$')
  
  newVec <- createColumnAndMatch(data, andCols, andConditions, 'LR', 'HR')
  dataWithRiskGroups$IIRiskGroup <- newVec
  
  andCols <- c('def_stadium_I_IV')
  andConditions <- c('^II$')
  dataWithRiskGroups$IIRiskGroup <- createColumnAndMatch(dataWithRiskGroups, andCols, andConditions, 'NA', 'NA', caseTrueColName = 'IIRiskGroup')
  
  #create stage III risk groupings
  
  andCols <- c('v971_1_mnppp01_32_postop_def_stadium')
  andConditions <- c('^131|^031')
  
  newVec <- createColumnAndMatch(dataWithRiskGroups, andCols, andConditions, 'IIIA', 'NA')
  dataWithRiskGroups$IIIRiskGroup <- newVec
  
  andConditions <- c('^032|^132')
  newVec <- createColumnAndMatch(dataWithRiskGroups, andCols, andConditions, 'IIIB', 'hR',caseFalseColName = 'IIIRiskGroup')
  dataWithRiskGroups$IIIRiskGroup <- newVec
  
  andConditions <- c('^133|^033')
  newVec <- createColumnAndMatch(dataWithRiskGroups, andCols, andConditions, 'IIIC', 'hR',caseFalseColName = 'IIIRiskGroup')
  dataWithRiskGroups$IIIRiskGroup <- newVec
  
  return(dataWithRiskGroups)
}


filterbyGroups <- function(data, neoCT = "ALL", colRec = "ALL", stage = "ALL"){
  # filter wrapper that filters data by neoCT, colRec and stage
  
  filtData <- data
  
  
  if(neoCT == "YES"){
    filtData <- filtData[filtData$nachsorge_chemo %in%c("1 (yes)"), ]
  }
  
  if(neoCT == "NO"){
    filtData <- filtData[!filtData$nachsorge_chemo %in%c("1 (yes)"), ]
  }
  
  if(colRec == "col"){
    filtData <- filtData[filtData$colon_rectum %in%c("Colon"), ]
  }
  
  if(colRec == "rec"){
    filtData <- filtData[filtData$colon_rectum %in%c("Rectum"), ]
  }
  
  
  if(stage == "II"){
    filtData <- filtData[filtData$def_stadium_I_IV %in%c("II"), ]
  }
  
  if(stage == "III"){
    filtData <- filtData[filtData$def_stadium_I_IV %in%c("III", "yIII"), ]
  }
  
  if(stage == "NOTIV"){
    filtData <- filtData[!filtData$def_stadium_I_IV %in%c("yIV", "IV"), ]
  }
  
  return(filtData)
  
}


filterForDeathByCancer <- function(data) {
  
  filtData <- data
  filtData <- filtData[!filtData$patient_todesursache %in% c("5 (aus anderer Ursache)", "7 (unbekannt)"),]
  return(filtData)  
}


call_dcf_function <- function(data, funcToCall){
  # wrapper function to implement the general calling of a function according to string given
  
  if(funcToCall == "dcf_selectTumors_tumorRegression") {
    data <- dcf_selectTumors_tumorRegression(data)
  }
  
  if(funcToCall == "dcf_dfs_event_factor") {
    data <- dcf_dfs_event_factor(data)
  }
  
  if(funcToCall == "dcf_alldata_dfs") {
    data <- dcf_alldata_dfs(data)
  }
  
  if(funcToCall == "dcf_alldata_surv") {
    data <- dcf_alldata_surv(data)
  }
  
  if(funcToCall == "dcf_alldata_rct") {
    data <- dcf_alldata_rct(data)
  }
  
  if(funcToCall == "dcf_alldata_r2") {
    data <- dcf_alldata_r2(data)
  }
  
  if(funcToCall == "dcf_alldata_r3") {
    data <- dcf_alldata_r3(data)
  }
  
  if(funcToCall == "dcf_alldata_relapse") {
    data <- dcf_alldata_relapse(data)
  }
  
  if(funcToCall == "dcf_alldata_tstage") {
    data <- dcf_alldata_tstage(data)
  }
    
  if(funcToCall == "dcf_rct_yesNo"){
    data <- dcf_rct_yesNo(data)
  }
  
  if(funcToCall == "dcf_less_tumor_stages"){
    data <- dcf_less_tumor_stages(data)
  }
  
  return(data)
}


dcf_dfs_event_factor <- function(data){
  data$DFS_Event <- sapply(data$DFS_Event, function(x) { ifelse(x == "TRUE", "YES", "NO")})
  data$DFS_Event <- as.factor(data$DFS_Event)
  return(data)
}

dcf_selectTumors_tumorRegression <- function(data){
  
  data$v971_1_mnppp01_33_patho_am_primaertumor <- sapply(strsplit(as.character(data$v971_1_mnppp01_33_patho_am_primaertumor), split=' ', fixed=TRUE), function(x) (x[1]))
  data <- na.omit(data)
  data <- data[!data$v971_1_mnppp01_33_patho_am_primaertumor %in%c("8", "9"), ]
  data <- data[data$v971_1_mnppp01_33_patho_am_primaertumor %in%c("0", "1","2","3","4"), ]
  data$v971_1_mnppp01_33_patho_am_primaertumor <- sapply(data$v971_1_mnppp01_33_patho_am_primaertumor, function(x) { ifelse(x == "0" , "lv0", x)})
  data$v971_1_mnppp01_33_patho_am_primaertumor <- sapply(data$v971_1_mnppp01_33_patho_am_primaertumor, function(x) { ifelse(x == "1" , "lv1", x)})
  data$v971_1_mnppp01_33_patho_am_primaertumor <- sapply(data$v971_1_mnppp01_33_patho_am_primaertumor, function(x) { ifelse(x == "2" , "lv2", x)})
  data$v971_1_mnppp01_33_patho_am_primaertumor <- sapply(data$v971_1_mnppp01_33_patho_am_primaertumor, function(x) { ifelse(x == "3" , "lv3", x)})
  data$v971_1_mnppp01_33_patho_am_primaertumor <- sapply(data$v971_1_mnppp01_33_patho_am_primaertumor, function(x) { ifelse(x == "4" , "lv4", x)})
  return(data)
}


dcf_alldata_dfs <- function (data) {
  data <- data[,!(colnames(data) %in% c("patient_ueberlebenszeit", "patient_lebensstatus"))]
  return(data)
}

dcf_alldata_surv <- function (data) {
  
  data <- data[,!(colnames(data) %in% c("DFS_Event", "disease_free_survival_time"))]

  
  return(data)
}

dcf_alldata_rct <- function (data) {
  data <- data[,!(colnames(data) %in% c("DFS_Event", "disease_free_survival_time", "patient_ueberlebenszeit", "patient_lebensstatus"))]
  
  data <- dcf_selectTumors_tumorRegression(data)
  data$v971_1_mnppp01_33_patho_am_primaertumor <- as.factor(data$v971_1_mnppp01_33_patho_am_primaertumor)
  print("reached dcf rct all")
  return(data)
  
  
}

dcf_alldata_r2 <- function (data) {
  
  data <- createRiskGroupingCols(data)
  
  data <- data[,!(colnames(data) %in% c("DFS_Event", "disease_free_survival_time", "patient_ueberlebenszeit", "patient_lebensstatus", "IIIRiskGroup"))]
  data <- filterbyGroups(data, stage = "II")
  data$IIRiskGroup <- as.factor(data$IIRiskGroup)
  return(data)
}

dcf_alldata_r3 <- function (data) {
  data <- createRiskGroupingCols(data)
  data <- data[,!(colnames(data) %in% c("DFS_Event", "disease_free_survival_time", "patient_ueberlebenszeit", "patient_lebensstatus", "IIRiskGroup"))]
  data$IIIRiskGroup <- as.factor(data$IIIRiskGroup)
  data <- filterbyGroups(data, stage = "III")
  return(data)
}

dcf_alldata_relapse <- function (data) {
  data <- data[,!(colnames(data) %in% c("disease_free_survival_time", "patient_ueberlebenszeit", "patient_lebensstatus"))]
  
  data <- dcf_dfs_event_factor(data)
  return(data)
}

dcf_alldata_tstage <- function(data){
  data$def_stadium_I_IV <- as.factor(data$def_stadium_I_IV)
  #data <- droplevels(data)
  return(data)
}

dcf_rct_yesNo <- function(data){
  data$v971_1_mnppp01_33_patho_am_primaertumor <- sapply(strsplit(as.character(data$v971_1_mnppp01_33_patho_am_primaertumor), split=' ', fixed=TRUE), function(x) (x[1]))
  data <- na.omit(data)
  data <- data[!data$v971_1_mnppp01_33_patho_am_primaertumor %in%c("8", "9"), ]
  data <- data[data$v971_1_mnppp01_33_patho_am_primaertumor %in%c("0", "1","2","3","4"), ]
  data$v971_1_mnppp01_33_patho_am_primaertumor <- sapply(data$v971_1_mnppp01_33_patho_am_primaertumor, function(x) { ifelse(x == "3" || x == "4" , "YES", "NO")})
  data$v971_1_mnppp01_33_patho_am_primaertumor <- as.factor(data$v971_1_mnppp01_33_patho_am_primaertumor)
  
  return(data)
}

dcf_less_tumor_stages <- function(data){
  data$def_stadium_I_IV <- as.character(data$def_stadium_I_IV)
  data$def_stadium_I_IV <- sapply(data$def_stadium_I_IV, function(x) { ifelse(x == "yI" , "I", x)})
  data$def_stadium_I_IV <- sapply(data$def_stadium_I_IV, function(x) { ifelse(x == "yII" , "II", x)})
  data$def_stadium_I_IV <- sapply(data$def_stadium_I_IV, function(x) { ifelse(x == "yIII" , "III", x)})
  data$def_stadium_I_IV <- sapply(data$def_stadium_I_IV, function(x) { ifelse(x == "yIV" , "IV", x)})
  data <- data[data$def_stadium_I_IV %in%c("I","II", "III", "IV"), ]
  data$def_stadium_I_IV <- as.factor(data$def_stadium_I_IV)
  
  return(data)
}
