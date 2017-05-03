

trainModelsCaret <- function(dataset = NULL, outcomeVar = NULL, metric = "Accuracy", control = NULL){
  # trains all the caret models according to control and metric
  
  retList <- list()
  
  form <- as.formula(paste(c(outcomeVar, "~."), collapse=" "))
  
  #set.seed(seed)
  print("start train GLMNET")
  set.seed(seed)
  retList$glmnet <- caret::train(form, data=dataset, method="glmnet", metric=metric, preProc=c("center", "scale"), trControl=control)

  # KNN (KNN - k-nearest-neighbour)
  print("start train KNN")
  set.seed(seed)
  retList$knn <- caret::train(form, data=dataset, method="knn", metric=metric, preProc=c("center", "scale"), trControl=control)

  # nnet (ANN - artificial neural network)
  print("start train ANN")
  set.seed(seed)
  retList$ann <- caret::train(form, data=dataset, method="nnet", metric=metric, trControl=control)

  #C5.0 (DT - decision tree)
  print("start train DT")
  set.seed(seed)
  retList$dt <- caret::train(form, data=dataset, method="C5.0", metric=metric, trControl=control)

  # RF (RF - random forest)
  print("start train RF")
  set.seed(seed)
  retList$rf <- caret::train(form, data=dataset, method="rf", metric=metric, trControl=control)

  #dnn
  print("start train dnn")
  set.seed(seed)
  retList$dnn <- caret::train(form, data=dataset, method="dnn", metric=metric, trControl=control)
  
  return(retList)
  
}

trainModelsMlr <- function(dataset = NULL, outcomeVars = NULL, measures = cindex){
  # trains all mlr methods
  
  # available models see: https://mlr-org.github.io/mlr-tutorial/devel/html/integrated_learners/index.html#survival-analysis-15
  
  retList <- list()
  
  #create the survival task
  learnTask = makeSurvTask(data = dataset, target = outcomeVars)
  
  # create resampling strategy
  resampleDesc = makeResampleDesc("RepCV", reps = 10)  #using 10-fold cv
  
  
  
  #build model for randomForestSrc
  #create a learner
  tM.rfsrc.learner <- makeLearner("surv.randomForestSRC",id="rfsrc", predict.type = "response", par.vals = list(nsplit=10))
  
  # set tunable params
  survTunParams <- makeParamSet(
    makeIntegerParam("ntree",lower = 100, upper = 1000),
    makeIntegerParam("nsplit", lower = 10, upper = 10)    # what if param not listed here ..will it be optimized then?
  )
  
  # make tune control
  survRanCtrl <- makeTuneControlRandom(maxit = 1L)
  
  # tuning of the model
  rfsrc_tune <- tuneParams(learner = tM.rfsrc.learner, resampling = resampleDesc, task = learnTask, par.set = survTunParams, control = survRanCtrl, measures = measures)  # use cindex for survival data later  - see also listMeasures()
  
  # build rfsrc model
  tM.rfsrc.ModelToTrain <- setHyperPars(tM.rfsrc.learner, par.vals = rfsrc_tune$x)
  
  # train rfsrc model
  print("start train rfsrc")
  tM.rfsrc = train(tM.rfsrc.ModelToTrain, learnTask)

  
  
  #build model for sruvival coxph
   
  tM.coxph.learner <- makeLearner("surv.coxph",id="coxph", predict.type = "response")
  tM.coxph.ModelToTrain <- setHyperPars(tM.coxph.learner)
  print("start train coxph")
  tM.coxph = train(tM.coxph.ModelToTrain, learnTask)
  
  #build model for survival glmnet
  
  #set tunable params
  tM.glmnet.learner <- makeLearner("surv.glmnet",id="glmnet", predict.type = "response")
  
  survTunParams <- makeParamSet(
    makeNumericParam("alpha", lower = 0, upper = 1)    # what if param not listed here ..will it be optimized then?
  )
  
  glmnet_tune <- tuneParams(learner = tM.glmnet.learner, resampling = resampleDesc, task = learnTask, par.set = survTunParams, control = survRanCtrl, measures = measures)  # use cindex for survival data later  - see also listMeasures()
  
  tM.glmnet.ModelToTrain <- setHyperPars(tM.glmnet.learner, par.vals = glmnet_tune$x)
  print("start train glmnet")
  tM.glmnet = train(tM.glmnet.ModelToTrain, learnTask)
  
  # build model for survival tree (surv.rpart)
  
  
  #add models to ret list
  retList$rfsrc = tM.rfsrc
  retList$coxph = tM.coxph
  retList$glmnet = tM.glmnet
  
  return(retList)
}


trainModelsMlrWithFs <- function(dataset = NULL, outcomeVars = NULL, measures = cindex){
  
  # available models see: https://mlr-org.github.io/mlr-tutorial/devel/html/integrated_learners/index.html#survival-analysis-15
 
  retList <- list()
  
  #create the survival task
  learnTask = makeSurvTask(data = dataset, target = outcomeVars)

  
  tM.rfsrc.learner <- makeLearner("surv.randomForestSRC",id="rfsrc", predict.type = "response", par.vals = list(nsplit=10))
  
  lrn = makeFilterWrapper(learner = tM.rfsrc.learner, fw.method = "rf.importance")
  
  resampleDescInner = makeResampleDesc("RepCV", reps = 5)
  
  #try makeFeatSelWrapper
  #lrn = makeFeatSelWrapper(tM.rfsrc.learner, resampling = resampleDescInner, control = makeFeatSelControlRandom(maxit = 10), show.info = FALSE)
  
  survTunParams <- makeParamSet(
    makeDiscreteParam("fw.abs", values = c(20)),
    makeIntegerParam("ntree",lower = 100, upper = 200),
    makeIntegerParam("nsplit", lower = 10, upper = 10)    # what if param not listed here ..will it be optimized then?
  )
  
  ctrl <- makeTuneControlRandom(maxit = 1L)
  
  resampleDescOuter = makeResampleDesc("RepCV", reps = 10)
  lrn = makeTuneWrapper(lrn, resampling = resampleDescOuter, par.set = survTunParams, control = ctrl,measures = measures,show.info = FALSE)
  
  tM.rfsrc = train(lrn, learnTask)
  
  #add models to ret list
  retList$rfsrc = tM.rfsrc
  
  return(retList)
}