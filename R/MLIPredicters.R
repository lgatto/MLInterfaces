## standardMLIPredicter <- function(obj, newdata) {
## Using class prototype instead
## }

MLIPredicter.plsda <- function(model, newdata, ...) {
  .predClass <- predict(model,newdata,type="class") 
  .predProb <- predict(model,newdata,type="prob")
  return(list(testPredictions=.predClass,
              testScores=.predProb))
}

MLIPredicter.svm <- function(model, newdata, ...) {
  .predClass <- predict(model, newdata, decision.values=TRUE, probability=TRUE)
  .predProb <- attr(.predClass,"probabilities")
  return(list(testPredictions=factor(.predClass),
              testScores=.predProb))
}

MLIPredicter.knn <- function(model, newdata, ...) {
  .predClass <- class::knn(model$traindat, newdata,
                           model$traincl, prob=TRUE, ...)
  .predProb <- attr(.predClass, "prob")
  return(list(testPredictions=factor(.predClass),
              testScores=.predProb))
}

MLIPredicter.nnet <- function(model, newdata, ...) {
  .predClass <- predict(model, newdata, type="class")
  .predProb <- predict(model, newdata, type="raw")
  return(list(testPredictions=factor(.predClass),
              testScores=.predProb))
}

MLIPredicter.naiveBayes <- function(model, newdata, ...) {
  .predClass <- predict(model, newdata, type="class")
  .predProb <- predict(model, newdata, type="raw")
  return(list(testPredictions=.predClass,
              testScores=.predProb))
}

MLIPredicter.randomForest <- function(model, newdata, ...) {
  .predClass <- predict(model, newdata, type="response")
  .predProb <- predict(model, newdata, type="prob")
  attr(.predProb,"class") <- NULL
  return(list(testPredictions=.predClass,
              testScores=.predProb))
}

