## Laurent Gatto <lg390@cam.ac.uk> -- 2 July 2011 

MLIConverter.plsda <- function(obj, data, trainInd) {
  rhs <- colnames(obj$model$x)
  trData <- data[ trainInd,rhs]
  teData <- data[-trainInd,rhs]
  tePredProb <- predict(obj,teData,type="prob")
  tePredClass <- predict(obj,teData,type="class")   
  names(tePredClass) <- rownames(teData)  
  trPredProb <- predict(obj,trData,type="prob")
  trPredClass <- predict(obj,trData,type="class")   
  names(trPredClass) <- rownames(trData)
  new("classifierOutput",
      testPredictions = tePredClass, 
      testScores = tePredProb,
      trainPredictions = trPredClass, 
      trainScores = trPredProb,
      RObject = obj)

}

plsdaI <- makeLearnerSchema("MLInterfaces", ## package
                            "plsda2",       ## function
                            MLIConverter.plsda) ## converter

plsda2 <- function (formula, 
                    data, ## training data (prepared in MLearn)
                    probMethod="Bayes", ## using bayes rule to form posterior probs (default in caret::plsda is 'softmax')
                    ncomp=2,    ## as in caret::plsda
                    prior=NULL, ## as in caret::plsda
                    ...   ## other args for plsr
                    ) {
  mf <- model.frame(formula, data)
  cl <- factor(model.response(mf))
  x <- mf[, -1]
  ans <- caret::plsda(x, cl,
                      probMethod=probMethod,
                      ncomp=ncomp,
                      prior=prior, ...)    
  ##class(ans) <- "plsda2" ## only required to define a plsda2.predict method
  return(ans)
}

