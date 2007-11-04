setMethod("show", "classifierOutput", function(object) {
 cat("MLInterfaces classification output container\n")
 cat("The call was:\n")
 print(object@call)
 cat("Predicted outcome distribution for test set:\n")
 print(table(testPredictions(object)))
 if (length(tsco <- object@testScores)>0) {
  cat("Summary of scores on test set (use testScores() method for details):\n") 
  if (is(tsco, "numeric")) print(summary(tsco))
  else if (is(tsco, "matrix")) print(apply(tsco,2,mean))
  }
 if (length(object@fsHistory)>0) cat("history of feature selection in cross-validation available; use fsHistory()\n")
})

#setGeneric("RObject", function(x) standardGeneric("RObject"))
setMethod("RObject", "classifierOutput", function(obj) obj@RObject)

setGeneric("testScores", function(x) standardGeneric("testScores"))
setMethod("testScores", "classifierOutput", function(x) x@testScores)

#setGeneric("confuMat", function(x,type) standardGeneric("confuMat"))
setMethod("confuMat", c("classifierOutput","missing"), function(obj,type)
   table(given=obj@testOutcomes, predicted=obj@testPredictions))
setMethod("confuMat", c("classifierOutput","character"), 
     function(obj,type) {
        if(type=="test")
           return(table(given=obj@testOutcomes, predicted=obj@testPredictions))
        else if(type=="train")
           return(table(given=obj@trainOutcomes, predicted=obj@trainPredictions))
 	else stop("non-missing type must be either 'test' or 'train'")
	})
      

setGeneric("testPredictions", function(x) standardGeneric("testPredictions"))
setMethod("testPredictions", "classifierOutput", function(x) x@testPredictions)

setGeneric("trainPredictions", function(x) standardGeneric("trainPredictions"))
setMethod("trainPredictions", "classifierOutput", function(x) x@trainPredictions)
setGeneric("fsHistory", function(x) standardGeneric("fsHistory"))
setMethod("fsHistory", "classifierOutput", function(x) x@fsHistory)
