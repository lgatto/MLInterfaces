setMethod("show", "classifierOutput", function(object) {
 cat("MLInterfaces classification output container\n")
 cat("The call was:\n")
 print(object@call)
 if (length(testPredictions(object))>0) {
    sl = sort(levels(testPredictions(object)))
    cat("Predicted outcome distribution for test set:\n")
    print(table(testPredictions(object))[sl])  # aesthetic for brixen 2011
 }
 if (length(tsco <- object@testScores)>0) {
  cat("Summary of scores on test set (use testScores() method for details):\n") 
  if (is(tsco, "numeric")) print(summary(tsco))
  else if (is(tsco, "matrix")) print(apply(tsco,2,mean))
  }
 if (length(object@fsHistory)>0) cat("history of feature selection in cross-validation available; use fsHistory()\n")
})

setGeneric("RObject", function(x) standardGeneric("RObject"))
setMethod("RObject", "classifierOutput", function(x) x@RObject)

setGeneric("testScores", function(x) standardGeneric("testScores"))
setMethod("testScores", "classifierOutput", function(x) x@testScores)

setGeneric("confuMat", function(obj,type) standardGeneric("confuMat"))
setMethod("confuMat", c("classifierOutput","missing"), function(obj,type) {
    confuMat(obj, "test") })
setMethod("confuMat", c("classifierOutput","character"), 
function (obj, type)   # revised brixen 2011 to give better output table column order
{
#  we are banking hard on the use of factors to represent response and
# predictions
    if (type == "test") {
        giv = obj@testOutcomes
        templ = table(given=giv, predicted=giv)
        templ = templ * 0
        if (length(giv) == 0) 
            stop("there is no test set in this classifier output")
        ans = table(given = giv, predicted = obj@testPredictions)
        if (!isTRUE(all.equal(dim(ans), dim(templ)))) {
            used = colnames(ans)
            for (i in 1:ncol(ans)) templ[, used[i]] = ans[, used[i]]
            ans = templ
        }
        if (all(colnames(ans) %in% levels(giv))) return(ans[, levels(giv)])  # can reorder
        return(ans)
    }
    else if (type == "train") {
        giv = obj@trainOutcomes
        templ = table(given=giv, predicted=giv)
        templ = templ * 0
        if (length(giv) == 0) 
            stop("there is no training set in this classifier output")
        ans = table(given = giv, predicted = obj@trainPredictions)
        if (!isTRUE(all.equal(dim(ans), dim(templ)))) {
            used = colnames(ans)
            for (i in 1:ncol(ans)) templ[, used[i]] = ans[, used[i]]
            ans = templ
        }
        if (all(colnames(ans) %in% levels(giv))) return(ans[, levels(giv)])  # can reorder
        return(ans)
    }
    else stop("non-missing type must be either 'test' or 'train'")
})
      

setGeneric("testPredictions", function(x) standardGeneric("testPredictions"))
setMethod("testPredictions", "classifierOutput", function(x) x@testPredictions)

setGeneric("trainPredictions", function(x) standardGeneric("trainPredictions"))
setMethod("trainPredictions", "classifierOutput", function(x) x@trainPredictions)
setGeneric("fsHistory", function(x) standardGeneric("fsHistory"))
setMethod("fsHistory", "classifierOutput", function(x) x@fsHistory)
