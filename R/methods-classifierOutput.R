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

setGeneric("trainInd", function(x) standardGeneric("trainInd"))
setMethod("trainInd", "classifierOutput", function(x) x@trainInd)

setGeneric("testScores", function(x) standardGeneric("testScores"))
setMethod("testScores", "classifierOutput", function(x) x@testScores)

setGeneric("trainScores", function(x) standardGeneric("trainScores"))
setMethod("trainScores", "classifierOutput", function(x) x@trainScores)


## recall, precision and macroF1 methods - L. Gatto <lg390@cam.ac.uk>, 30 July 2011
setGeneric("recall", function(obj, type,...) standardGeneric("recall"))
setGeneric("precision", function(obj, type,...) standardGeneric("precision"))
setGeneric("macroF1", function(obj, type,...) standardGeneric("macroF1"))

setGeneric("sensitivity",
           function(obj, type,...) {
             if (class(obj) == "table") {
               recall(obj, ...)
             } else {
               recall(obj, type, ...)
             }
           })


setMethod("recall",
          c("classifierOutput","missing"),
          function(obj, type, ...) return(.recall(confuMat(obj, "test", ...))))

setMethod("recall",
          c("classifierOutput","character"),
          function(obj, type, ...) return(.recall(confuMat(obj, type, ...))))

setMethod("recall",
          c("classifierOutput","numeric"),
          function(obj, type) return(.recall(confuMat(obj, "test", type))))

setMethod("precision",
          c("classifierOutput","missing"),
          function(obj, type, ...) return(.precision(confuMat(obj, "test", ...))))

setMethod("precision",
          c("classifierOutput","character"),
          function(obj, type, ...) return(.precision(confuMat(obj, type, ...))))

setMethod("precision",
          c("classifierOutput","numeric"),
          function(obj, type) return(.precision(confuMat(obj, "test", type))))

setMethod("macroF1",
          c("classifierOutput","missing"),
          function(obj, type, ...) {
            p <- precision(obj, "test", ...)
            r <- recall(obj, "test", ...)
            return(.macroF1(p,r))
          })

setMethod("macroF1",
          c("classifierOutput","character"),
          function(obj, type, ...) {
            p <- precision(obj, type, ...)
            r <- recall(obj, type, ...)
            return(.macroF1(p,r))
          })

setMethod("macroF1",
          c("classifierOutput","numeric"),
          function(obj, type) {
            p <- precision(obj, "test", type)
            r <- recall(obj, "test", type)
            return(.macroF1(p,r))
          })



# threshold-related code added by L. Gatto 2011 Brixen
setGeneric("confuMat", function(obj,type,...) standardGeneric("confuMat"))
setMethod("confuMat", c("classifierOutput","missing"), function(obj,type, ...) {
    confuMat(obj, "test", ...) })

setMethod("confuMat", c("classifierOutput","numeric"), function(obj,type) {
  confuMat(obj, "test", type) }) ## 'type' is 't' here

setMethod("confuMat", c("classifierOutput","character"), 
          function (obj, type, t) {  ## revised brixen 2011 to give better output table column order
            ## we are banking hard on the use of factors to represent response and
            ## predictions
            if (type == "test") {
              if (missing(t)) predictions <- testPredictions(obj)
              else predictions <- testPredictions(obj,t)
              giv = obj@testOutcomes
            } else if (type == "train") {
              if (missing(t)) predictions <- trainPredictions(obj)
              else predictions <- trainPredictions(obj,t)
              giv = obj@trainOutcomes
            } else stop("non-missing type must be either 'test' or 'train'")
            templ = table(given=giv, predicted=giv)
            templ = templ * 0
            if (all(is.na(levels(predictions))))
              return(cbind(templ,'NA'=table(giv)))
            if (length(giv) == 0) 
              stop("there is no test set in this classifier output")
            ans = table(given = giv, predicted = predictions)
            colnames(ans)[which(is.na(colnames(ans)))] <- 'NA' ## fixes 'strange' bug
            if (ncol(templ)!=ncol(ans)) {
              ## this is generally the case when some items have not been
              ## predicted with a score >= t and have been returned as NA
              ## predictions. Adding that column to the template.
              oldcolnames <- colnames(templ)
              templ <-  cbind(templ,0)
              colnames(templ) <- c(oldcolnames,'NA')
            }
            if (!isTRUE(all.equal(dim(ans), dim(templ)))) {
              used = colnames(ans)
              for (i in 1:ncol(ans)) templ[, used[i]] = ans[, used[i]]
              ans = templ
            }
            if (all(colnames(ans) %in% levels(giv))) return(ans[, levels(giv)])  # can reorder
            return(ans)
          })

#setGeneric("testPredictions", function(x) standardGeneric("testPredictions"))
#setMethod("testPredictions", "classifierOutput", function(x) x@testPredictions)

## modified by L. Gatto to allow setting a threshold and
## adjust the predictions dynamically
setGeneric("testPredictions", function(x,...) standardGeneric("testPredictions"))
setMethod("testPredictions", "classifierOutput", function(x,t) {
  tePredictions <- x@testPredictions
  if (missing(t)) {
    return(tePredictions)
  } else {
    teScores <- testScores(x)
    if (is.vector(teScores)) {
      tePredictions[teScores<t] <- NA
      return(factor(tePredictions,exclude=NULL))
    }
    ## assuming teScores is a matrix with
    ## columns named to classes
    if (!is.matrix(teScores)) {
      warning("testScores is not of class ",class(teScores),
              ", expecting vector or matrix - no threshold applied.")
      return(x@testPredictions)
    }
    teScores[teScores<t] <- -1
    clindex <- apply(teScores,1,function(x) {
      k <- names(which(max(x)==x))
      ## ties (?) or no value >= t - returning 'NA'
      ifelse((length(k)!=1 | is.null(k)),NA,k)
    })
    return(factor(clindex,exclude=NULL))
  }
})


#setGeneric("trainPredictions", function(x) standardGeneric("trainPredictions"))
#setMethod("trainPredictions", "classifierOutput", function(x) x@trainPredictions)

## modified by L. Gatto to allow setting a threshold and
## adjust the predictions dynamically
setGeneric("trainPredictions", function(x,...) standardGeneric("trainPredictions"))
setMethod("trainPredictions", "classifierOutput", function(x,t) {
  trPredictions <- x@trainPredictions
  if (missing(t)) {
    return(trPredictions)
  } else {
    trScores <- x@trainScores
    if (is.vector(trScores)) {
      trPredictions[trScores<t] <- NA
      return(factor(trPredictions,exclude=NULL))
    }
    ## assuming trScores is a matrix with
    ## columns named to classes
    if (!is.matrix(trScores)) {
      warning("testScores is of class ",class(trScores),
              ", expecting vector or matrix - no threshold applied.")
      return(x@trainPredictions)
    }
    trScores[trScores<t] <- -1
    clindex <- apply(trScores,1,function(x) {
      k <- names(which(max(x)==x))
      ## ties (?) or no value >= t - returning NA
      ifelse((length(k)!=1 | is.null(k)),NA,k)
    })
    return(factor(clindex,exclude=NULL))
  }
})


setGeneric("predictions", function(x,...) standardGeneric("predictions"))
setMethod("predictions", "classifierOutput", function(x,t) {
  trainInd <- x@trainInd
  if (missing(t))
    t <- 0
  trout <- as.character(x@trainOutcomes)
  tepred <- as.character(testPredictions(x,t))
  out <- rep(NA,length(trout)+length(tepred))
  out[trainInd] <- trout
  out[-trainInd] <- tepred
  return(factor(out))
})

setGeneric("predScore", function(x,...) standardGeneric("predScore"))
setMethod("predScore", "classifierOutput", function(x) {
  trainInd <- x@trainInd
  n <- length(x@trainOutcomes) + length(x@testOutcomes)
  out <- rep(1,n)
  trscores <- testScores(x)
  if (is.matrix(trscores)) {
    out[-trainInd] <- rowMax(trscores)
  } else {
    out[-trainInd] <- trscores
  }
  return(out)
})


setGeneric("predScores", function(x,...) standardGeneric("predScores"))
setMethod("predScores", "classifierOutput", function(x) {
  tescores <- testScores(x)
  if (!is.matrix(tescores)) {
    ans <- matrix(predScore(x), ncol = 1)
  } else {
    n <- nrow(tescores) + nrow(trainScores(x))
    trainOut <- as.character(x@trainOutcomes)
    testOut <- as.character(x@testOutcomes)
    trainInd <- x@trainInd
    testInd <- (1:n)[ -trainInd ]
    ans <-  matrix(0,
                   nrow = n,
                   ncol = ncol(tescores))   
    colnames(ans) <- colnames(tescores)
    rownames(ans)[testInd] <- rownames(tescores)
    rownames(ans)[trainInd] <- rownames(trainScores(x))
    ## updating test scores as returned by MLInterfaces::testScores
    stopifnot(length(testInd) == length(testOut))
    ans[testInd, ] <- tescores
    ## updating train scores - setting appropriate cell to 1
    for (i in 1:length(trainInd))
      ans[trainInd[i], trainOut[i]] <- 1
  }
  return(ans)
})


setGeneric("fsHistory", function(x) standardGeneric("fsHistory"))
setMethod("fsHistory", "classifierOutput", function(x) x@fsHistory)


predict.classifierOutput <- function(object, newdata, ...) {
  if (class(newdata)[1] == "ExpressionSet")
    newdata <- data.frame(t(exprs(newdata)))
  if (class(newdata)[1] == "matrix")
    newdata <- data.frame(newdata)
  model <- RObject(object)
  if (class(model)[1]=="list")
    stop("The 'classifierOutput' has", length(model),
         "models.", "Expecting only 1.")
  predict <- object@learnerSchema@predicter ## MLIPredicter
  ans <- predict(model, newdata, ...)
  ## ans is a list(testPredictions, testScores)
  ## testScores is numeric() and testPredictions should be
  ## factor(,levels=levels(trainPredictions))
  ## not sure if all original levels will always be included
  ## and possible downstream repercutions
  ## Will be more elaborated at a later stage
  return(ans)
}
