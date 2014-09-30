setGeneric("MLearn", function(formula, data, .method, trainInd, ...) standardGeneric("MLearn"))

setMethod("MLearn",
          c("formula", "data.frame", "learnerSchema", "numeric" ),
          function( formula, data, .method, trainInd, ...) {
            ## find software
            pname = .method@packageName
            fname = .method@mlFunName
            ## create the requested function
            lfun = do.call("::", list(pname, fname))
            ## build test and train subsets
            if (length(trainInd) != nrow(data))
              tedata = gdata::drop.levels(data[-trainInd,])
            else tedata = data  # needed for xvalSpec("NOTEST") with rdacvI (only?)
            trdata = gdata::drop.levels(data[trainInd,])
            ## execute on training data 
            ans = lfun( formula, trdata, ...)
            ## collect response subsets
            trFrame = try(model.frame(formula, trdata, na.action=na.fail))
            if (inherits(trFrame, "try-error")) stop("NA encountered in data.  Please rectify.")
            teFrame = try(model.frame(formula, tedata, na.action=na.fail))
            if (inherits(teFrame, "try-error")) stop("NA encountered in data.  Please rectify.")
            trout = model.response( trFrame )
            teout = model.response( teFrame )
            ## tell what was done
            thecall = match.call()
            ## convert the execute result into an MLint output container
            tmp = .method@converter( ans, data, trainInd )
            ## add some stuff to the converted representation
            if (!tmp@embeddedCV) {
              tmp@testOutcomes = teout
              tmp@trainOutcomes = trout
            }
            else tmp@testOutcomes = trout # if CV is embedded, the 'training' is 'test'
            tmp@call = thecall
            tmp@learnerSchema = .method
            tmp@trainInd = trainInd
            tmp
          })

es2df = function(x,keep=NULL) {
#
# the keep parameter says which pData vars are kept in
#
   if (is.null(keep)) return(data.frame(t(exprs(x)),pData(x)))
   else {
        tmp = data.frame(t(exprs(x)),pData(x)[[keep]])
        names(tmp)[ncol(tmp)] = keep
        return(tmp)
        }
}

setMethod("MLearn", c("formula", "ExpressionSet", "learnerSchema", "numeric" ),
  function(formula, data, .method, trainInd, ...) {
#
# the keep setting below says just keep the response variable
# from pData
#
        data = es2df(data, keep=as.character(as.list(formula)[[2]]))
	thecall = match.call()
        ans = MLearn( formula, data, .method, trainInd, ... )
 	ans@call = thecall
        ans@learnerSchema = .method
	ans
 })


##############################################################################
## TODO
## Methods below will be equivalent to MLearn(class~.,eset,svnI,1:nrow(eset)),
## i.e use all features in the data to generate the model and subsequently use
## that resulting classifierOutput's model to predict new data with the
## predict method.
##
## setMethod("MLearn",
##           c("formula", "data.frame", "learnerSchema", "missing" ),
##           function( formula, data, .method, trainInd, ...) { ... })
##
## setMethod("MLearn",
##           c("formula", "ExpressionSet", "learnerSchema", "missing" ),
##           function( formula, data, .method, trainInd, ...) { ... })
##
## Using 1:nrow(eset) seems to work for knnI, randomForestI, nnetI,
## but not for svmI, plsdaI:
##
## > aa <- MLearn(train~.,trainSet,svmI,1:142)
## Error in matrix(ret$dec, nrow = nrow(newdata), byrow = TRUE, dimnames = list(rowns,  : 
##   negative extents to matrix
## > traceback()
## 12: matrix(ret$dec, nrow = nrow(newdata), byrow = TRUE, dimnames = list(rowns, 
##         colns))
## 11: napredict.default(act, matrix(ret$dec, nrow = nrow(newdata), 
##         byrow = TRUE, dimnames = list(rowns, colns)))
## 10: napredict(act, matrix(ret$dec, nrow = nrow(newdata), byrow = TRUE, 
##         dimnames = list(rowns, colns)))
## 9: predict.svm(obj, teData, decision.values = TRUE, probability = TRUE)
## 8: predict(obj, teData, decision.values = TRUE, probability = TRUE)
## 7: .method@converter(ans, data, trainInd)
## 6: MLearn(formula, data, .method, trainInd, ...)
## 5: MLearn(formula, data, .method, trainInd, ...)
## 4: MLearn(formula, data, .method, trainInd, ...)
## 3: MLearn(formula, data, .method, trainInd, ...)
## 2: MLearn(train ~ ., trainSet, svmI, 1:142)
## 1: MLearn(train ~ ., trainSet, svmI, 1:142)
##
## This could be handled in the respective MLIConverters by checking if
## there is any test data left before calling predict.
##
## Other related point:missing values; svm ignores the NA's in a factor, and just
## does not use these features to train the classifier.
## In MLearn, stop("missing values in object") comes up (probably from model.frame).
##
##############################################################################

# this .method for MLearn is devoted essentially to cross-validation.  it structures
# a series of calls to MLearn[numeric trainInd] and collects the output, suitably
# ordered, into a classifierOutput structure, in contrast to the older xvalML

# it is an open question whether we should try to keep all the RObjects generated through
# the sequence of cross-validations.  i think we can as long as we are not in LOO

setMethod("MLearn",
          c("formula", "data.frame", "learnerSchema", "xvalSpec" ),
          function( formula, data, .method, trainInd, ...) {
            xvspec = trainInd # rationalize parameter name
            xvalMethod = xvspec@type
            if (!(xvspec@type %in% c("LOO", "LOG", "NOTEST")))
              stop("only supporting NOTEST (fit to all data), or LOO or LOG type xvalidation at this time")
            if (xvspec@type == "LOG" && is(xvspec@partitionFunc, "NULL"))
              stop("for xval type LOG, must supply partition function")
            thecall = match.call()
            tef = model.frame(formula, data)
            teo = model.response( tef )
            classLab = names(tef)[ respind <- attr( terms(formula,data=data), "response" ) ]
            N <- nrow(data)
            inds <- 1:N
            if (xvspec@type == "NOTEST") {
              ans = MLearn(formula, data, .method, 1:N, ...)
              ans@call = thecall
              return(ans)
            }
            ## deal with sample selection
            if (xvalMethod == "LOO") {
              n <- length(inds)
              selnProc <- function(i) -i   # how to get the training set from inds
            } else { # FUN
              n <- xvspec@niter
              selnProc <- function(i) xvspec@partitionFunc( data, classLab, i )  # func defines training set directly
            }
            ## deal with feature selection
            ## check the supplied fsFun
            do.fs = FALSE
            if (is.function(xvspec@fsFun)) {
              do.fs = TRUE
              fsFun = xvspec@fsFun
              if (!all(names(formals(fsFun)) %in% c("formula", "data"))) {
                stop("xvspec@fsFun must have formals formula, data")
              }
              tst = fsFun(formula,data)
              if (!is( tst, "formula") ) {
                print("problem with fsFun in xvalSpec:")
                print(tst)
                stop("fsFun must return a formula; instead returned the object just printed.")
              }
            }

            xvalidator <- function(i, ...) {
              idx <- selnProc(i) # need to change sign when reordering...
              if (do.fs) fmla2use=fsFun(formula, data[inds[idx],])  # we are clobbering input formula
              else fmla2use=formula
              rhs_fmla = function (f) colnames(attr(terms(f, data=data), "factors"))
              try(list(test.idx = setdiff(inds,idx),
                       mlans = MLearn( fmla2use, data, .method=.method, trainInd=inds[idx], ...),
                       featInUse = rhs_fmla(fmla2use))) # package result -- test.idx kept for rearrangement
            }

            ##   xvalLoop = xvalLoop(NULL) # eventually will allow clusters
            out <-
              if (is.loaded("mc_fork", PACKAGE="parallel")) {
                mcLapply <- get("mclapply", envir=getNamespace("parallel"))
                mcLapply(1:n, xvalidator, ...)
              } else {
                lapply( 1:n, xvalidator, ... )
              }  # thanks Martin Morgan!
            chkout = sapply(out, function(z) inherits(z, "try-error"))
            if (any(chkout)) stop("xvalidator iteration threw error")
            ## now want the test sets for the various iterations
            ords <- unlist( lapply( out, function(x) x[["test.idx"]] ) )
            featsUsed = list()
            if (do.fs) featsUsed = lapply(  out, function(x) x[["featInUse"]] )
            reord = match(inds, ords)
            ## getting and aggregating xval test scores and predictions (if available)
            teClassif <- unlist( sapply(out, function(x) testPredictions(x[["mlans"]])) )
            teScores <- lapply(out, function(x) testScores(x[["mlans"]]))
            if (is.vector(teScores[[1]])) {
              teScores <- unlist(teScores)              
              testscores <- teScores[reord]
            } else if (is.matrix(teScores[[1]])) {
              teScores <- do.call(rbind,teScores)
              testscores <- teScores[reord,]
            } else {
              warning("Ignoring testScores of class ", class(teScores[[1]]),
                      ", expecting vector or matrix.")
              testscores <- NULL
            }
            testpred = factor(teClassif)[reord]
            ## ## train scores and predictions -- not sure if these should be
            ## ## returned as part of the classifierOutput
            ## trClassif <- unlist( sapply(out, function(x) trainPredictions(x[["mlans"]])) ) 
            ## trScores <- lapply(out, function(x) trainScores(x[["mlans"]]))
            ## if (is.vector(trScores[[1]])) {
            ##   trScores <- unlist(trScores)              
            ## } else if (is.matrix(trScores[[1]])) {
            ##   trScores <- do.call(rbind,trScores)
            ## } else {
            ##   warning("Ignoring trainScores of class ",class(trScores[[1]]),
            ##           ", expecting vector or matrix.")
            ##   trainscores <- NULL
            ## }
            ## trainpred = factor(teClassif)
            new("classifierOutput",
                testPredictions=testpred,
                testScores=testscores,
                ## trainScores=trainscores,
                ## trainPredictions=trainpred,
                testOutcomes=teo,
                call=thecall,
                RObject = out,
                fsHistory=featsUsed,
                learnerSchema=.method)
          })


setMethod("MLearn",
          c("formula", "ExpressionSet", "learnerSchema", "xvalSpec" ),
          function( formula, data, .method, trainInd, ...) {
            thecall = match.call()
            data = es2df(data, keep=as.character(as.list(formula)[[2]]))
            ans = MLearn(formula, data, .method, trainInd, ...)
            ans@call = thecall
            ans@learnerSchema = .method
            ans
          })


