#setGeneric("MLearn", function(formula, data, method, trainInd, mlSpecials, ...)standardGeneric("MLearn"))

#setGeneric("MLearn", function( formula, data, 
#    method, trainInd, mlSpecials ...) standardGeneric("MLearn"))

setMethod("MLearn", c("formula", "data.frame", "learnerSchema",
   "numeric", "missing"), function( formula, data, method, trainInd, mlSpecials, ...) {
## find software
  pname = method@packageName
  fname = method@mlFunName
## create the requested function
  lfun = do.call("::", list(pname, fname))
## build test and train subsets
  tedata = data[-trainInd,]
  trdata = data[trainInd,]
## execute on training data 
  ans = lfun( formula, trdata, ...)
## collect response subsets
  trFrame = model.frame(formula, trdata)
  teFrame = model.frame(formula, tedata)
  trout = model.response( trFrame )
  teout = model.response( teFrame )
## tell what was done
  thecall = match.call()
## convert the execute result into an MLint output container
  tmp = method@converter( ans, data, trainInd )
## add some stuff to the converted representation
  tmp@testOutcomes = teout
  tmp@trainOutcomes = trout
  tmp@call = thecall
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

setMethod("MLearn", c("formula", "ExpressionSet", "learnerSchema", "numeric", "missing"),
  function(formula, data, method, trainInd, mlSpecials, ...) {
#
# the keep setting below says just keep the response variable
# from pData
#
        data = es2df(data, keep=as.character(as.list(formula)[[2]]))
        MLearn( formula, data, method, trainInd, ... )
 })

# try an approach to cross-validated interface

setMethod("MLearn", c("formula", "data.frame", "learnerSchema",
   "xvalSpec", "missing"), function( formula, data, method, trainInd, mlSpecials, ...) {
   xvspec = trainInd # rationalize parameter name
   if (!(xvspec@type %in% c("LOO", "LOG"))) stop("only supporting LOO or LOG type xvalidation at this time")
   if (xvspec@type == "LOG" && is(xvspec@partitionFunc, "NULL")) stop("for xval type LOG, must supply partition function")
   thecall = match.call()
# first very primitive implementation; need to introduce MMorgan's cluster-capable formulation
# and henderson's feature selection support
   if (xvspec@type == "LOO") {
	N = nrow(data)
	inds = 1:N
        testpred = rep(NA, N)
        for (i in 1:N) {
          tmp = MLearn(formula, data, method, inds[-i], ...)
          testpred[i] = as.character(tmp@testPredictions)
        }
   tef = model.frame(formula, data)
   teo = model.response( tef )
   }
   new("classifierOutput", testPredictions=factor(testpred), testOutcomes=teo, call=thecall)
})

setMethod("MLearn", c("formula", "ExpressionSet", "learnerSchema",
   "xvalSpec", "missing"), function( formula, data, method, trainInd, mlSpecials, ...) {
        data = es2df(data, keep=as.character(as.list(formula)[[2]]))
	MLearn(formula, data, method, trainInd, ...)
})
