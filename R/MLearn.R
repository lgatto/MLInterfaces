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

#setMethod("MLearn", c("formula", "data.frame", "learnerSchema",
#   "xvalSpec", "missing"), function( formula, data, method, trainInd, mlSpecials, ...) {
#   xvspec = trainInd # rationalize parameter name
#   if (!(xvspec@type %in% c("LOO", "LOG"))) stop("only supporting LOO or LOG type xvalidation at this time")
#   if (xvspec@type == "LOG" && is(xvspec@partitionFunc, "NULL")) stop("for xval type LOG, must supply partition function")
#   thecall = match.call()
## first very primitive implementation; need to introduce MMorgan's cluster-capable formulation
## and henderson's feature selection support
#   if (xvspec@type == "LOO") {
#	N = nrow(data)
#	inds = 1:N
#        testpred = rep(NA, N)
#        for (i in 1:N) {
#          tmp = MLearn(formula, data, method, inds[-i], ...)
#          testpred[i] = as.character(testPredictions(tmp))
#        }
#   tef = model.frame(formula, data)
#   teo = model.response( tef )
#   }
#   new("classifierOutput", testPredictions=factor(testpred), testOutcomes=teo, call=thecall)
#})

# this method for MLearn is devoted essentially to cross-validation.  it structures
# a series of calls to MLearn[numeric trainInd] and collects the output, suitably
# ordered, into a classifierOutput structure, in contrast to the older xvalML

# it is an open question whether we should try to keep all the RObjects generated through
# the sequence of cross-validations.  i think we can as long as we are not in LOO

setMethod("MLearn", c("formula", "data.frame", "learnerSchema",
   "xvalSpec", "missing"), function( formula, data, method, trainInd, mlSpecials, ...) {
   xvspec = trainInd # rationalize parameter name
   xvalMethod = xvspec@type
   if (!(xvspec@type %in% c("LOO", "LOG"))) stop("only supporting LOO or LOG type xvalidation at this time")
   if (xvspec@type == "LOG" && is(xvspec@partitionFunc, "NULL")) stop("for xval type LOG, must supply partition function")
   thecall = match.call()
   tef = model.frame(formula, data)
   teo = model.response( tef )
   classLab = names(tef)[ respind <- attr( terms(formula,data=data), "response" ) ]

   N <- nrow(data)
   inds <- 1:N

# deal with sample selection

   if (xvalMethod == "LOO")
     {
     n <- length(inds)
     selnProc <- function(i) -i   # how to get the training set from inds
     }
   else                          # FUN
     {
     n <- xvspec@niter
     selnProc <- function(i) xvspec@partitionFunc( data, classLab, i )  # func defines training set directly
     }

# deal with feature selection

   # check the supplied fsFun
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
     rhs_fmla = function (f) colnames(attr(terms(f), "factors"))
     list( test.idx=(setdiff(inds,idx)), mlans=MLearn( fmla2use, data, method=method, trainInd=inds[idx], ...),
             featInUse= rhs_fmla(fmla2use) ) # package result -- test.idx kept for rearrangement
     }

#   xvalLoop = xvalLoop(NULL) # eventually will allow clusters

   out <- lapply( 1:n, xvalidator, ... )
   classif <- unlist( sapply( out, function(x) testPredictions(x[["mlans"]]) ) )
# now want the test sets for the various iterations
   ords <- unlist( lapply( out, function(x) x[["test.idx"]] ) )
   featsUsed = list()
   if (do.fs) featsUsed = lapply(  out, function(x) x[["featInUse"]] )
   reord = match(inds, ords)
   testpred = factor(classif)[reord]
   new("classifierOutput", testPredictions=factor(testpred), testOutcomes=teo, call=thecall, fsHistory=featsUsed)
})


setMethod("MLearn", c("formula", "ExpressionSet", "learnerSchema",
   "xvalSpec", "missing"), function( formula, data, method, trainInd, mlSpecials, ...) {
        data = es2df(data, keep=as.character(as.list(formula)[[2]]))
	MLearn(formula, data, method, trainInd, ...)
})

