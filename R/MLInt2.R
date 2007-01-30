
setGeneric("MLearn", function(formula, data, method, trainInd, ...)standardGeneric("MLearn"))

#setMethod("MLearn", c("character", "ExpressionSet", "character", "numeric"),
#  function(formula, data, method, trainInd, ...) {
#	switch( method ,
#		knn = knnB(data, formula, as.integer(trainInd), ...),
#		nnet = nnetB( data, formula, as.integer(trainInd), ...),
#		rpart = rpartB( data, formula, as.integer(trainInd), ...),
#		randomForest = randomForestB( data, formula, as.integer(trainInd), ...),
#		gbm = gbmB( data, formula, as.integer(trainInd), ...)
#	      )
#  })
			
es2df = function(x,keep=NULL) {
   if (is.null(keep)) return(data.frame(t(exprs(x)),pData(x)))
   else {
        tmp = data.frame(t(exprs(x)),pData(x)[[keep]])
	names(tmp)[ncol(tmp)] = keep
        return(tmp)
        }
}

setMethod("MLearn", c("formula", "ExpressionSet", "character", "numeric"),
  function(formula, data, method, trainInd, ...) {
        data = es2df(data, keep=as.character(as.list(formula)[[2]]))
        MLearn( formula, data, method, trainInd, ... )
  })
    
setMethod("MLearn", c("formula", "data.frame", "character", "numeric"),
  function(formula, data, method, trainInd, ...) {
#
# extending MLInterfaces to work with formulas
#
# create training data (sdata) and test data (tdata) using trainInd
#
	sdata <- data[trainInd,]
	tdata <- data[-trainInd,]
        allClass <- data[[ as.character(as.list(formula)[[2]]) ]] # model.response(model.frame(formula,data))
#
# to add a new method, add a switch element with tag the method name,
#   and return a list with components rob (R object output by method),
#   pred (MLLabel instance of predictions on the test data) and
#   [optionally] pScores (MLScore instance of quality scores for predictions)
#
	dometh <- switch( method ,
		knn = { sdata <- model.frame(formula,data=sdata)[,-1] # drop intercept
				tdata <- model.frame(formula,data=tdata)[,-1]
				list( 
#rob = ROB <- class::knn( train=sdata, test=tdata, cl = allClass[trainInd], ...),
rob = ROB <- knnP(sdata, tdata, allClass[trainInd], ...),  # should keep training classes?
				pred = newPredClass(as.character(OUT <- knnP(sdata, tdata, allClass[trainInd], ...))),
				pScores = newQualScore(attr(OUT,"prob"))
                        	) },
		lda = { list( rob = ROB <- MASS::lda( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict( ROB, tdata)$class))
                        	) },
		nnet = { list( rob = ROB <- nnet::nnet( formula =formula, data=sdata, ...),
				pred = newPredClass(predict( ROB, tdata, type="class")),
				pScores = newProbMat(predict(ROB, newdata=tdata))
                        	) },
		randomForest = { list( rob = ROB <- randomForest::randomForest( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict(ROB, tdata)))
				) },
		rpart = { list( rob = ROB <- rpart::rpart( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict(ROB, tdata, type="class"))),
				pScores = newQualScore(attr(ROB,"prob"))
				) },
		svm = { list( rob = ROB <- e1071::svm( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict(ROB, tdata, decision.values=FALSE)))
				) }
	      	) # end  switch
	if (is.null( dometh$pScores )) dometh$pScores <- new("probMat") # placeholder
   	new("classifOutput", method=method, predLabels=dometh$pred, trainInds=trainInd,
		allClass=as.character(allClass),
		predScores=dometh$pScores, call=match.call(),
		RObject=dometh$rob)
  })


#
#setGeneric("MLearn", function(formula, data, method, trainInd, ...)standardGeneric("MLearn"))
#
#setMethod("MLearn", c("character", "exprSet", "character", "numeric"),
#  function(formula, data, method, trainInd, ...) {
#	switch( method ,
#		knn = knnB(data, formula, as.integer(trainInd), ...),
#		nnet = nnetB( data, formula, as.integer(trainInd), ...),
#		rpart = rpartB( data, formula, as.integer(trainInd), ...),
#		randomForest = randomForestB( data, formula, as.integer(trainInd), ...),
#		gbm = gbmB( data, formula, as.integer(trainInd), ...)
#	      )
#  })
#			
#    
#setMethod("MLearn", c("formula", "data.frame", "character", "numeric"),
#  function(formula, data, method, trainInd, ...) {
##
## extending MLInterfaces to work with formulas
##
## create training data (sdata) and test data (tdata) using trainInd
##
#	sdata <- data[trainInd,]
#	tdata <- data[-trainInd,]
#        allClass <- model.response(model.frame(formula,data))
##
## to add a new method, add a switch element with tag the method name,
##   and return a list with components rob (R object output by method),
##   pred (MLLabel instance of predictions on the test data) and
##   [optionally] pScores (MLScore instance of quality scores for predictions)
##
#	dometh <- switch( method ,
#		knn = { sdata <- model.frame(formula,data=sdata)[,-1] # drop intercept
#				tdata <- model.frame(formula,data=tdata)[,-1]
#				list( rob = ROB <- class::knn( train=sdata, test=tdata, cl = allClass[trainInd], ...),
#				pred = newPredClass(as.character(OUT <- knnP(sdata, tdata, allClass[trainInd], ...))),
#				pScores = newQualScore(attr(OUT,"prob"))
#                        	) },
#		nnet = { list( rob = ROB <- nnet::nnet( formula =formula, data=sdata, ...),
#				pred = newPredClass(predict( ROB, tdata, type="class")),
#				pScores = newProbMat(predict(ROB, newdata=tdata))
#                        	) },
#		rpart = { list( rob = ROB <- rpart::rpart( formula =formula, data=sdata, ...),
#				pred = newPredClass(predict(ROB, tdata, type="class")),
#				pScores = newQualScore(attr(ROB,"prob"))
#				) },
#		svm = { list( rob = ROB <- e1071::svm( formula =formula, data=sdata, ...),
#				pred = newPredClass(as.character(predict(ROB, tdata, decision.values=FALSE)))
#				) },
#		randomForest = { list( rob = ROB <- randomForest::randomForest( formula =formula, data=sdata, ...),
#				pred = newPredClass(as.character(predict(ROB, tdata)))
#				) }
#	      	) # end  switch
#	if (is.null( dometh$pScores )) dometh$pScores <- new("probMat") # placeholder
#   	new("classifOutput", method=method, predLabels=dometh$pred, trainInds=trainInd,
#		allClass=as.character(allClass),
#		predScores=dometh$pScores, call=match.call(),
#		RObject=dometh$rob)
#  })
#
#es2df = function(x,keep=NULL) {
#   if (is.null(keep)) return(data.frame(t(exprs(x)),pData(x)))
#   else {
#        tmp = data.frame(t(exprs(x)),pData(x)[[keep]])
#	names(tmp)[ncol(tmp)] = keep
#	return(tmp)
#	}
#}
#
#setClass("minimalistClassOutput", representation(
#	method="character", predLabels="ANY",
#	trainInds="numeric", allClass="ANY",
#	call="call", RObject="ANY"))
#setMethod("show", "minimalistClassOutput", function(object){
#cat("MLInterfaces output, method", object@method, "\n")
#cat("Call:\n")
#print(object@call)
#cat("Confusion on test data:\n")
#print(table(given=object@allClass[-object@trainInds],predicted=object@predLabels))
#})
#
#setGeneric("MLearn2", function(formula, data, method, trainInd, ...)
#   standardGeneric("MLearn2"))
#setMethod("MLearn2", c("formula", "exprSet", "function", "numeric"),
#  function(formula, data, method, trainInd, ...) {
#        data= es2df(data, keep=all.vars(formula)[1]) # force exclusion of nonresponse vars in pData
#	sdata <- data[trainInd,]
#	tdata <- data[-trainInd,]
#        allClass <- model.response(model.frame(formula,data))
#	ROBJ <- method( formula =formula, data=sdata, ...)
#        preds =  predict( ROBJ, newdata=tdata, type="class")
#	new("minimalistClassOutput", 
#   	     method=deparse(substitute(method)), 
#		predLabels=preds, trainInds=trainInd,
#		allClass=as.character(allClass),
#		call=match.call(),
#		RObject=ROBJ)
#  })
#
#setMethod("MLearn2", c("formula", "data.frame", "function", "numeric"),
#  function(formula, data, method, trainInd, ...) {
#	sdata <- data[trainInd,]
#	tdata <- data[-trainInd,]
#        allClass <- model.response(model.frame(formula,data))
#	ROBJ <- method( formula =formula, data=sdata, ...)
#        preds =  predict( ROBJ, newdata=tdata, type="class")
#	new("minimalistClassOutput", 
#   	     method=deparse(substitute(method)), 
#		predLabels=preds, trainInds=trainInd,
#		allClass=as.character(allClass),
#		call=match.call(),
#		RObject=ROBJ)
#  })
