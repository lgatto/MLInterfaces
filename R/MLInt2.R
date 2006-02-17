
setGeneric("MLearn", function(formula, data, method, trainInd, ...)standardGeneric("MLearn"))

setMethod("MLearn", c("character", "exprSet", "character", "numeric"),
  function(formula, data, method, trainInd, ...) {
	switch( method ,
		knn = knnB(data, formula, as.integer(trainInd), ...),
		nnet = nnetB( data, formula, as.integer(trainInd), ...),
		rpart = rpartB( data, formula, as.integer(trainInd), ...),
		randomForest = randomForestB( data, formula, as.integer(trainInd), ...),
		gbm = gbmB( data, formula, as.integer(trainInd), ...)
	      )
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
        allClass <- model.response(model.frame(formula,data))
#
# to add a new method, add a switch element with tag the method name,
#   and return a list with components rob (R object output by method),
#   pred (MLLabel instance of predictions on the test data) and
#   [optionally] pScores (MLScore instance of quality scores for predictions)
#
	dometh <- switch( method ,
		knn = { sdata <- model.frame(formula,data=sdata)[,-1] # drop intercept
				tdata <- model.frame(formula,data=tdata)[,-1]
				list( rob = ROB <- class::knn( train=sdata, test=tdata, cl = allClass[trainInd], ...),
				pred = newPredClass(as.character(OUT <- knnP(sdata, tdata, allClass[trainInd], ...))),
				pScores = newQualScore(attr(OUT,"prob"))
                        	) },
		nnet = { list( rob = ROB <- nnet::nnet( formula =formula, data=sdata, ...),
				pred = newPredClass(predict( ROB, tdata, type="class")),
				pScores = newProbMat(predict(ROB, newdata=tdata))
                        	) },
		rpart = { list( rob = ROB <- rpart::rpart( formula =formula, data=sdata, ...),
				pred = newPredClass(predict(ROB, tdata, type="class")),
				pScores = newQualScore(attr(ROB,"prob"))
				) },
		svm = { list( rob = ROB <- e1071::svm( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict(ROB, tdata, decision.values=FALSE)))
				) },
		randomForest = { list( rob = ROB <- randomForest::randomForest( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict(ROB, tdata)))
				) }
	      	) # end  switch
	if (is.null( dometh$pScores )) dometh$pScores <- new("probMat") # placeholder
   	new("classifOutput", method=method, predLabels=dometh$pred, trainInds=trainInd,
		allClass=as.character(allClass),
		predScores=dometh$pScores, call=match.call(),
		RObject=dometh$rob)
  })
