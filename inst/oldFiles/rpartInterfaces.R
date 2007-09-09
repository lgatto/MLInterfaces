#####################
# PACKAGE: rpart 
#####################
#
# title: rpartB
# description: interface to rpart {rpart} 
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# rOut <- rpartB(golubMerge[100:120,], "ALL.AML", train)
#####################

setGeneric("rpartB", function(exprObj, classifLab, trainInd, weights, subset, na.action, 
		method="class", model=TRUE, x=TRUE, y=TRUE, parms, control, cost, 
		metric="euclidean", ...){
		standardGeneric("rpartB")
})

setMethod("rpartB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", 
		"ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, weights, subset, 
			na.action, method, model, x, y, parms, control, cost, metric, ...){

			if(missing(subset)){ subset <- NULL }
			if(missing(control)){ control <- NULL }
			if(missing(parms)){ parms <- NULL }
      		
			cl <- pData(exprObj)[[classifLab]]
			trainDat <- data.frame(grouping=cl[trainInd], t(exprs(exprObj)[,trainInd]))
			testDat <- data.frame(t(exprs(exprObj)[,-trainInd]))

			if(missing(weights)){ weights <- rep(1, nrow(trainDat)) }		
			if(missing(cost)){ cost <- rep(1, (ncol(trainDat)-1) ) }

			dis <- dist(testDat, method=metric)

			if(missing(na.action)){ 
				out <- rpart::rpart(grouping ~., data=trainDat, na.action=rpart::na.rpart, 
							weights=weights, subset=subset, method=method, model=model, 
							x=x, y=y, parms=parms, control=control, cost=cost, ...)
			}
			else{
				out <- rpart::rpart(grouping ~., data=trainDat, na.action=na.action, 
							weights=weights, subset=subset, method=method, model=model, 
							x=x, y=y, parms=parms, control=control, cost=cost, ...)	
			}							
		preds <- predict(out, testDat, type="class")
                new("classifOutput", method="rpart",
                        predLabels=newPredClass(as.character(preds)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
#                        predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
})
