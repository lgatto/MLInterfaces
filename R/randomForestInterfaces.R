#####################
# PACKAGE: randomForest
#####################
# 
#####################
# title: randomForestB
# description: interface to randomForest {randomForest}
# arguments:
#	exprObj		ExpressionSet
#	trainInd	vector of indices for the columns to be 
#			included in the training set
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# rfOut <- randomForestB(golubMerge[100:200,], "ALL.AML", train)
# note: default for mtry, nodesize taken from Breiman's tips in pdf
####################

# randomForest

setGeneric("randomForestB", function(exprObj, classifLab, trainInd, xtest, 
			ytest, addclass = 0, ntree = 500, mtry, classwt, 
			cutoff, sampsize, nodesize, importance = FALSE, proximity = FALSE, 
			oob.prox = TRUE, outscale = FALSE, norm.votes = TRUE, do.trace = FALSE, 
			keep.forest = is.null(xtest), corr.bias=FALSE, metric="euclidean", ...){
			standardGeneric("randomForestB")
			})

setMethod("randomForestB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", 
			"ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", 
			"ANY", "ANY"),
			function(exprObj, classifLab, trainInd, xtest, ytest, addclass, ntree, mtry, 
			classwt, cutoff, sampsize, nodesize, importance, proximity, oob.prox, outscale, 
			norm.votes, do.trace, keep.forest, corr.bias, metric, ...){

			trainDat <- t(exprs(exprObj)[,trainInd])
			cl <- pData(exprObj)[[classifLab]][trainInd]
			if(missing(xtest)){ xtest <- NULL }
			if(missing(ytest)){ ytest <- NULL }
			if(missing(mtry)){ mtry <- sqrt(ncol(trainDat)) }
			if(missing(classwt)){ classwt <- NULL }
			if(missing(nodesize)){ nodesize <- 1 }
			if(missing(sampsize)){ sampsize <- table(cl) }

			testDat <- t(exprs(exprObj)[ ,-trainInd])
			dis <- dist(testDat, method=metric)

			out <- randomForest::randomForest(trainDat, y=cl, xtest=xtest, ytest=ytest, addclass=addclass, 
						ntree=ntree, mtry= mtry,
						classwt=classwt, cutoff=cutoff, sampsize=sampsize, nodesize=nodesize,
						importance=importance, proximity=proximity, oob.prox=oob.prox, 
						outscale=outscale, norm.votes=norm.votes, do.trace=do.trace, 
						keep.forest=keep.forest, corr.bias=corr.bias, ...)

                preds <- predict(out, testDat)
                new("classifOutput", method="randomForest",
                        predLabels=newPredClass(as.character(preds)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
#                        predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
})
