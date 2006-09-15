#####################
# PACKAGE: MASS
#####################
#
#####################
# title: ldaB
# description: interface to lda {MASS}
# arguments:
#	exprObj		ExpressionSet
#	trainInd	vector of indices for the columns to be 
#			included in the training set
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classif3Output"
# example:
# train <- c(sample(1:47, 21), sample(48:72, 12))
# lOut <- ldaB(golubMerge[100:200,], "ALL.AML", train)
####################

setGeneric("ldaB", function(exprObj, classifLab, trainInd, prior, tol=1.0e-4, 
		method, CV=FALSE, nu, metric="euclidean", ...){
		standardGeneric("ldaB")
})

setMethod("ldaB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, prior, tol, method, CV, nu, metric, ...){

		if(missing(method)){ method <- NULL }
		if(missing(nu)){ nu <- NULL }
		cl <- pData(exprObj)[[classifLab]][trainInd]
		if(missing(prior)){ prior <- as.numeric(table(cl))/length(cl) }
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		out <- MASS::lda(trainDat, grouping=cl, prior=prior, tol=tol, method=method, 
				CV=CV, nu=nu, ...)
		res <- predict(out, testDat, ...)
                new("classifOutput", method="lda",
                        predLabels=newPredClass(as.character(res$class)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
                        predScores=newProbMat(res$posterior),
                        RObject=out, call=match.call(), distMat=dis)
})

#####################
# title: qdaB
# description: interface to qda {MASS}
# arguments:
#	exprObj		ExpressionSet
#	trainInd	vector of indices for the columns to be 
#			included in the training set
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classif3Output"
# example:
# train <- c(sample(1:47, 21), sample(48:72, 12))
# qOut <- qdaB(golubMerge[100:200,], "ALL.AML", train)
# note: example is not executable - "some group is too small for qda"
####################

setGeneric("qdaB", function(exprObj, classifLab, trainInd, prior, tol=1.0e-4, method, CV=FALSE, nu, 
		metric="euclidean", ...){
		standardGeneric("qdaB")
})

setMethod("qdaB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, prior, tol, method, CV, nu, metric, ...){
		if(missing(method)){ method <- NULL }
		if(missing(nu)){ nu <- NULL }
		cl <- pData(exprObj)[[classifLab]][trainInd]
		if(missing(prior)){ prior <- as.numeric(table(cl))/length(cl) }
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		out <- MASS::qda(trainDat, cl, prior=prior, tol=tol, method=method,
				CV=CV, nu=nu, ...)
		res <- predict(out, testDat, ...)
                new("classifOutput", method="qda",
                        predLabels=newPredClass(as.character(res$class)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
                        predScores=newProbMat(res$posterior),
                        RObject=out, call=match.call(), distMat=dis)
})

#####################
# title: isoMDSB
# description: interface to isoMDS {MASS}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# iOut <- isoMDSB(golubMerge[100:200,], "ALL.AML", k=1)
####################

setGeneric("isoMDSB", function(exprObj, classifLab, y, k=2, maxit=50, trace=TRUE, 
		tol=1e-3, p=2, metric="euclidean"){
		standardGeneric("isoMDSB")
})

#setMethod("isoMDSB", c("ExpressionSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"),
#		function(exprObj, classifLab, y, k, maxit, trace, tol, p, metric){
#
#		cl <- pData(exprObj)[[classifLab]]
#		dat <- exprs(exprObj)
#		colnames(dat) <- cl
#		dmat <- dist(t(dat), method=metric)
#
#		if(missing(y)){ y <- cmdscale(dmat, k) }
#
#		out <- MASS::isoMDS(dmat, y=y, k=k, maxit=maxit, trace=trace, tol=tol, p=p)				
#		res <- predict(out, testDat, ...)
#                new("classifOutput", method="qda",
#                        predLabels=newPredClass(as.character(res$class)),
#			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
#                        predScores=newProbMat(res$posterior),
#                        RObject=out, call=match.call(), distMat=dis)
#		new("classifPred", sampLabels=cl, distMat=dmat, classifObj=out)
#})

#####################
# title: sammonB
# description: interface to sammon {MASS}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# sOut <- sammonB(golubMerge[100:200,], "ALL.AML")
####################
#
#setGeneric("sammonB", function(exprObj, classifLab, y, k=2, niter=100, trace=TRUE, 
#		magic=0.2, tol=1e-4, metric="euclidean"){
#		standardGeneric("sammonB")
#})
#
#setMethod("sammonB", c("ExpressionSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"),
#		function(exprObj, classifLab, y, k, niter, trace, magic, tol, metric){
#
#		cl <- pData(exprObj)[[classifLab]]
#		dat <- exprs(exprObj)
#		colnames(dat) <- cl
#		dmat <- dist(t(dat), method=metric)
#		if(missing(y)){ y <- cmdscale(dmat, k) }
#
#		out <- MASS::sammon(dmat, y=y, k=k, niter=niter, trace=trace,
#					magic=magic, tol=tol)
#		new("classifPred", sampLabels=cl, distMat=dmat, classifObj=out)
#})
