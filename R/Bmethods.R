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

		#.Deprecated("MLearn", "MLInterfaces")
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
		#.Deprecated("MLearn", "MLInterfaces")
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
setOldClass("knnP")
setOldClass("nnet.formula")
setOldClass("diana")
setOldClass("agnes")
setOldClass("pam")
setOldClass("rpart")
setOldClass("svm")
setOldClass("bclust")
setOldClass("fclust")
setOldClass("cshell")
setOldClass("ica")
setOldClass("lca")
setOldClass("naiveBayes")
setOldClass("pamr")
setOldClass("randomForest")
setOldClass("hclust")
setOldClass("kmeans")
setOldClass("prcomp")
setOldClass("classbagg")
setOldClass("bagging")
setOldClass("ipredknn")
setOldClass("slda")
setOldClass("lda")
setOldClass("qda")
setOldClass("nsc")
setOldClass("gbm")
setOldClass("logitboost")

knnP <- function(train, test, cl, k=1, l=0, prob=FALSE, use.all=TRUE) {
#
# idea here is to allow knn to work with predict method using new data.
# create a closure that knows about the training data, and later evaluate
# it on any conforming test data
#
# october 25 -- seem to need to use name newdata
# for this to work with generic prediction
#
 #.Deprecated("MLearn", "MLInterfaces")
 ans <- class::knn(train,test,cl,k,l,prob,use.all)
 nf <- function(train,cl,k,l,prob,use.all) function(newdata)
	 class::knn(train,newdata,cl,k,l,prob,use.all)
 attr(ans, "predfun") <- nf(train,cl,k,l,prob,use.all)
 class(ans) <- c("knnP", "factor")
 ans
}

predict.knnP <- function(object, ...) {
 	#.Deprecated("MLearn", "MLInterfaces")
	attr(object, "predfun")(...)
}

print.knnP <- function(x, ...)
	{
 	#.Deprecated("MLearn", "MLInterfaces")
	cat("instance of knnP [predictable knn object]\n")
	NextMethod()
	}

setGeneric("knnB", function(exprObj, classifLab, trainInd, 
		k=1, l=1, prob=TRUE, use.all=TRUE, metric="euclidean"){
			standardGeneric("knnB")
		})

setMethod("knnB", c("ExpressionSet", "character", "integer", 
			"ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, k, l, 
			prob, use.all, metric){
		#.Deprecated("MLearn", "MLInterfaces")

		cl <- pData(exprObj)[[classifLab]][trainInd]				
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		out <- knnP(trainDat, testDat, cl, k, l, prob, use.all)
                new("classifOutput", method="knn", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
                                                                                
})


#####################
# title: knn.cvB
# description: interface to knn.cv {class}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classif2Output"
# example:
# knn.cvOut <- knn.cvB(golubMerge[101:140,], "ALL.AML")
####################

#setGeneric("knn.cvB", function(exprObj, classifLab, trainInd=NULL, k=1, l=1, prob=TRUE, use.all=TRUE, metric="euclidean"){
#		standardGeneric("knn.cvB")
#})
#
#setMethod("knn.cvB", c("ExpressionSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY"), 
#			function(exprObj, classifLab, trainInd=NULL, k, l, prob, use.all, metric){
#			if (!is.null(trainInd)) warning("disregarding trainInd for knn.cvB")
#			cl <- pData(exprObj)[[classifLab]]
#			dat <- t(exprs(exprObj))
#			dis <- dist(dat, method=metric)
#			out <- class::knn.cv(dat, cl, k, l, prob, use.all)
#                new("classifOutput", method="knn.cv", 
##			predLabels=newPredClass(as.character(out)), 
#			trainInds=integer(0), allClass=as.character(pData(exprObj)[[classifLab]]),
#			predScores=newQualScore(attr(out,"prob")),
#                        RObject=out, call=match.call(), distMat=dis)
#})

#####################
# title: knn1B
# description: interface to knn1 {class}
# arguments:
#	exprObj		ExpressionSet
#	trainInd	vector of indices for the columns to be 
#			included in the training set
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classif1Output"
# example:
# train <- c(sample(1:47, 21), sample(48:72, 12))
# knn1Out <- knn1B(golubMerge[100:200,], "ALL.AML", train)
####################

setGeneric("knn1B", function(exprObj, classifLab, trainInd, metric="euclidean"){
		standardGeneric("knn1B")
})

setMethod("knn1B", c("ExpressionSet", "character", "integer", "ANY"), 
		function(exprObj, classifLab, trainInd, metric){
		#.Deprecated("MLearn", "MLInterfaces")
				
		cl <- pData(exprObj)[[classifLab]][trainInd]
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		out <- class::knn1(trainDat, testDat, cl)
                new("classifOutput", method="knn1", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
})

#####################
# title: lvq1B
# description: interface to lvq1 {class}
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
# train <- c(sample(1:47, 21), sample(48:72, 12))
# lvq1Out <- lvq1B(golubMerge[100:200,], "ALL.AML", train)
####################

setGeneric("lvq1B", function(exprObj, classifLab, trainInd, size, prior, k=5, niter, alpha=0.03, metric="euclidean"){
		standardGeneric("lvq1B")
})

setMethod("lvq1B", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, size, prior, k, niter, alpha, metric){

		#.Deprecated("MLearn", "MLInterfaces")
		if(missing(size)){ size <- NULL }
		cl <- pData(exprObj)[[classifLab]][trainInd]
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)

		cbkInit <- class::lvqinit(trainDat, cl, size=size, prior=prior, k=k)
		if(missing(niter)){ niter <- 100 * nrow(cbkInit$x) } 
		cbkTrain <- class::lvq1(trainDat, cl, cbkInit, niter=niter)
		out <- class::lvqtest(cbkTrain, testDat)
                new("classifOutput", method="lvq1", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
			#predScores=newQualScore(attr(out,"prob")),
                        RObject=cbkTrain, call=match.call(), distMat=dis)
})	

#####################
# title: lvq2B
# description: interface to lvq2 {class}
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
# train <- c(sample(1:47, 21), sample(48:72, 12))
# lvq2Out <- lvq2B(golubMerge[100:200,], "ALL.AML", train)
####################

setGeneric("lvq2B", function(exprObj, classifLab, trainInd, size, 
		prior, k=5, niter, alpha=0.03, win=0.3, metric="euclidean"){
		standardGeneric("lvq2B")
})

setMethod("lvq2B", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", 
		"ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, size, prior, k, 
			niter, alpha, win, metric){

		#.Deprecated("MLearn", "MLInterfaces")
		if(missing(size)){ size <- NULL }
		cl <- pData(exprObj)[[classifLab]][trainInd]
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		cbkInit <- class::lvqinit(trainDat, cl, size=size, 
					prior=prior, k=k)
		if(missing(niter)){ niter <- 100 * nrow(cbkInit$x) } 
		cbkTrain <- class::lvq2(trainDat, cl, cbkInit, niter=niter, 										alpha=alpha, win=win)
		out <- class::lvqtest(cbkTrain, testDat)
                new("classifOutput", method="lvq2", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=cbkTrain, call=match.call(), distMat=dis)
	
})	

#####################
# title: lvq3B
# description: interface to lvq3 {class}
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
# train <- c(sample(1:47, 21), sample(48:72, 12))
# lvq3Out <- lvq3B(golubMerge[100:200,], "ALL.AML", train)
####################

setGeneric("lvq3B", function(exprObj, classifLab, trainInd, size, prior, k=5, 
		niter, alpha=0.03, win=0.3, epsilon=0.1, metric="euclidean"){
		standardGeneric("lvq3B")
})

setMethod("lvq3B", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, size, prior, k, niter, alpha, win, epsilon, metric){

		#.Deprecated("MLearn", "MLInterfaces")
		if(missing(size)){ size <- NULL }
		cl <- pData(exprObj)[[classifLab]][trainInd]			
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])	
		dis <- dist(testDat, method=metric)
		cbkInit <- class::lvqinit(trainDat, cl, size=size, prior=prior, k=k)
		if(missing(niter)){ niter <- 100 * nrow(cbkInit$x) } 
		cbkTrain <- class::lvq3(trainDat, cl, cbkInit, niter=niter, alpha=alpha, 
					win=win, epsilon=epsilon)
		out <- class::lvqtest(cbkTrain, testDat)
                new("classifOutput", method="lvq3", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=cbkTrain, call=match.call(), distMat=dis)
	
})	

#####################
# title: olvq1B
# description: interface to olvq1 {class}
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
# train <- c(sample(1:47, 21), sample(48:72, 12))
# olvq1Out <- olvq1B(golubMerge[100:200,], "ALL.AML", train)
####################

setGeneric("olvq1B", function(exprObj, classifLab, trainInd, size, prior, k=5, niter, alpha=0.03, metric="euclidean"){
		standardGeneric("olvq1B")
})

setMethod("olvq1B", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, size, prior, k, niter, alpha, metric){

		#.Deprecated("MLearn", "MLInterfaces")
		if(missing(size)){ size <- NULL }
		cl <- pData(exprObj)[[classifLab]][trainInd]
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		cbkInit <- class::lvqinit(trainDat, cl, size=size, prior=prior)
		if(missing(niter)){ niter <- 100 * nrow(cbkInit$x) } 
		cbkTrain <- class::olvq1(trainDat, cl, cbkInit, niter=niter, alpha=alpha)
		out <- class::lvqtest(cbkTrain, testDat)
                new("classifOutput", method="olvq1", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=cbkTrain, call=match.call(), distMat=dis)

})	

#####################
# title: SOMB
# description: interface to SOM {class}
# arguments:
#	exprObj		ExpressionSet
#	kx		x dimension
#	ky		y dimension
# 	topo		grid topology 
#	classifLab	character string specifying what covariate data 
#			to use for classification
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# somOut <- SOMB(golubMerge[100:200,], "ALL.AML", 2, 2)
####################

setGeneric("SOMB", function(exprObj, classifLab, kx=3, ky=3, topo="hexagonal", rlen=10000, 
		alpha=seq(0.05, 0, len=rlen), 
		radii=seq(4, 1, len = rlen), init, metric="euclidean"){
		standardGeneric("SOMB")
})

# a special container is provided for SOMB in somInterfaces.R
#####################
# PACKAGE: cluster
#####################
#
#####################
# title: agnesB
# description: interface to agnes {cluster} 
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data to use for classification
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# agOut <- agnesB(golubMerge[100:200,], "ALL.AML")
#####################

setGeneric("agnesB", function(exprObj, k, height=0, stand=FALSE, method="average",
		keep.diss=TRUE, keep.data=TRUE, metric="euclidean", ...){
		standardGeneric("agnesB")
})

setMethod("agnesB", c("ExpressionSet", "numeric", "ANY", 
                "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, k, height, stand, method, keep.diss, keep.data, metric, ...){

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
#		row.names(dat) <- pData(exprObj)[[classifLab]]
		
		out <- cluster::agnes(dat, metric=metric, stand=stand, method=method, 
					keep.diss=keep.diss, keep.data=keep.data)
		tmp <- wrapClust(out, k, height, dis)

		new("clustOutput", method="agnes",
			RObject=out, call=match.call(),
			distMat=dis,
			clustIndices=tmp$clinds, clustScores=tmp$clsco)
})

#####################
# title: claraB
# description: interface to clara {cluster}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# clObj <- claraB(golubMerge[100:200,], "ALL.AML", 2)
#####################

setGeneric("claraB", function(exprObj, k, height=0, stand=FALSE, 
		samples=5, sampsize, trace=0, keep.data=TRUE, keepdata, rngR=FALSE, metric="euclidean"){
		standardGeneric("claraB")
})

setMethod("claraB", c("ExpressionSet", "numeric", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", 
		"ANY", "ANY"), function(exprObj, k, height, stand, samples, sampsize,
		trace, keep.data, keepdata, rngR, metric){

		if(missing(sampsize)){ sampsize <- 40 + 2*k }
		if (height > 0 ) warning("ignoring height parameter")

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		out <- cluster::clara(dat, k=k, metric=metric, stand=stand, samples=samples, 							sampsize=sampsize, trace=trace, keep.data=keep.data, keepdata=keepdata, rngR= rngR)	
                clinds <- newGroupIndex(out$clustering)
                clsco <- newSilhouetteVec(cluster::silhouette( clinds, dis )[,3])
		new("clustOutput", method="clara",
			RObject=out, call=match.call(),
			distMat=dis,
			clustIndices=clinds, clustScores=clsco)
})

#####################
# title: dianaB
# description: interface to diana {cluster}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data to use for classification
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# diOut <- dianaB(golubMerge[100:200,], "ALL.AML")
#####################

setGeneric("dianaB", function(exprObj, k, height=0, diss, stand=FALSE, 
		keep.diss, keep.data=TRUE, metric="euclidean"){
		standardGeneric("dianaB")
})

setMethod("dianaB", c("ExpressionSet", "numeric", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, k, height, diss, stand, keep.diss, keep.data, metric){

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)

		out <- cluster::diana(dat, diss=FALSE, metric=metric, 
					stand=stand, keep.diss=TRUE,
					keep.data=keep.data)
		tmp <- wrapClust( out, k, height, dis)

		new("clustOutput", method="diana",
			RObject=out, call=match.call(),
			distMat=dis,
			clustIndices=tmp$clinds, clustScores=tmp$clsco)
})

#####################
# title: fannyB
# description: interface to fanny {cluster}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# faOut <- fannyB(golubMerge[100:200,], "ALL.AML", 2)
#####################

setGeneric("fannyB", function(exprObj, k, height=0, diss, stand=FALSE, metric="euclidean"){
		standardGeneric("fannyB")
}) 

setMethod("fannyB", c("ExpressionSet", "numeric", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, k, height, diss, stand, metric){
		if (height > 0) warning("ignoring height parameter")

		if(missing(diss)){ diss <- F }

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		out <- cluster::fanny(dat, k=k, diss=diss, metric=metric, stand=stand)

		clinds <- newGroupIndex(out$clustering)
                clsco <- newSilhouetteVec(cluster::silhouette( clinds, dis )[,3])
		new("clustOutput", method="fanny",
			RObject=out, call=match.call(),
			distMat=dis,
			clustIndices=clinds, clustScores=clsco)
})

#####################
# title: pamB
# description: interface to pam {cluster}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# paOut <- pamB(golubMerge[100:200,], "ALL.AML", 2)
#####################

setGeneric("pamB", function(exprObj, k, height=0, diss, stand=FALSE, 
		keep.diss=TRUE, keep.data=TRUE, metric="euclidean"){
		standardGeneric("pamB")
})

setMethod("pamB", c("ExpressionSet", "numeric", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, k, height, diss, stand, keep.diss, keep.data, metric){

		
		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		if (height > 0 ) warning("ignoring height parameter")
		out <- cluster::pam(dat, k=k, diss=FALSE, metric=metric, stand=stand, keep.diss=keep.diss, 
					keep.data=keep.data)
		clinds <- newGroupIndex(out$clustering)
                clsco <- newSilhouetteVec(cluster::silhouette( clinds, dis )[,3])
		new("clustOutput", method="pam",
			RObject=out, call=match.call(),
			distMat=dis,
			clustIndices=clinds, clustScores=clsco)
})
			
#
# UNTIL THERE IS A CLEAR METHOD FOR DICHOTOMIZING AN EXPRSET,
# MONA WILL HAVE TO BE RUN RAW

#####################
# title: monaB
# description: interface to mona {cluster}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# x <- matrix(sample(c(1,2), 7272, replace=T), ncol=72, nrow=101)
# colnames(x) <- golubMerge$"ALL.AML"
# g2Merge <- golubMerge[100:200,]
# exprs(g2Merge) <- x
# mOut <- monaB(g2Merge, "ALL.AML")
# note: artificial example since data needs to be binary
#####################

#setGeneric("monaB", function(exprObj, classifLab, metric="euclidean"){
#		standardGeneric("monaB")
#})
#
#setMethod("monaB", c("ExpressionSet", "character", "ANY"), 
#		function(exprObj, classifLab, metric){
#
#		dat <- t(exprs(exprObj))
#		dis <- dist(dat, method=metric)
#		row.names(dat) <- pData(exprObj)[[classifLab]]
#		out <- cluster::mona(dat)
#
#		new("classifPred", sampLabels=pData(exprObj)[[classifLab]], distMat=dis, classifObj=out)
##})
#####################
# PACKAGE: e1071
#####################
#
#####################
# title: bclustB
# description: interface to bclust {e1071}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	dist.method	for distance matrix (equivalent to the "metric" argument in other 
#			machLI interfaces, eg. see knnB)
# value:
# 	object of class "classifPred"
# example:
# bOut <- bclustB(golubMerge[100:200,], "ALL.AML", 2)
####################

setGeneric("bclustB", function(exprObj, k, height=0, iter.base=10, minsize=0, dist.method="euclidian", 
		hclust.method="average", base.method="kmeans", base.centers=5, verbose=TRUE, 
		final.kmeans=FALSE, docmdscale=FALSE, resample=TRUE, weights, maxcluster=5, ...){
		standardGeneric("bclustB")
})

setMethod("bclustB", c("ExpressionSet", "numeric", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY",
		"ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, k, height, iter.base, minsize, dist.method, hclust.method, base.method, 
			base.centers, verbose, final.kmeans, docmdscale, resample, weights, 
			maxcluster, ...){

			if(missing(weights)){ weights <- NULL }

			dat <- t(exprs(exprObj))
			dis <- dist(dat, method=dist.method)
			out <- e1071::bclust(dat, k, iter.base=iter.base, minsize=minsize, 
						dist.method=dist.method, hclust.method=hclust.method, 
						base.method=base.method, base.centers=base.centers, 
						verbose=verbose, final.kmeans=final.kmeans,
						docmdscale=docmdscale, resample=resample, weights=weights, 
									maxcluster=maxcluster, ...)
# CANNOT USE WRAPCLUST                tmp <- wrapClust( out, k, height, dis) 
		if (k > 0 && height > 0) warn("both k and height provided, using k")
		if (k > 0) clinds <- newGroupIndex(out$cluster)
		else if (k == 0 & height > 0)
			clinds <- cutree(out$hclust, h=height)
		clsco <- newSilhouetteVec(cluster::silhouette(clinds,dis)[,3])
                new("clustOutput", method="bclust",
                        RObject=out, call=match.call(),
                        distMat=dis,
                        clustIndices=clinds, clustScores=clsco)


})

#####################
# title: cmeansB
# description: interface to cmeans {e1071}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	dist		for distance matrix (equivalent to the "metric" argument in other 
#			machLI interfaces, eg. see knnB)
# value:
# 	object of class "classifPred"
# example:
# cOut <- cmeansB(golubMerge[100:200,], "ALL.AML", 2)
####################

setGeneric("cmeansB", function(exprObj, k, height=0, iter.max=100, verbose=FALSE, dist="euclidean", 
		method="cmeans", m=2, rate.par=NULL){
		standardGeneric("cmeansB")
})

setMethod("cmeansB", c("ExpressionSet", "numeric", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, k, height=0, iter.max, verbose, dist, method, m, rate.par){

			dat <- t(exprs(exprObj))
			dis <- dist(dat, method=dist)
			out <- e1071::cmeans(dat, k, iter.max=iter.max, verbose=verbose, dist=dist,
						method=method, m=m, rate.par=rate.par)
			
                	clinds <- newGroupIndex(out$cluster)
                	clsco <- newSilhouetteVec(cluster::silhouette( clinds, dis )[,3])
                new("clustOutput", method="cmeans",
                        RObject=out, call=match.call(),
                        distMat=dis,
                        clustIndices=clinds, clustScores=clsco)
})


setGeneric("cshellB", function(exprObj, k, height=0, iter.max=20, verbose=FALSE, dist="euclidean", 
		method="cshell", m=2, radius){
		standardGeneric("cshellB")
})

setMethod("cshellB", c("ExpressionSet", "numeric", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, k, height, iter.max, verbose, dist, method, m, radius){

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=dist)

		if(missing(radius)){

			out <- e1071::cshell(dat, k, iter.max=iter.max, verbose=verbose, dist=dist,
						method=method, m=m)
		}
		else{
			out <- e1071::cshell(dat, k, iter.max=iter.max, verbose=verbose, dist=dist,
						method=method, m=m, radius=radius)
		}
                clinds <- newGroupIndex(out$cluster)
                clsco <- newMembMat(out$membership)
                new("clustOutput", method="cmeans",
                        RObject=out, call=match.call(),
                        distMat=dis,
                        clustIndices=clinds, clustScores=clsco)

})

######################
## title: icaB
## description: interface to ica {e1071}
## arguments:
##	exprObj		ExpressionSet
##	metric		for distance matrix 
## value:
## 	object of class "classifPred"
##	where sampLabels are the labels of the original sample
## example:
## icaOut <- icaB(golubMerge[100:150,], "ALL.AML", 100)
## note: is there a better way to specify a learning rate? 
## ica {e1071} outputs weights and projection that are all NaN
## initweights output corresponds to the condensed profiles (rows) across columns
#####################
#
#setGeneric("icaB", function(exprObj, classifLab, lrate, epochs=100, ncomp, fun="negative", metric="euclidean"){
#		standardGeneric("icaB")
#})

#setMethod("icaB", c("ExpressionSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY"), 
#			function(exprObj, classifLab, lrate, epochs, ncomp, fun, metric){
#			
#			dat <- t(exprs(exprObj))
#			dis <- dist(dat, method=metric)
#			out <- e1071::ica(dat, lrate, epochs=epochs, ncomp=dim(dat)[2], fun=fun)
#
#			new("classifPred", sampLabels=pData(exprObj)[[classifLab]], distMat=dis, classifObj=out)
#})
#
######################
## title: lcaB
## description: interface to lca {e1071}
# arguments:
#	exprObj		ExpressionSet
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# x <- matrix(sample(c(0,1), 7272, replace=T), ncol=72, nrow=12)
# colnames(x) <- golubMerge$"ALL.AML"
# g2Merge <- golubMerge[100:111,]
# exprs(g2Merge) <- x
# lcaOut <- lcaB(g2Merge, "ALL.AML", 2)
# note: artificial example since data needs to be binary
# for > 15 genes, lca algorithm runs out of memory
# for a much larger number of genes, error is returned
####################

setGeneric("lcaB", function(exprObj, k, niter=100, matchdata=TRUE, verbose=FALSE, metric="euclidean"){
	standardGeneric("lcaB")
})

setMethod("lcaB", c("ExpressionSet", "numeric", "ANY", "ANY", "ANY", "ANY"), 
			function(exprObj, k, niter, matchdata, verbose, metric){

			dat <- t(exprs(exprObj))
	if (!all(dat %in% c(0,1))) stop("binary expr data required")
			dis <- dist(dat, method=metric)
			out <- e1071::lca(dat, k, niter=niter, matchdata=matchdata, verbose=verbose) 
                	clinds <- newGroupIndex(out$matching)
                	clsco <- newSilhouetteVec(cluster::silhouette( clinds, dis )[,3])
                	new("clustOutput", method="lca",
                        	RObject=out, call=match.call(),
                        	distMat=dis,
                        	clustIndices=clinds, clustScores=clsco)
})

#####################
# title: naiveBayesB
# description: interface to naiveBayes {e1071}
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
# train <- c(sample(1:47, 24), sample(48:72, 12))
# nbOut <- naiveBayesB(golubMerge[100:110,], "ALL.AML", train)
# note:
# algorithm appears to be bad at handling a large number of genes (ie. columns in naiveBayes)
####################

setGeneric("naiveBayesB", function(exprObj, classifLab, trainInd,	na.action=na.pass, threshold=0.001, 
		metric="euclidean"){
		standardGeneric("naiveBayesB")
})

setMethod("naiveBayesB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, na.action, threshold, metric){

		#.Deprecated("MLearn", "MLInterfaces")
		cl <- pData(exprObj)[[classifLab]][trainInd]		
		trainDat <- data.frame(y=cl, t(exprs(exprObj)[,trainInd]))
		testDat <- data.frame(t(exprs(exprObj)[,-trainInd]))	
		dis <- dist(testDat, method=metric)
		model <- e1071::naiveBayes(y~., data=trainDat)
		out <- predict( model, newdata=testDat )
                new("classifOutput", method="naiveBayes",
                        predLabels=newPredClass(as.character(out)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
                #        predScores=newQualScore(attr(out,"prob")),
                        RObject=model, call=match.call(), distMat=dis)
                                                                                
})

#####################
# title: svmB
# description: interface to svm {e1071} 
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
# svmOut <- svmB(golubMerge[100:200,], "ALL.AML", train)
#####################

setGeneric("svmB", function(exprObj, classifLab, trainInd, scale=TRUE, 
		type, kernel="radial", degree=3, gamma, coef0 = 0, 
		cost = 1, nu = 0.5, class.weights, cachesize = 40, 
		tolerance = 0.001, epsilon = 0.1, shrinking = TRUE, cross = 0, 
		fitted = TRUE, subset, na.action = na.omit, decision.values=FALSE, metric="euclidean", ...){
		standardGeneric("svmB")
})

setMethod("svmB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY",
		"ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", 
		"ANY", "ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, scale, type, kernel, degree, gamma, 
		coef0, cost, nu, class.weights, cachesize, tolerance, epsilon, shrinking, 
		cross, fitted, subset, na.action, decision.values, metric, ...){

		#.Deprecated("MLearn", "MLInterfaces")
			trainDat <- t(exprs(exprObj)[,trainInd])
			testDat <- t(exprs(exprObj)[,-trainInd])
			dis <- dist(testDat, method=metric)
			cl <- pData(exprObj)[[classifLab]][trainInd]

			if(missing(type)){ type <- NULL }
			if(missing(class.weights)){ class.weights <- NULL }
			if(missing(gamma)){ gamma <- 1/ncol(trainDat) }

			out <- e1071::svm(trainDat, cl, scale=scale, type=type, kernel=kernel, degree=degree, 
					gamma=gamma, coef0=coef0, cost=cost, nu=nu, class.weights=class.weights, 
					cachesize=cachesize, tolerance=tolerance, epsilon=epsilon, shrinking=shrinking, 
					cross=cross, fitted=fitted, subset=subset, na.action = na.action, ...)
		  			
			ans <- predict(out, newdata=testDat,
				decision.values=decision.values, na.action=na.action)
                new("classifOutput", method="svm",
                        predLabels=newPredClass(as.character(ans)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
                #        predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
			
})
#####################
# PACKAGE: ipred
#####################
#
#####################
# title: baggingB
# description: interface to bagging {ipred} 
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# note:
# 	aggregation	argument specifying how to combine results for prediction 
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# hg <- baggingB(golubMerge[100:200,], "ALL.AML", train)
#####################

setGeneric("baggingB", function(exprObj, classifLab, trainInd, subset, aggregation="majority", metric="euclidean", ...){
		standardGeneric("baggingB")
})

setMethod("baggingB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, subset, aggregation, metric, ...){

		#.Deprecated("MLearn", "MLInterfaces")
		if(missing(subset)){ subset <- NULL }

		cl <- pData(exprObj)[[classifLab]][trainInd]		
		trainDat <- data.frame(y=cl, t(exprs(exprObj)[,trainInd]))
		testDat <- data.frame(t(exprs(exprObj)[,-trainInd]))
		dis <- dist(testDat, method=metric)
library(ipred)
		tmp <- bagging(y~., data=trainDat, subset=subset, ...)
		out <- predict(tmp, newdata=testDat)
                new("classifOutput", method="bagging",
                        predLabels=newPredClass(as.character(out)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
#                        predScores=newQualScore(attr(out,"prob")),
                        RObject=tmp, call=match.call(), distMat=dis)
})

#####################
# title: ipredknnB
# description: interface to ipredknn {ipred} 
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
# hg <- ipredknnB(golubMerge[100:200,], "ALL.AML", train)
# note: would be great if we could get prob and class predictions
#####################

setGeneric("ipredknnB", function(exprObj, classifLab, trainInd, na.action, k=5, metric="euclidean", ...){
		standardGeneric("ipredknnB")
})

setMethod("ipredknnB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, na.action, k, metric, ...){
				
		#.Deprecated("MLearn", "MLInterfaces")  # not needed at all, just shapes knn output
		if(missing(na.action)){ na.action <- NULL }
		cl <- pData(exprObj)[[classifLab]][trainInd]		
		trainDat <- data.frame(y = cl, t(exprs(exprObj)[,trainInd]))
		testDat <- data.frame(t(exprs(exprObj)[,-trainInd]))
		dis <- dist(testDat, method=metric)
library(ipred)
		tmp <- ipredknn(y~., data=trainDat, na.action=na.action, k=k, ...)
		out <- predict(tmp, newdata=testDat, type="class")
		prob <- predict(tmp, newdata=testDat, type="prob")
                new("classifOutput", method="ipredknn",
                        predLabels=newPredClass(as.character(out)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
                        predScores=newQualScore(prob),
                        RObject=tmp, call=match.call(), distMat=dis)
})

#####################
# title: sldaB
# description: interface to slda {ipred} 
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
# train <- c(sample(1:47, 23), sample(48:72, 12))
# hg <- sldaB(golubMerge[100:200,], "ALL.AML", train)
#####################

setGeneric("sldaB", function(exprObj, classifLab, trainInd, subset, na.action=na.rpart, metric="euclidean", ...){
		standardGeneric("sldaB")
})

setMethod("sldaB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, subset, na.action, metric, ...){

		#.Deprecated("MLearn", "MLInterfaces")  
		if(missing(subset)){ subset <- NULL }
		cl <- pData(exprObj)[[classifLab]][trainInd]		
		trainDat <- data.frame(y=cl, t(exprs(exprObj)[,trainInd]))			
		dat <- data.frame(t(exprs(exprObj)[,trainInd]))
		testDat <- data.frame(t(exprs(exprObj)[,-trainInd]))
		dis <- dist(testDat, method=metric)

library(ipred)
		tmp <- slda(y~., data=trainDat, na.action=na.action, ...)
		out <- predict(tmp, newdata=testDat)
                new("classifOutput", method="slda",
                        predLabels=newPredClass(as.character(out$class)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
                        predScores=newProbMat(out$posterior),
                        RObject=tmp, call=match.call(), distMat=dis)
})

#####################
# title: inbaggB
# description: interface to inbagg {ipred} 
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	trainInd	vector of indices for the columns to be 
#			included in the training set
# 	intLab 		character string or vector of character strings 
#			representing names of the intermediate variables
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# a <- inbaggB(golubMerge[100:110,], "ALL.AML", train, "PS")
#####################
#
#setGeneric("inbaggB", function(exprObj, classifLab, trainInd, intLab, pFUN, cFUN, metric="euclidean", ...){
#		standardGeneric("inbaggB")
#})
#
#setMethod("inbaggB", c("ExpressionSet", "character", "integer", "character", "ANY", "ANY", "ANY", "ANY"), 
#		function(exprObj, classifLab, trainInd, intLab, pFUN, cFUN, metric, ...){
#
#
#
##		if(missing(pFUN)){ pFUN <- list(list(model=lm)) }
#		if(missing(pFUN)){ pFUN <- NULL }
#		if(missing(cFUN)){ cFUN <- NULL }
#
#		cl <- pData(exprObj)[[classifLab]]
#		
#		if(length(intLab) > 1){ 
#			intDat <- pData(exprObj)[[intLab[1]]]
#			for( v in intLab[-1] ){
#				intDat <- cbind(intDat, pData(exprObj)[[v]])
#			}
#
#			intNam <- paste("i", ".", 1:length(intLab), sep="")
#			colnames(intDat) <- intNam
#			trainDat <- data.frame(intVar=intDat[trainInd,], y=cl[trainInd], t(exprs(exprObj)[,trainInd]))
#			testDat <- data.frame(intVar=intDat[-trainInd,], y=cl[-trainInd], t(exprs(exprObj)[,-trainInd]))
#
#			intEqu <- paste(colnames(trainDat)[1:length(intLab)], collapse="+")
#			equ <- paste("y", intEqu, ".", sep="~")
#			equ <- as.formula(equ)
#		}
#
#		else{
#			intDat <- pData(exprObj)[[intLab]]
#			trainDat <- data.frame(intVar=intDat[trainInd], y=cl[trainInd], t(exprs(exprObj)[,trainInd]))
#			testDat <- data.frame(intVar=intDat[-trainInd], y=cl[-trainInd], t(exprs(exprObj)[,-trainInd]))
#			
#			equ <- as.formula(y ~ intVar ~ .)
#		}
#
#		dis <- dist(t(exprs(exprObj)[,-trainInd]), method=metric)
#library(ipred)
#		tmp <- inbagg(equ, pFun=pFun, cFun=cFun, data=trainDat,  ...)
#		out <- predict(tmp, newdata=testDat)
#                new("classifOutput", method="inbagg",
#                        predLabels=newPredClass(as.character(out)),
#			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
#                        predScores=newProbMat(out$posterior),
#                        RObject=tmp, call=match.call(), distMat=dis)
#})
#
######################
## title: inclassB
## description: interface to inclass {ipred} 
## arguments:
##	exprObj		ExpressionSet
##	classifLab	character string specifying what covariate data 
##			to use for classification
##	trainInd	
## 	intLab 		single label or a vector of labels
##			representing names of the factors for intermediate variables
##	metric		for distance matrix 
## value:
## 	object of class "classifPred"
## note:
## 	cFUN argument requires a function object which assigns
##	the response labels using the intermediate variable(s).
##	This function should have a single input argument "newdata".
##	Internally inclass applies this cFUN function to new data 
##	that is stored as a data frame in such a way that the 
##	intermediate variables occupy the last columns of the data
##	frame.
##	The example below uses a single intermediate variable ("PS" 
##	values) and samples with PS values > 0.75 are
##	classified as ALL (this is all made up, it's intended to illustrate
##	the function usage), otherwise AML. 
###
###
### -- Sep 12 2007 -- right ... easy to interface, hard to understand
### -- so no interfaces for inbagg or inclass until will have a plausible working example in genomics
###
## example:
## train <- c(sample(1:47, 23), sample(48:72, 12))
## psclassify <- function(newdata){
##		xx <- ifelse((newdata[,ncol(newdata)] > 0.75), "ALL", "AML")
##		as.factor(xx)
## }
## a <- inclassB(golubMerge[100:110,], "ALL.AML", train, "PS", cFUN=psclassify)
######################
#
#setGeneric("inclassB", function(exprObj, classifLab, trainInd, intLab, pFUN, cFUN, metric="euclidean", ...){
#		standardGeneric("inclassB")
#}) 
#
#setMethod("inclassB", c("ExpressionSet", "character", "integer", "character", "ANY", "ANY", "ANY", "ANY"), 
#			function(exprObj, classifLab, trainInd, intLab, pFUN, cFUN, metric, ...){
#
#		if(missing(pFUN)){ pFUN <- list(list(model=lm)) }
#	
#		cl <- pData(exprObj)[[classifLab]]
#	
#		if(length(intLab) > 1){ 
#
#			intDat <- pData(exprObj)[[intLab[1]]]
#			for( v in intLab[-1] ){
#				intDat <- cbind(intDat, pData(exprObj)[[v]])
#			}
#
#			intNam <- paste("i", ".", 1:length(intLab), sep="")
#			colnames(intDat) <- intNam
#
#			trainDat <- data.frame(intVar=intDat[trainInd,], y=cl[trainInd], t(exprs(exprObj)[,trainInd]))
#			testDat <- data.frame(intVar=intDat[-trainInd,], y=cl[-trainInd], t(exprs(exprObj)[,-trainInd]))
#	
#			intEqu <- paste(colnames(trainDat)[1:length(intLab)], collapse="+")
#			equ <- paste("y", intEqu, ".", sep="~")
#			equ <- as.formula(equ)
#		}
#
#		else{
#			intDat <- pData(exprObj)[[intLab]]
#			trainDat <- data.frame(intVar=intDat[trainInd], y=cl[trainInd], t(exprs(exprObj)[,trainInd]))
#			testDat <- data.frame(intVar=intDat[-trainInd], y=cl[-trainInd], t(exprs(exprObj)[,-trainInd]))
#			equ <- as.formula(y ~ intVar ~ .)
#		}
#
#		dis <- dist(t(exprs(exprObj)[,-trainInd]), method=metric)
#		out <- ipred::inclass(equ, pFUN=pFUN, cFUN=cFUN, data=trainDat, ...)
#		new("classifPred", sampLabels=predict(out, testDat, ...), distMat=dis, classifObj=out)
#})		
#####################
# PACKAGE: nnet
#####################
#
#####################
# title: nnetB
# description: interface to nnet {nnet}
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
# nnOut <- nnetB(golubMerge[100:200,], "ALL.AML", train)
# note: entropy, softmax, censored, linout (mutually exclusive) 
# are left to the user to specify if something other than the default
# is required (see nnet man page)
####################

setGeneric("nnetB", function(exprObj, classifLab, trainInd, weights, size=2, Wts, 
		mask, skip=FALSE, rang=0.7, decay=0, maxit=100, Hess=FALSE, trace=TRUE, MaxNWts=1000, 
		abstol=1.0e-4, reltol=1.0e-8, metric="euclidean", ...){
		standardGeneric("nnetB")
})


setMethod("nnetB", c("ExpressionSet", "character", "integer", "ANY", "ANY", 
		"ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", 
		"ANY", "ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, weights, size, Wts, mask, 
			skip, rang, decay, maxit, Hess, trace, MaxNWts, abstol, reltol, metric, ...){
		
			#.Deprecated("MLearn", "MLInterfaces")
			cl <- pData(exprObj)[[classifLab]][trainInd]

			trainDat <- data.frame(t(exprs(exprObj)[,trainInd]), sampLab = cl)
			testDat <- data.frame(t(exprs(exprObj)[,-trainInd]))

			dis <- dist(testDat, method=metric)
	
			if(missing(weights)){ weights <- rep(1,length(cl)) }
			if(missing(Wts)){ Wts <- NULL }
			if(missing(mask)){ mask <- NULL }
			out <- nnet::nnet(sampLab~., data=trainDat, weights=weights, size=size, skip=skip, 
						rang=rang, decay=decay, maxit=maxit, Hess=Hess, trace=trace, 
						MaxNWts=MaxNWts, abstol=abstol, reltol=reltol, ...) 

			new("classifOutput", method="nnet",
	predLabels=newPredClass(as.character(predict(out, testDat, type="class"))), 
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
        predScores=newProbMat(predict(out, newdata=testDat)), call=match.call(),
        distMat=dis, RObject=out)	
})		  
#####################
# PACKAGE: pamr
#####################
# 
#####################
# title: pamrB
# description: interface to pamr {pamr}
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
# pOut <- pamrB(golubMerge[100:200,], "ALL.AML", train)
####################

setGeneric("pamrB", function(exprObj, classifLab, trainInd, thresholdp=1, threshold, 
			n.threshold = 30, scale.sd = TRUE, threshold.scale, se.scale, 
			offset.percent = 50, prior, remove.zeros = TRUE, sign.contrast="both", 
			metric="euclidean"){
			standardGeneric("pamrB")
})
		
setMethod("pamrB", c("ExpressionSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", 
			"ANY", "ANY", "ANY", "ANY", "ANY", "ANY"),
			function(exprObj, classifLab, trainInd, thresholdp, threshold, n.threshold, 
			scale.sd, threshold.scale, se.scale, offset.percent, prior, remove.zeros, 
			sign.contrast="both", metric){
			
			if(missing(threshold)){ threshold <- NULL }
			if(missing(prior)){ prior <- NULL }
			if(missing(threshold.scale)){ threshold.scale <- NULL }
			if(missing(se.scale)){ se.scale <- NULL }

			cl <- pData(exprObj)[[classifLab]][trainInd]
			trainDat <- list(x=exprs(exprObj)[,trainInd], y = cl)
			testDat <- exprs(exprObj)[,-trainInd]
				
			dis <- dist(t(exprs(exprObj)[,-trainInd]), method=metric)
			
require(pamr)
			out <- pamr.train(trainDat, threshold=threshold, n.threshold=n.threshold, 
			scale.sd=scale.sd, threshold.scale=threshold.scale, se.scale=se.scale,
			offset.percent=offset.percent, prior=prior, remove.zeros=remove.zeros, 
			sign.contrast=sign.contrast)
			res <- pamr.predict(out, testDat, thresholdp)
                new("classifOutput", method="pamr",
                        predLabels=newPredClass(as.character(res)),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
                        predScores=newProbArray(out$prob),
                        RObject=out, call=match.call(), distMat=dis)

})
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

			#.Deprecated("MLearn", "MLInterfaces")
			.Defunct("MLearn",,"randomForestB is no longer available.  Please use MLearn method with randomForestI learnerSpec.")
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

			out <- randomForest::randomForest(data.frame(trainDat), y=cl, xtest=data.frame(testDat), ytest=ytest, addclass=addclass, 
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

			#.Deprecated("MLearn", "MLInterfaces")
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


setGeneric("stat.diag.daB", function(exprObj, classifLab, trainInd, 
 pool=1, metric="euclidean", ...) standardGeneric("stat.diag.daB") )

setMethod("stat.diag.daB", c("ExpressionSet", "character", "integer", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, pool,  
                       metric, ...){
		
			#.Deprecated("MLearn", "MLInterfaces")
			require(sma) # nice if it had a namespace
			cl <- pData(exprObj)[[classifLab]][trainInd]

			trainDat <- t(exprs(exprObj)[,trainInd])
			testDat <- t(exprs(exprObj)[,-trainInd])

			dis <- dist(testDat, method=metric)
	
			out <- stat.diag.da(trainDat,
   as.integer(factor(cl)), testDat, pool=pool )

			new("classifOutput", method="stat.diag.da",
	predLabels=newPredClass(as.character(out$pred)), 
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
        call=match.call(),
        distMat=dis, RObject=out)	
})		  
# description: interface to som {som} 
# arguments:
#	exprObj		ExpressionSet
#	xdim		x dimension
#	ydim		y dimension
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# somOut <- somB(golubMerge[100:200,], "ALL.AML", 2, 3)
#####################

setClass("somout", contains="list")
setMethod("show", "somout", function(object) {
 cat("somB output\n")
 print(object$call)
 cat("available elements:\n")
 print(names(object))
})

setGeneric("somB", function(exprObj, classifLab, xdim=3, ydim=3, init="linear", alpha, alphaType="inverse", 
		neigh="gaussian", topol="rect", radius, rlen, err.radius=1, inv.alp.c, metric="euclidean"){
		standardGeneric("somB")
})

setMethod("somB", c("ExpressionSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", 
		"ANY", "ANY"), 
		function(exprObj, classifLab, xdim, ydim, init, alpha, alphaType, neigh, topol, 
		radius, rlen, err.radius, inv.alp.c, metric){

		if(missing(alpha)){ alpha <- NULL }
		if(missing(radius)){ radius <- NULL }
		if(missing(rlen)){ rlen <- NULL }
		if(missing(inv.alp.c)){ inv.alp.c <- NULL }
		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		out <- som::som(dat, xdim, ydim, init=init, alpha=alpha, alphaType=alphaType, neigh=neigh,
				topol=topol, radius=radius, rlen=rlen, err.radius=err.radius,
				inv.alp.c=inv.alp.c)

		new("somout", list(method="som", somout=out, call=match.call()))
})
	


setClass("SOMBout", contains="list")
setMethod("show", "SOMBout", function(object) {
 cat("SOMB output\n")
 print(object$call)
 cat("available elements:\n")
 print(names(object))
})

setMethod("SOMB", c("ExpressionSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, kx, ky, topo, rlen, alpha, radii, init, metric){

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		sgrid <- class::somgrid(xdim=kx, ydim=ky, topo=topo)
		out <- class::SOM(dat, sgrid, rlen=rlen, alpha=alpha, radii=radii, init)
#		new("classifPred", sampLabels=pData(exprObj)[[classifLab]], distMat=dis, classifObj=out)
# this function has no commonality with the others, just return a list for now
		new("SOMBout", list(method="SOM", SOMout=out, SOMgrid=sgrid, distMat=dis,
			call=match.call()))

})
#####################
# PACKAGE: stats (formerly mva)
#####################

wrapClust <- function( out, k, height, dis )
{
                if (k>0 && height>0)
                        {
                        warning("both nclust(k) and height supplied, using nclust")
                        clinds <- newGroupIndex(cutree(out,k))
                        }
                else if (k > 0 && height == 0)
                        clinds <- newGroupIndex(cutree(out,k))
                else if (k == 0 && height > 0)
                        clinds <- newGroupIndex(cutree(as.hclust(out),h=height))                else clinds <- NA
                clsco <- newSilhouetteVec(cluster::silhouette( clinds, dis )[,3])
return(list(clinds=clinds, clsco=clsco))
}

#
#####################
# title: hclustB
# description: interface to hclust {stats}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# hOut <- hclustB(golubMerge[100:200,], "ALL.AML")
####################

setGeneric("hclustB", function(exprObj, k, height, method="complete", members, metric="euclidean"){
			standardGeneric("hclustB")
})
setMethod("hclustB", c("ExpressionSet", "numeric", "missing", "ANY", "ANY", "ANY"), 
			function(exprObj, k, height, method, members, metric)
			hclustB(exprObj=exprObj, k=k, height=0, method=method , 
				members=members, metric=metric))
setMethod("hclustB", c("ExpressionSet", "missing", "numeric", "ANY", "ANY", "ANY"), 
			function(exprObj, k, height, method, members, metric)
			hclustB(exprObj=exprObj, k=0, height=height, method=method , 
				members=members, metric=metric))

setMethod("hclustB", c("ExpressionSet", "numeric", "ANY", "ANY", "ANY", "ANY"), 
			function(exprObj, k, height, method, members, metric){

			if(missing(members)){ members <- NULL }

			dat <- t(exprs(exprObj))
			dis <- dist(dat, method=metric)
			out <- stats::hclust(dis, method=method, members=members)
#                if (k>0 && height>0) 
#                        {
#                        warning("both nclust(k) and height supplied, using nclust")
#                        clinds <- newGroupIndex(cutree(out,k))
#                        }
#                else if (k > 0 && height == 0)
#                        clinds <- newGroupIndex(cutree(out,k))
#                else if (k == 0 && height > 0)
#                        clinds <- newGroupIndex(cutree(as.hclust(out),h=height))                else clinds <- NA
#                clsco <- newSilhouetteVec(cluster::silhouette( clinds, dis )[,3])

                 tmp <- wrapClust(out, k, height, dis)

                new("clustOutput", method="hclust",
                        RObject=out, call=match.call(),
                        distMat=dis,
                        clustIndices=tmp$clinds, clustScores=tmp$clsco)

})

#####################
# title: kmeansB
# description: interface to kmeans {stats}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# kOut <- kmeansB(golubMerge[100:200,], "ALL.AML", 2)
####################

setGeneric("kmeansB", function(exprObj, k, iter.max=10, metric="euclidean"){
			standardGeneric("kmeansB")
})

setMethod("kmeansB", c("ExpressionSet", "numeric", "ANY", "ANY"), 
			function(exprObj, k, iter.max, metric){

			dat <- t(exprs(exprObj))
			dis <- dist(dat, method=metric)			
			out <- stats::kmeans(dis, centers=k, iter.max=iter.max)
                clinds <- newGroupIndex(out$cluster)
                clsco <- newSilhouetteVec(cluster::silhouette( clinds, dis )[,3])
                new("clustOutput", method="kmeans",
                        RObject=out, call=match.call(),
                        distMat=dis,
                        clustIndices=clinds, clustScores=clsco)
})

#####################
# title: prcompB
# description: interface to prcomp {stats}
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# prOut <- prcompB(golubMerge[100:200,], "ALL.AML")
####################
#
#setGeneric("prcompB", function(exprObj, classifLab, retx=TRUE, center=TRUE, scale.=FALSE, tol, metric="euclidean"){
#			standardGeneric("prcompB")
#})
#
#setMethod("prcompB", c("ExpressionSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY"), 
#			function(exprObj, classifLab, retx, center, scale., tol, metric){
#
#			if(missing(tol)){ tol <- NULL }
#
#			dat <- t(exprs(exprObj))
#			dis <- dist(dat, method=metric)
#			cl <- exprObj[[classifLab]]
#			row.names(dat) <- cl
#			out <- stats::prcomp(dat, retx=retx, center=center, scale.=scale., tol=tol)
#
#			new("classifPred", sampLabels=cl, distMat=dis, classifObj=out)
#
#})


#####################
# title: silhouetteB
# description: interface to silhouette {cluster} 
# arguments:
#	out	output of any function in machLI
#		i.e. a classifOutput object (or any of its 
#		extensions)
# value:
# 	object of class "silhouette"
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# knnOut <- knnB(golubMerge[100:200,], "ALL.AML", train) 
# agOut <- agnesB(golubMerge[100:200,], "ALL.AML")
# sil.knn <- silhouetteB(knnOut)
# sil.ag <- silhouetteB(agOut)
# plot(sil.ag, main="Silhouette Plot for Agnes Clustering")	
# plot(sil.knn, main="Silhouette Plot for knn Classification")
#####################

setGeneric("silhouetteB", function(out, ...){
	standardGeneric("silhouetteB")
})

setMethod("silhouetteB", c("classifOutput"), 
	function(out, ...){
	
	lab <- out@predLabels@.Data

	if(is.factor(lab)){ 
		lab <- as.integer(lab)
	}
        else if (is(lab, "character"))
 		lab = as.integer(factor(lab))
	cluster::silhouette(lab, out@distMat, ...)
})

setMethod("silhouetteB", c("clustOutput"), 
	function(out, ...){
	
	lab <- out@clustIndices@.Data

	if(is.factor(lab)){ 
		lab <- as.integer(lab)
	}
        else if (is(lab, "character"))
 		lab = as.integer(factor(lab))
	cluster::silhouette(lab, out@distMat, ...)
})


#####################
# title: cvB
# description: leave one out cross-validation classification
# arguments:
#	exprObj		ExpressionSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	algofunc	function object that calls the classification algorithm 
#			(can be created by makeCVFunc)
# value:
# 	object of class "classifCV"
# example:
# eset <- golubMerge[100:110,] 
# cvtknn <- cvB(eset, "ALL.AML", makeCVFunc("knn", k=10) )
# cvtnb <- cvB(eset, "ALL.AML", makeCVFunc("naiveBayes") )
####################

# nice but i don't think we need this any more, sep 12 2007 VJC

#setGeneric("cvB", function(exprObj, classifLab, algofunc, metric="euclidean", ...){
#	standardGeneric("cvB")
#})
#
#setMethod("cvB", c("ExpressionSet", "character", "ANY", "ANY"), 
#		function(exprObj, classifLab, algofunc, metric, ...){
#
#	cl <- pData(exprObj)[[classifLab]]
#	dat <- exprs(exprObj)
#	dis <- dist(t(dat), method=metric)
#	n <- ncol(dat)
#	predCV <- cl
#
#	for( i in 1:n ){
#		predCV[i] <- algofunc(t(dat[,-i]), t(dat[,i]), cl[-i], ...)
#	}
#
#	tab <- table(predCV, cl)
#	diag(tab) <- 0
#	err <- paste(round(100*sum(tab)/length(cl),2), "%", sep="")
##	new("classifCV", err=err, sampLabels=predCV, distMat=dis)
#                #out <- class::knn(trainDat, testDat, cl, k, l, prob, use.all)
#                new("classifOutput", method="knn",
#                        predLabels=newPredClass(as.character(predCV)),
#			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
##                        predScores=newQualScore(attr(out,"prob")),
#                        RObject=err, call=match.call(), distMat=dis)
#                                                                                
#
#})

#####################
# title: makeCVFunc
# description: creates function object for input to cvB
# arguments:
#	algorithm	character string specifying the name of the 
#			classification algorithm to be used
#			must be either: 
#				knn, knn1, knn.cv, lvq1, lvq2, lvq3,
#				olvq1, naiveBayes, svm, lda,
#				qda, bagging, ipredknn,
#				slda, randomForest, rpart, nnet, pamr
#			(i.e. all classification algorithms in machLI
#			except for knn.cv {class}, inbagg {ipred}, 
#			inclass {ipred})
#	...		additional parameters specific to the 
#			classification algorithm
# value:
# 	function object
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# aa <- pamrB(golubMerge[100:200,], train, "ALL.AML")
####################

#makeCVFunc <- function(algorithm, ...){
#
#	if( algorithm == "knn" ){		
#		resfunc <- function(train, test, lab){
#				class::knn(train, test, lab, ...)
#		}	
#	}
#
#	if( algorithm == "knn1" ){		
#		resfunc <- function(train, test, lab){
#				class::knn1(train, test, lab)
#		}	
#	}
#			
#	if( algorithm == "lvq1" ){		
#		resfunc <- function(train, test, lab){
#				initbk <- class::lvqinit(train, lab, ...)
#				trbk <- class::lvq1(train, lab, initbk, ...)
#				class::lvqtest(trbk, test)
#		}
#	}
#
#	if( algorithm == "lvq2" ){		
#		resfunc <- function(train, test, lab){
#				initbk <- class::lvqinit(train, lab, ...)
#				trbk <- class::lvq2(train, lab, initbk, ...)
#				class::lvqtest(trbk, test)
#		}
#	}
#
#	if( algorithm == "lvq3" ){		
#		resfunc <- function(train, test, lab){
#				initbk <- class::lvqinit(train, lab, ...)
#				trbk <- class::lvq3(train, lab, initbk, ...)
#				class::lvqtest(trbk, test)
#		}
#	}
#
#	if( algorithm == "olvq1" ){
#		
#		resfunc <- function(train, test, lab){
#				initbk <- class::lvqinit(train, lab, ...)
#				trbk <- class::olvq1(train, lab, initbk, ...)
#				class::lvqtest(trbk, test)
#		}
#	}	
#
#	if( algorithm == "naiveBayes" ){
#
#		resfunc <- function(train, test, lab){
#				df <- data.frame(y=lab, train)
#				classifObj <- e1071::naiveBayes(y~., data=df, ...)
#				predict(classifObj, test, ...)
#		}	
#	}
#
#	if( algorithm == "svm" ){
#		resfunc <- function(train, test, lab){
#				classifObj <- e1071::svm(train, lab, ...)
#				predict(classifObj, test, ...)
#		}	
#	}
#
#	if( algorithm == "lda" ){
#		resfunc <- function(train, test, lab){
#				classifObj <- MASS::lda(train, grouping=lab, ...)
#				predict(classifObj, test, ...)$class
#		}
#	}
#
#
#	if( algorithm == "qda" ){
#		resfunc <- function(train, test, lab){
#				classifObj <- MASS::qda(train, grouping=lab, ...)
#				predict(classifObj, test, ...)$class
#		}
#	}
#
#	if( algorithm == "bagging" ){
#		resfunc <- function(train, test, lab){
#				df <- data.frame(y=lab, train)
#				classifObj <- ipred::bagging(y~., data=df, ...)
#				predict(classifObj, data.frame(test), type="class", ...)
#		}	
#	}
#
#	if( algorithm == "ipredknn" ){
#
#		resfunc <- function(train, test, lab){
#				df <- data.frame(y=lab, train)
#				classifObj <- ipred::ipredknn(y~., data=df, ...)
#				ipred::predict.ipredknn(classifObj, data.frame(test), type="class", ...)
#		}	
#	}
#
#	if( algorithm == "slda" ){
#
#		resfunc <- function(train, test, lab){
#				df <- data.frame(y=lab, train)
#				classifObj <- ipred::slda(y~., data=df, ...)
#				predict(classifObj, data.frame(test), ...)$class
#		}	
#	}
#
#	if( algorithm == "randomForest" ){
#		resfunc <- function(train, test, lab){
#				classifObj <- randomForest::randomForest(train, y=lab, ...)
#				predict(classifObj, test, ...)
#		}	
#	}
#
#	if( algorithm == "rpart" ){
#		resfunc <- function(train, test, lab){
#				df <- data.frame(train, y=lab)
#				classifObj <- rpart::rpart(y~., data=df, ...)
#				predict(classifObj, data.frame(test), type="class")
#		}
#	}
#
#	if( algorithm == "nnet" ){
#		resfunc <- function(train, test, lab){
#				df <- data.frame(train, y=lab)
#				classifObj <- nnet::nnet(y~., data=df, ...)
#				predict(classifObj, data.frame(test), type="class", ...)
#		}	
#	}
#
#	if( algorithm == "pamr" ){
#		resfunc <- function(train, test, lab, threshold=1){
#				df <- list(x=t(train), y=lab)
#				classifObj <- pamr::pamr.train(df, ...)		
#				pamr::pamr.predict(classifObj, matrix(test, ncol=1), threshold, ...)
#		}	
#	}
#	resfunc
#}



setGeneric("gbmB", function(exprObj, classifLab, trainInd, 
 distribution="bernoulli", weights=NULL, 
 offset=NULL, var.monotone=NULL, 
 n.trees=100, interaction.depth=1, 
 n.minobsinnode=10, shrinkage=0.001, 
 bag.fraction=0.5, train.fraction=1, keep.data=TRUE, verbose=TRUE, metric="euclidean")
 standardGeneric("gbmB"))

setMethod("gbmB", c("ExpressionSet", "character", "integer", 
			"ANY", "ANY", "ANY", "ANY", "ANY", 
			"ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
 function(exprObj, classifLab, trainInd, 
  distribution="bernoulli", weights=NULL, 
  offset=NULL, var.monotone=NULL, 
  n.trees=100, interaction.depth=1, 
  n.minobsinnode=10, shrinkage=0.001, 
  bag.fraction=0.5, train.fraction=1, keep.data=TRUE, verbose=FALSE,
  metric = "euclidean") {
require(gbm) # no namespace
		cl <- pData(exprObj)[[classifLab]][trainInd]				
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		if (is.null(weights)) weights <- rep(1, length(cl))
		out <- gbm.fit(x=trainDat, y=as.numeric(cl)-1,
		  distribution=distribution, w=weights, 
		  offset=offset, var.monotone=var.monotone, 
		  n.trees=n.trees, interaction.depth=interaction.depth, 
		  n.minobsinnode=n.minobsinnode, shrinkage=shrinkage, 
		  bag.fraction=bag.fraction, 
		train.fraction=train.fraction, keep.data=keep.data, 
		verbose=verbose)
                ps <- predict( out, newdata= testDat, n.trees, type="response" )
                
                new("classifOutput", method="gbm", 
			predLabels=newPredClass(levels(cl)[(ps>0.5)+1]),
			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
#			predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
})

# the API for logitboost has changed and this will need nontrivial revision
#setGeneric("logitboostB",
# function(exprObj, classifLab, trainInd, mfinal=100, presel=0, estimate=0, verbose=FALSE, metric="euclidean")
# standardGeneric("logitboostB"))
#setMethod("logitboostB", 
# c("ExpressionSet", "character", "integer", "numeric", "ANY", "ANY", "ANY", "ANY"),
# function(exprObj, classifLab, trainInd, mfinal=100, presel=0, estimate=0, verbose=FALSE, metric="euclidean") {
#require(boost)
##warning("version 1.1 of LogitBoost has bugs.  consult with authors of that package if you run into problems with this method.")
#		cl <- pData(exprObj)[[classifLab]][trainInd]				
#		trainDat <- t(exprs(exprObj)[,trainInd])
#		testDat <- t(exprs(exprObj)[,-trainInd])
#		dis <- dist(testDat, method=metric)
#                out <- logitboost(trainDat, as.numeric(as.factor(cl))-1, 
#			testDat, mfinal, presel=presel, 
#			estimate=estimate, verbose=verbose)
#                if (length(dim(out$probs))==3)
#                  predcat <- apply(apply(out$probs,c(1,3),mean),1,which.max)
#	 	else predcat <- 1*(apply(out$probs,1,mean)>.5)+1
#
#                new("classifOutput", method="logitboost", 
#			predLabels=newPredClass(levels(as.factor(cl))[predcat]),
#			trainInds=trainInd, allClass=as.character(pData(exprObj)[[classifLab]]),
##			predScores=newQualScore(attr(out,"prob")),
#                        RObject=out, call=match.call(), distMat=dis)
#})
