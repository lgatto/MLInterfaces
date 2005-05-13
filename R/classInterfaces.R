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
 ans <- class::knn(train,test,cl,k,l,prob,use.all)
 nf <- function(train,cl,k,l,prob,use.all) function(newdata)
	 class::knn(train,newdata,cl,k,l,prob,use.all)
 attr(ans, "predfun") <- nf(train,cl,k,l,prob,use.all)
 class(ans) <- c("knnP", "factor")
 ans
}

predict.knnP <- function(object, ...) 
	attr(object, "predfun")(...)

print.knnP <- function(x, ...)
	{
	cat("instance of knnP [predictable knn object]\n")
	NextMethod()
	}

setGeneric("knnB", function(exprObj, classifLab, trainInd, 
		k=1, l=1, prob=TRUE, use.all=TRUE, metric="euclidean"){
			standardGeneric("knnB")
		})

setMethod("knnB", c("exprSet", "character", "integer", 
			"ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, k, l, 
			prob, use.all, metric){

		cl <- exprObj[[classifLab]][trainInd]				
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		out <- knnP(trainDat, testDat, cl, k, l, prob, use.all)
                new("classifOutput", method="knn", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=trainInd, allClass=as.character(exprObj[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
                                                                                
})


#####################
# title: knn.cvB
# description: interface to knn.cv {class}
# arguments:
#	exprObj		exprSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classif2Output"
# example:
# knn.cvOut <- knn.cvB(golubMerge[101:140,], "ALL.AML")
####################

setGeneric("knn.cvB", function(exprObj, classifLab, trainInd=NULL, k=1, l=1, prob=TRUE, use.all=TRUE, metric="euclidean"){
		standardGeneric("knn.cvB")
})

setMethod("knn.cvB", c("exprSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY"), 
			function(exprObj, classifLab, trainInd=NULL, k, l, prob, use.all, metric){
			if (!is.null(trainInd)) warning("disregarding trainInd for knn.cvB")
			cl <- exprObj[[classifLab]]
			dat <- t(exprs(exprObj))
			dis <- dist(dat, method=metric)
			out <- class::knn.cv(dat, cl, k, l, prob, use.all)
                new("classifOutput", method="knn.cv", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=integer(0), allClass=as.character(exprObj[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
})

#####################
# title: knn1B
# description: interface to knn1 {class}
# arguments:
#	exprObj		exprSet
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

setMethod("knn1B", c("exprSet", "character", "integer", "ANY"), 
		function(exprObj, trainInd, classifLab, metric){
				
		cl <- exprObj[[classifLab]][trainInd]
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		out <- class::knn1(trainDat, testDat, cl)
                new("classifOutput", method="knn1", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=trainInd, allClass=as.character(exprObj[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
})

#####################
# title: lvq1B
# description: interface to lvq1 {class}
# arguments:
#	exprObj		exprSet
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

setMethod("lvq1B", c("exprSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, size, prior, k, niter, alpha, metric){

		if(missing(size)){ size <- NULL }
		cl <- exprObj[[classifLab]][trainInd]
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)

		cbkInit <- class::lvqinit(trainDat, cl, size=size, prior=prior, k=k)
		if(missing(niter)){ niter <- 100 * nrow(cbkInit$x) } 
		cbkTrain <- class::lvq1(trainDat, cl, cbkInit, niter=niter)
		out <- class::lvqtest(cbkTrain, testDat)
                new("classifOutput", method="lvq1", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=trainInd, allClass=as.character(exprObj[[classifLab]]),
			#predScores=newQualScore(attr(out,"prob")),
                        RObject=cbkTrain, call=match.call(), distMat=dis)
})	

#####################
# title: lvq2B
# description: interface to lvq2 {class}
# arguments:
#	exprObj		exprSet
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

setMethod("lvq2B", c("exprSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", 
		"ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, size, prior, k, 
			niter, alpha, win, metric){

		if(missing(size)){ size <- NULL }
		cl <- exprObj[[classifLab]][trainInd]
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
			trainInds=trainInd, allClass=as.character(exprObj[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=cbkTrain, call=match.call(), distMat=dis)
	
})	

#####################
# title: lvq3B
# description: interface to lvq3 {class}
# arguments:
#	exprObj		exprSet
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

setMethod("lvq3B", c("exprSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, size, prior, k, niter, alpha, win, epsilon, metric){

		if(missing(size)){ size <- NULL }
		cl <- exprObj[[classifLab]][trainInd]			
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
			trainInds=trainInd, allClass=as.character(exprObj[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=cbkTrain, call=match.call(), distMat=dis)
	
})	

#####################
# title: olvq1B
# description: interface to olvq1 {class}
# arguments:
#	exprObj		exprSet
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

setMethod("olvq1B", c("exprSet", "character", "integer", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, size, prior, k, niter, alpha, metric){

		if(missing(size)){ size <- NULL }
		cl <- exprObj[[classifLab]][trainInd]
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		cbkInit <- class::lvqinit(trainDat, cl, size=size, prior=prior)
		if(missing(niter)){ niter <- 100 * nrow(cbkInit$x) } 
		cbkTrain <- class::olvq1(trainDat, cl, cbkInit, niter=niter, alpha=alpha)
		out <- class::lvqtest(cbkTrain, testDat)
                new("classifOutput", method="olvq1", 
			predLabels=newPredClass(as.character(out)), 
			trainInds=trainInd, allClass=as.character(exprObj[[classifLab]]),
			predScores=newQualScore(attr(out,"prob")),
                        RObject=cbkTrain, call=match.call(), distMat=dis)

})	

#####################
# title: SOMB
# description: interface to SOM {class}
# arguments:
#	exprObj		exprSet
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

setGeneric("SOMB", function(exprObj, classifLab, kx, ky, topo="hexagonal", rlen=10000, 
		alpha=seq(0.05, 0, len=rlen), 
		radii=seq(4, 1, len = rlen), init, metric="euclidean"){
		standardGeneric("SOMB")
})

# a special container is provided for SOMB in INIT.R
