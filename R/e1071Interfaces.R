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
