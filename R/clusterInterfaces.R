#####################
# PACKAGE: cluster
#####################
#
#####################
# title: agnesB
# description: interface to agnes {cluster} 
# arguments:
#	exprObj		exprSet
#	classifLab	character string specifying what covariate data to use for classification
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# agOut <- agnesB(golubMerge[100:200,], "ALL.AML")
#####################

setGeneric("agnesB", function(exprObj, nclust, height, stand=FALSE, method="average",
		keep.diss=TRUE, keep.data=TRUE, metric="euclidean", ...){
		standardGeneric("agnesB")
})

setMethod("agnesB", c("exprSet", "numeric", "numeric", 
                "logical", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, nclust, height, stand, method, keep.diss, keep.data, metric, ...){

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
#		row.names(dat) <- exprObj[[classifLab]]
		
		out <- cluster::agnes(dat, metric=metric, stand=stand, method=method, 
					keep.diss=keep.diss, keep.data=keep.data)
		if (nclust>0 && height>0) warning("both nclust and height supplied, using nclust")
                else if (nclust > 0 && height == 0)
                        clinds <- newGroupIndex(cutree(out,nclust))
                else if (nclust == 0 && height > 0)
                        clinds <- newGroupIndex(cutree(as.hclust(out),h=height))
		else clinds <- NA
                clsco <- newQualScore(cluster::silhouette( clinds, dis )[,3])
		new("clustOutput", method="agnes",
			RObject=out, call=match.call(),
			distMat=dis,
			clustIndices=clinds, clustScores=clsco)
})

#####################
# title: claraB
# description: interface to clara {cluster}
# arguments:
#	exprObj		exprSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# clObj <- claraB(golubMerge[100:200,], "ALL.AML", 2)
#####################

setGeneric("claraB", function(exprObj, classifLab, k, stand=FALSE, 
		samples=5, sampsize, trace=0, keep.data=TRUE, keepdata, rngR=FALSE, metric="euclidean"){
		standardGeneric("claraB")
})

setMethod("claraB", c("exprSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", 
		"ANY", "ANY"), function(exprObj, classifLab, k, stand, samples, sampsize,
		trace, keep.data, keepdata, rngR, metric){

		if(missing(sampsize)){ sampsize <- 40 + 2*k }

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		row.names(dat) <- exprObj[[classifLab]]
		out <- cluster::clara(dat, k=k, metric=metric, stand=stand, samples=samples, 							sampsize=sampsize, trace=trace, keep.data=keep.data, keepdata=keepdata, rngR= rngR)	
		new("classifPred", sampLabels=out$clustering, distMat=dis, classifObj=out)
})

#####################
# title: dianaB
# description: interface to diana {cluster}
# arguments:
#	exprObj		exprSet
#	classifLab	character string specifying what covariate data to use for classification
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# diOut <- dianaB(golubMerge[100:200,], "ALL.AML")
#####################

setGeneric("dianaB", function(exprObj, classifLab, diss, stand=FALSE, 
		keep.diss, keep.data=TRUE, metric="euclidean", ...){
		standardGeneric("dianaB")
})

setMethod("dianaB", c("exprSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, diss, stand, keep.diss, keep.data, metric, ...){

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		row.names(dat) <- exprObj[[classifLab]]

		out <- cluster::diana(dat, diss=F, metric=metric, stand=stand, keep.diss=T,
					keep.data=keep.data)
		new("classifPred", sampLabels=exprObj[[classifLab]], distMat=dis, classifObj=out)
})

#####################
# title: fannyB
# description: interface to fanny {cluster}
# arguments:
#	exprObj		exprSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# faOut <- fannyB(golubMerge[100:200,], "ALL.AML", 2)
#####################

setGeneric("fannyB", function(exprObj, classifLab, k, diss, stand=FALSE, metric="euclidean", ...){
		standardGeneric("fannyB")
}) 

setMethod("fannyB", c("exprSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, k, diss, stand, metric, ...){

		if(missing(diss)){ diss <- F }

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		row.names(dat) <- exprObj[[classifLab]]
		out <- cluster::fanny(dat, k=k, diss=diss, metric=metric, stand=stand)

		new("classifPred", sampLabels=out$clustering, distMat=dis, classifObj=out)
})

#####################
# title: pamB
# description: interface to pam {cluster}
# arguments:
#	exprObj		exprSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# paOut <- pamB(golubMerge[100:200,], "ALL.AML", 2)
#####################

setGeneric("pamB", function(exprObj, classifLab, k, diss, stand=FALSE, 
		keep.diss=TRUE, keep.data=TRUE, metric="euclidean"){
		standardGeneric("pamB")
})

setMethod("pamB", c("exprSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, k, diss, stand, keep.diss, keep.data, metric){

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		row.names(dat) <- exprObj[[classifLab]]
		out <- cluster::pam(dat, k=k, diss=FALSE, metric=metric, stand=stand, keep.diss=keep.diss, 
					keep.data=keep.data)
			
		new("classifPred", sampLabels=out$clustering, distMat=dis, classifObj=out)
})

#####################
# title: monaB
# description: interface to mona {cluster}
# arguments:
#	exprObj		exprSet
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

setGeneric("monaB", function(exprObj, classifLab, metric="euclidean"){
		standardGeneric("monaB")
})

setMethod("monaB", c("exprSet", "character", "ANY"), 
		function(exprObj, classifLab, metric){

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		row.names(dat) <- exprObj[[classifLab]]
		out <- cluster::mona(dat)

		new("classifPred", sampLabels=exprObj[[classifLab]], distMat=dis, classifObj=out)
})
