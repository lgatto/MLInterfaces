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

setGeneric("agnesB", function(exprObj, k, height=0, stand=FALSE, method="average",
		keep.diss=TRUE, keep.data=TRUE, metric="euclidean", ...){
		standardGeneric("agnesB")
})

setMethod("agnesB", c("exprSet", "numeric", "ANY", 
                "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, k, height, stand, method, keep.diss, keep.data, metric, ...){

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
#		row.names(dat) <- exprObj[[classifLab]]
		
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
#	exprObj		exprSet
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

setMethod("claraB", c("exprSet", "numeric", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", 
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
#	exprObj		exprSet
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

setMethod("dianaB", c("exprSet", "numeric", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
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
#	exprObj		exprSet
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

setMethod("fannyB", c("exprSet", "numeric", "ANY", "ANY", "ANY", "ANY"), 
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
#	exprObj		exprSet
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

setMethod("pamB", c("exprSet", "numeric", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
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

#setGeneric("monaB", function(exprObj, classifLab, metric="euclidean"){
#		standardGeneric("monaB")
#})
#
#setMethod("monaB", c("exprSet", "character", "ANY"), 
#		function(exprObj, classifLab, metric){
#
#		dat <- t(exprs(exprObj))
#		dis <- dist(dat, method=metric)
#		row.names(dat) <- exprObj[[classifLab]]
#		out <- cluster::mona(dat)
#
#		new("classifPred", sampLabels=exprObj[[classifLab]], distMat=dis, classifObj=out)
##})
