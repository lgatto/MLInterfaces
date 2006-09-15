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
