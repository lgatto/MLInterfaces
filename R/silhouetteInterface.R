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
	cluster::silhouette(lab, out@distMat, ...)
})

