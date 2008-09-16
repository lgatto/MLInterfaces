setMethod("show", "probMat", function(object) {
	if (length(object)>0) {
		cat("summary of class membership probabilities:\n")
		print(apply(object,2,summary))
	}
})
setMethod("show", "probArray", function(object) {
	cat("dimensions of (threshold-based) class membership probabilities:\n")
	print(dim(object))
})
setMethod("show", "membMat", function(object) {
	cat("summary of cluster membership scores:\n")
	print(apply(object,2,summary))
})
setMethod("show", "qualScore", function(object) {
   if (length(object)>0)
	{
	cat("summary of class assignment quality scores:\n")
	print(summary(object))
        }
   else invisible(NULL)
})
setMethod("show", "silhouetteVec", function(object) {
	cat("summary of clustering silhouette values:\n")
	print(summary(object))
})

#
# the base output representation class now just knows
# about the MLLabel and MLScore output classes,
# but now retains call, fitted model object, and dist
#
setGeneric("RObject", function(obj) standardGeneric("RObject"))
setMethod("RObject", "MLOutput", function(obj) obj@RObject)
setGeneric("distMat", function(obj) standardGeneric("distMat"))
setMethod("distMat", "MLOutput", function(obj) obj@distMat)



setMethod("show", "MLOutput", function(object) {
	cat("MLOutput instance, method=", object@method, "\n")
	if (object@method == "nnet")
		print(object@RObject)
	if (length(object@call)>0) {cat("Call:\n "); print (object@call)}
        if (is(object, "classifOutput") && length(object@predLabels)>0) {
		cat("predicted class distribution:")
		print(table(object@predLabels))
		show(object@predScores)
	}
        else if (is(object, "clustOutput") && length(object@clustIndices)>0) {
		cat("predicted cluster size distribution:")
		print(table(object@clustIndices))
		show(object@clustScores)
	}
})

setGeneric("predLabels", function(obj) standardGeneric("predLabels"))
setMethod("predLabels", "MLOutput", function(obj) obj@predLabels@.Data)
setGeneric("predLabelsTr", function(obj) standardGeneric("predLabelsTr"))
setMethod("predLabelsTr", "MLOutput", function(obj) obj@predLabelsTr@.Data)
setGeneric("predLabels", function(obj) standardGeneric("predLabels"))
setMethod("predLabels", "classifOutput", function(obj) obj@predLabels@.Data)
setGeneric("predLabelsTr", function(obj) standardGeneric("predLabelsTr"))
setMethod("predLabelsTr", "classifOutput", function(obj) obj@predLabelsTr@.Data)
setGeneric("allClass", function(obj) standardGeneric("allClass"))
setMethod("allClass", "classifOutput", function(obj) obj@allClass)
setGeneric("trainInds", function(obj) standardGeneric("trainInds"))
setMethod("trainInds", "classifOutput", function(obj) obj@trainInds)
#setGeneric("confuMat", function(obj,type) standardGeneric("confuMat"))
#setMethod("confuMat", c("classifOutput","missing"), function(obj,type) 
#  table(given=allClass(obj)[-trainInds(obj)], predicted=predLabels(obj) ) )
