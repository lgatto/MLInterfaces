#
# the class structure has been changed.  labels and scores
# are fundamental and can have different structures.  so
# virual classes are defined with specializations to
#   a) class labels (as in classification outputs) vs
#       group indices (as in clustering outputs)
#   b) probability matrices (as with nnet predict) vs
#       vector scores (as in knn voting proportions)
#
#

setClass("MLLabel", "VIRTUAL")
setClass("predClass", contains=c("MLLabel", "character"),
	prototype=prototype(""))
setClass("groupIndex", contains=c("MLLabel", "integer"),
	prototype=prototype(integer(0)))
newPredClass <- function(x) new("predClass", x)
newGroupIndex <- function(x) new("groupIndex", x)

setClass("MLScore", "VIRTUAL")
setClass("probMat", contains=c("MLScore", "matrix"))
setClass("qualScore", contains=c("MLScore", "vector"))
newProbMat <- function(x) if(length(x)>0)new("probMat", x) else new("probMat")
newQualScore <- function(x) if(length(x)>0)new("qualScore", x) else new("qualScore")

#
# the base output representation class now just knows
# about the MLLabel and MLScore output classes,
# but now retains call, fitted model object, and dist
#

setClass("classifOutput", representation(method="character",
	predLabels="MLLabel", predScores="MLScore", 
	call="call", classifRobject="ANY", distMat="dist"),
		prototype=prototype(method="",
			predLabels=newPredClass(character(0)),
			predScores=newQualScore(numeric(0))))


setMethod("show", "classifOutput", function(object) {
	cat("classifOutput instance, method", object@method, "\n")
	if (object@method == "nnet")
		print(object@classifRobject)
	if (length(object@call)>0) print (object@call)
        if (length(object@predLabels)>0) {
		cat("predicted class distribution:")
		print(table(object@predLabels))
	}
        if (length(object@predScores)>0 && object@method %in% c("nnet", "knn"))
           {
	   if (is(object@predScores,"qualScore"))
	   	{
		cat("summary of labeling scores:\n")
		print(summary(object@predScores))
		}
	   else if (is(object@predScores,"probMat"))
		{
		cat("summary of class probabilities:\n")
		print(apply(object@predScores,2,summary))
		}
            }
	})
