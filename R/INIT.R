#
# the class structure has been changed.  labels and scores
# are fundamental and can have different structures for
# different procedures.  so
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
setClass("MLOutput", representation(method="character",
			RObject="ANY", call="call", distMat="dist"), "VIRTUAL")

setClass("classifOutput", representation(
	predLabels="MLLabel", predScores="MLScore"), contains="MLOutput",
		prototype=prototype(method="", RObject=NULL,
			call=match.call(), distMat=dist(0),
			predLabels=newPredClass(character(0)),
			predScores=newQualScore(numeric(0))))

setClass("clustOutput", representation(
	clustIndices="MLLabel", clustScores="MLScore"), contains="MLOutput",
		prototype=prototype(method="", RObject=NULL,
			call=match.call(), distMat=dist(0),
			clustIndices=newGroupIndex(integer(0)),
			clustScores=newQualScore(numeric(0))))


setMethod("show", "MLOutput", function(object) {
	cat("MLOutput instance, method=", object@method, "\n")
	if (object@method == "nnet")
		print(object@RObject)
	if (length(object@call)>0) print (object@call)
        if (is(object, "classifOutput") && length(object@predLabels)>0) {
		cat("predicted class distribution:")
		print(table(object@predLabels))
	}
        else if (is(object, "clustOutput") && length(object@clustIndices)>0) {
		cat("predicted cluster size distribution:")
		print(table(object@clustIndices))
	}
        if (object@method %in% c("nnet", "knn"))
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
