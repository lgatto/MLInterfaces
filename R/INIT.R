##PLEASE PUT THE CLASSES FIRST!!!! THEN THE METHODS
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


setOldClass("dist")
setClass("MLLabel", "VIRTUAL")
setClass("predClass", contains=c("MLLabel", "character", "factor"),
	prototype=prototype(""))
setClass("groupIndex", contains=c("MLLabel", "integer"),
	prototype=prototype(integer(0)))
newPredClass <- function(x) new("predClass", x)
newGroupIndex <- function(x) new("groupIndex", x)

setClass("MLScore", "VIRTUAL")
setClass("probMat", contains=c("MLScore", "matrix"))
setClass("probArray", contains=c("MLScore", "array"))
setClass("membMat", contains=c("MLScore", "matrix"))
setClass("qualScore", contains=c("MLScore", "numeric"))
setClass("silhouetteVec", contains=c("MLScore", "numeric"))
newProbMat <- function(x) if(length(x)>0)new("probMat", x) else new("probMat")
newProbArray <- function(x) if(length(x)>0)new("probArray", x) else new("probArray")
newMembMat <- function(x) if(length(x)>0)new("membMat", x) else new("membMat")
newQualScore <- function(x) if(length(x)>0)new("qualScore", x) else new("qualScore")
newSilhouetteVec <- function(x) if(length(x)>0)new("silhouetteVec", x) else new("silhouetteVec")

setClass("MLOutput", representation(method="character",
			RObject="ANY", call="call", distMat="dist"), "VIRTUAL")

setClass("classifOutput", representation(
	predLabels="MLLabel", predScores="MLScore", predLabelsTr="MLLabel",
	trainInds="integer", allClass="character"), contains="MLOutput",
		prototype=prototype(method="", RObject=NULL,
			call=match.call(), distMat=dist(0), 
			allClass=character(0), trainInds=integer(0),
			predLabels=newPredClass(character(0)),
			predLabelsTr=newPredClass(character(0)),
			predScores=newQualScore(numeric(0))))

setClass("clustOutput", representation(
	clustIndices="MLLabel", clustScores="MLScore"), contains="MLOutput",
		prototype=prototype(method="", RObject=NULL,
			call=match.call(), distMat=dist(0),
			clustIndices=newGroupIndex(integer(0)),
			clustScores=newSilhouetteVec(numeric(0))))

########################################################

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
setGeneric("confuMat", function(obj) standardGeneric("confuMat"))
setMethod("confuMat", "classifOutput", function(obj) 
table(given=allClass(obj)[-trainInds(obj)], predicted=predLabels(obj) ) )
setGeneric("confuMatTrain", function(obj) standardGeneric("confuMatTrain"))
setMethod("confuMatTrain", "classifOutput", function(obj) {
acal = function(x) as.character(as.list(x))
maker = acal(obj@call) # test for MLearn origin
if (maker[1] != "MLearn") stop("confuMatTrain only applicable to outputs of MLearn interface.  See help(MLearn).")
table(given=allClass(obj)[trainInds(obj)], predicted=predLabelsTr(obj) ) 
 })
