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
setClass("predClass", contains=c("MLLabel", "character"))
setClass("groupIndex", contains=c("MLLabel", "integer"))
newPredClass <- function(x) new("predClass", x)
newGroupIndex <- function(x) new("groupIndex", x)

setClass("MLScore", "VIRTUAL")
setClass("probMat", contains=c("MLScore", "matrix"))
setClass("qualScore", contains=c("MLScore", "vector"))
newProbMat <- function(x) new("probMat", x)
newQualScore <- function(x) new("qualScore", x)

#
# the base output representation class now just knows
# about the MLLabel and MLScore output classes,
# but now retains call, fitted model object, and dist
#

setClass("classifOutput", representation(method="character",
	predLabels="MLLabel", predScores="MLScore", 
	call="call", classifRobject="ANY", distMat="dist"))
