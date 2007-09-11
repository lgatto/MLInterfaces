# first comes the schema-based class collection, then
# the legacy classes for back-compatibility

setClass("learnerSchema", representation(
	packageName="character",
	mlFunName="character",
	converter="function")) 

setClass("classifierOutput", representation(
        testOutcomes="factor",
	testPredictions="factor",
	testScores="ANY",
	trainOutcomes="factor",
	trainPredictions="factor",
	trainScores="ANY", fsHistory="list",
	RObject="ANY",
	call="call"))

setClass("nonstandardLearnerSchema", representation(frontConverter="function",
   hasNamespace="logical"), contains="learnerSchema")

#setClassUnion("funcOrNull", c("function", "NULL"))
setClass("xvalSpec", representation(type="character", niter="numeric", partitionFunc="function", fsFun="function"))

# constructor defined here for now

 xvalSpec = function(type, niter=0, partitionFunc=function(data, classLab, 
       iternum){(1:nrow(data))[-iternum]}, fsFun=function(formula, data)formula) {
  new("xvalSpec", type=type, niter=niter, partitionFunc=partitionFunc, fsFun=fsFun)
  }

# -- below find the legacy classes as of sep 9 2007

# virual classes are defined with specializations to
#   a) class labels (as in classification outputs) vs
#       group indices (as in clustering outputs)
#   b) probability matrices (as with nnet predict) vs
#       vector scores (as in knn voting proportions)


setOldClass("dist")

# MLOutput -- basic ML result container framework

setClass("MLOutput", representation(method="character",
			RObject="ANY", call="call", distMat="dist"), "VIRTUAL")

# MLLabel -- container for object labels in categorical prediction/classification
#            or for labels created in clustering

setClass("MLLabel", "VIRTUAL")
setClass("predClass", contains=c("MLLabel", "character", "factor"),
	prototype=prototype(""))
setClass("groupIndex", contains=c("MLLabel", "integer"),
	prototype=prototype(integer(0)))

# MLScore -- container for posterior probability structures in classification
#         -- or for silhouette, e.g., in clustering

setClass("MLScore", "VIRTUAL")
setClass("probMat", contains=c("MLScore", "matrix"))
setClass("probArray", contains=c("MLScore", "array"))
setClass("membMat", contains=c("MLScore", "matrix"))
setClass("qualScore", contains=c("MLScore", "numeric"))
setClass("silhouetteVec", contains=c("MLScore", "numeric"))
setClass("classifOutput", representation(
	predLabels="MLLabel", predScores="MLScore", predLabelsTr="MLLabel",
	trainInds="integer", allClass="character"), contains="MLOutput",
		prototype=prototype(method="", RObject=NULL,
			call=new("call"), distMat=dist(0), 
			allClass=character(0), trainInds=integer(0),
			predLabels=newPredClass(character(0)),
			predLabelsTr=newPredClass(character(0)),
			predScores=newQualScore(numeric(0))))

setClass("clustOutput", representation(
	clustIndices="MLLabel", clustScores="MLScore"), contains="MLOutput",
		prototype=prototype(method="", RObject=NULL,
			call=new("call"), distMat=dist(0),
			clustIndices=newGroupIndex(integer(0)),
			clustScores=newSilhouetteVec(numeric(0))))

########################################################

