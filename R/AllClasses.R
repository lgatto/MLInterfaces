# first comes the schema-based class collection, then
# the legacy classes for back-compatibility

## setClass("learnerSchema", representation(
## 	packageName="character",
## 	mlFunName="character",
## 	converter="function"), 
##           prototype=prototype(packageName="",
## 				mlFunName="",
## 				converter=function(obj, data, trainInd){}))

## L. Gatto <lg390@cam.ac.uk>, 1 Aug 2011
## new learnerSchema with predictor function
## - converters are defined in MLIConverters.R
## - predicters are defined in MLIPredicters.R
## This change requires to update the schema interfaces
## defined in schemaInterfaces.R by adding the
## predicter slot.
setClass("learnerSchema",
         representation(packageName="character",
                        mlFunName="character",
                        converter="function",
                        predicter="function"), 
         prototype=prototype(
           packageName="",
           mlFunName="",
           converter = function(obj, data, trainInd){},
           predicter = function(obj, newdata){
             .predClass <- predict(obj, newdata)
             ## no .predScores in standard predicter
             ## write specific ones to get prediction scores
             .predScores <- numeric()
             return(list(testPredictions=.predClass,
                         testScores=.predScores))
           }))

#setClass("clusteringSchema", representation(distMethod="character", 
#     agglomMethod="character", 
#     algorithm="character", extras="list"), contains="learnerSchema")

setClass("classifierOutput", representation(
        trainInd="numeric",
        testOutcomes="factor",
	testPredictions="factor",
	testScores="ANY",
	trainOutcomes="factor",
	trainPredictions="factor",
	trainScores="ANY",
        fsHistory="list",
	RObject="ANY",
	call="call",
	embeddedCV="logical",
        learnerSchema="learnerSchema"),
         prototype=prototype(testOutcomes=factor(),
           testPredictions=factor(),
           testScores=NULL,
           trainOutcomes=factor(),
           trainPredictions=factor(),
           trainScores=NULL,
           fsHistory=list(), 
           RObject=list(), 
           call=new("call"),
           embeddedCV=FALSE,
           learnerSchema=new("learnerSchema")))

#setClass("nonstandardLearnerSchema", representation(frontConverter="function",
#   hasNamespace="logical"), contains="learnerSchema")

#setOldClass("silhouette")
#
#setOldClass("prcomp")
#
#setClass("prcompObj", contains="prcomp")
#
#setClass("clusteringOutput", representation(
#	partition="integer", silhouette="silhouette", distEnv="environment",
#	prcomp="prcompObj",
#	metric="character", call="call", learnerSchema="learnerSchema",
#        RObject="ANY"))

setClass("xvalSpec",
         representation(type="character",
                        niter="numeric", 
                        partitionFunc="function",
                        fsFun="function"))

# constructor defined here for now

xvalSpec <- function(type,
                     niter = 0,
                     partitionFunc = function(data, classLab,iternum){ (1:nrow(data))[-iternum] },
                     fsFun = function(formula, data) formula ) {
  new("xvalSpec", type=type, niter=niter, partitionFunc=partitionFunc, fsFun=fsFun)
}

# -- below find the legacy classes as of sep 9 2007

# virual classes are defined with specializations to
#   a) class labels (as in classification outputs) vs
#       group indices (as in clustering outputs)
#   b) probability matrices (as with nnet predict) vs
#       vector scores (as in knn voting proportions)


setOldClass("dist")
