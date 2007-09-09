# premises -- MLearn will create a learner on a training set
# and allow evaluation on a test set

setClass("classifierOutput", representation(
        testOutcomes="factor",
	testPredictions="factor",
	testScores="ANY",
	trainOutcomes="factor",
	trainPredictions="factor",
	trainScores="ANY",
	RObject="ANY",
	call="call"))

setClass("learnerSchema", representation(
	packageName="character",
	mlFunName="character",
	converter="function")) #,
#	selftesting="logical"))

setClass("nonstandardLearnerSchema", representation(frontConverter="function",
   hasNamespace="logical"), contains="learnerSchema")


