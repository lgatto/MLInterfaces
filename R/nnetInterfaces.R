#####################
# PACKAGE: nnet
#####################
#
#####################
# title: nnetB
# description: interface to nnet {nnet}
# arguments:
#	exprObj		exprSet
#	trainInd	vector of indices for the columns to be 
#			included in the training set
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# nnOut <- nnetB(golubMerge[100:200,], "ALL.AML", train)
# note: entropy, softmax, censored, linout (mutually exclusive) 
# are left to the user to specify if something other than the default
# is required (see nnet man page)
####################

setGeneric("nnetB", function(exprObj, classifLab, trainInd, weights, size=2, Wts, 
		mask, skip=FALSE, rang=0.7, decay=0, maxit=100, Hess=FALSE, trace=TRUE, MaxNWts=1000, 
		abstol=1.0e-4, reltol=1.0e-8, metric="euclidean", ...){
		standardGeneric("nnetB")
})


setMethod("nnetB", c("exprSet", "character", "integer", "ANY", "ANY", 
		"ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", 
		"ANY", "ANY", "ANY", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, weights, size, Wts, mask, 
			skip, rang, decay, maxit, Hess, trace, MaxNWts, abstol, reltol, metric, ...){
		
			cl <- exprObj[[classifLab]][trainInd]

			trainDat <- data.frame(t(exprs(exprObj)[,trainInd]), sampLab = cl)
			testDat <- data.frame(t(exprs(exprObj)[,-trainInd]))

			dis <- dist(testDat, method=metric)
	
			if(missing(weights)){ weights <- rep(1,length(cl)) }
			if(missing(Wts)){ Wts <- NULL }
			if(missing(mask)){ mask <- NULL }
			out <- nnet::nnet(sampLab~., data=trainDat, weights=weights, size=size, skip=skip, 
						rang=rang, decay=decay, maxit=maxit, Hess=Hess, trace=trace, 
						MaxNWts=MaxNWts, abstol=abstol, reltol=reltol, ...) 

			new("classifOutput", method="nnet",
	predLabels=newPredClass(as.character(predict(out, testDat, type="class"))), 
			trainInds=trainInd, allClass=as.character(exprObj[[classifLab]]),
        predScores=newProbMat(predict(out, newdata=testDat)), call=match.call(),
        distMat=dis, RObject=out)	
})		  
