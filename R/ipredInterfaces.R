#####################
# PACKAGE: ipred
#####################
#
#####################
# title: baggingB
# description: interface to bagging {ipred} 
# arguments:
#	exprObj		exprSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# note:
# 	aggregation	argument specifying how to combine results for prediction 
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# hg <- baggingB(golubMerge[100:200,], "ALL.AML", train)
#####################

setGeneric("baggingB", function(exprObj, classifLab, trainInd, subset, aggregation="majority", metric="euclidean", ...){
		standardGeneric("baggingB")
})

setMethod("baggingB", c("exprSet", "character", "integer", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, subset, aggregation, metric, ...){

		if(missing(subset)){ subset <- NULL }

		cl <- exprObj[[classifLab]][trainInd]		
		trainDat <- data.frame(y=cl, t(exprs(exprObj)[,trainInd]))
		testDat <- data.frame(t(exprs(exprObj)[,-trainInd]))
		dis <- dist(testDat, method=metric)
library(ipred)
		tmp <- bagging(y~., data=trainDat, subset=subset, ...)
		out <- predict(tmp, newdata=testDat)
                new("classifOutput", method="bagging",
                        predLabels=newPredClass(as.character(out)),
#                        predScores=newQualScore(attr(out,"prob")),
                        RObject=tmp, call=match.call(), distMat=dis)
})

#####################
# title: ipredknnB
# description: interface to ipredknn {ipred} 
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
# hg <- ipredknnB(golubMerge[100:200,], "ALL.AML", train)
# note: would be great if we could get prob and class predictions
#####################

setGeneric("ipredknnB", function(exprObj, classifLab, trainInd, na.action, k=5, metric="euclidean", ...){
		standardGeneric("ipredknnB")
})

setMethod("ipredknnB", c("exprSet", "character", "integer", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, na.action, k, metric, ...){
				
		if(missing(na.action)){ na.action <- NULL }
		cl <- exprObj[[classifLab]][trainInd]		
		trainDat <- data.frame(y = cl, t(exprs(exprObj)[,trainInd]))
		testDat <- data.frame(t(exprs(exprObj)[,-trainInd]))
		dis <- dist(testDat, method=metric)
library(ipred)
		tmp <- ipredknn(y~., data=trainDat, na.action=na.action, k=k, ...)
		out <- predict(tmp, newdata=testDat, type="class")
		prob <- predict(tmp, newdata=testDat, type="prob")
                new("classifOutput", method="ipredknn",
                        predLabels=newPredClass(as.character(out)),
                        predScores=newQualScore(prob),
                        RObject=tmp, call=match.call(), distMat=dis)
})

#####################
# title: sldaB
# description: interface to slda {ipred} 
# arguments:
#	exprObj		exprSet
#	trainInd	vector of indices for the columns to be 
#			included in the training set
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	metric		for distance matrix 
# value:
# 	object of class "classif3Output"
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# hg <- sldaB(golubMerge[100:200,], "ALL.AML", train)
#####################

setGeneric("sldaB", function(exprObj, classifLab, trainInd, subset, na.action=na.rpart, metric="euclidean", ...){
		standardGeneric("sldaB")
})

setMethod("sldaB", c("exprSet", "character", "integer", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, subset, na.action, metric, ...){

		if(missing(subset)){ subset <- NULL }
		cl <- exprObj[[classifLab]][trainInd]		
		trainDat <- data.frame(y=cl, t(exprs(exprObj)[,trainInd]))			
		dat <- data.frame(t(exprs(exprObj)[,trainInd]))
		testDat <- data.frame(t(exprs(exprObj)[,-trainInd]))
		dis <- dist(testDat, method=metric)

library(ipred)
		tmp <- slda(y~., data=trainDat, na.action=na.action, ...)
		out <- predict(tmp, newdata=testDat)
                new("classifOutput", method="slda",
                        predLabels=newPredClass(as.character(out$class)),
                        predScores=newProbMat(out$posterior),
                        RObject=tmp, call=match.call(), distMat=dis)
})

#####################
# title: inbaggB
# description: interface to inbagg {ipred} 
# arguments:
#	exprObj		exprSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	trainInd	vector of indices for the columns to be 
#			included in the training set
# 	intLab 		character string or vector of character strings 
#			representing names of the intermediate variables
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# a <- inbaggB(golubMerge[100:110,], "ALL.AML", train, "PS")
#####################

setGeneric("inbaggB", function(exprObj, classifLab, trainInd, intLab, pFUN, cFUN, metric="euclidean", ...){
		standardGeneric("inbaggB")
})

setMethod("inbaggB", c("exprSet", "character", "integer", "character", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, trainInd, intLab, pFUN, cFUN, metric, ...){

		if(missing(pFUN)){ pFUN <- list(list(model=lm)) }
		if(missing(cFUN)){ cFUN <- NULL }

		cl <- exprObj[[classifLab]]
		
		if(length(intLab) > 1){ 
			intDat <- exprObj[[intLab[1]]]
			for( v in intLab[-1] ){
				intDat <- cbind(intDat, exprObj[[v]])
			}

			intNam <- paste("i", ".", 1:length(intLab), sep="")
			colnames(intDat) <- intNam
			trainDat <- data.frame(intVar=intDat[trainInd,], y=cl[trainInd], t(exprs(exprObj)[,trainInd]))
			testDat <- data.frame(intVar=intDat[-trainInd,], y=cl[-trainInd], t(exprs(exprObj)[,-trainInd]))

			intEqu <- paste(colnames(trainDat)[1:length(intLab)], collapse="+")
			equ <- paste("y", intEqu, ".", sep="~")
			equ <- as.formula(equ)
		}

		else{
			intDat <- exprObj[[intLab]]
			trainDat <- data.frame(intVar=intDat[trainInd], y=cl[trainInd], t(exprs(exprObj)[,trainInd]))
			testDat <- data.frame(intVar=intDat[-trainInd], y=cl[-trainInd], t(exprs(exprObj)[,-trainInd]))
			
			equ <- as.formula(y ~ intVar ~ .)
		}

		dis <- dist(t(exprs(exprObj)[,-trainInd]), method=metric)
library(ipred)
		tmp <- inbagg(equ, pFun=pFun, cFun=cFun, data=trainDat,  ...)
		out <- predict(tmp, newdata=testDat)
                new("classifOutput", method="slda",
                        predLabels=newPredClass(as.character(out)),
#                        predScores=newProbMat(out$posterior),
                        RObject=tmp, call=match.call(), distMat=dis)
})

#####################
# title: inclassB
# description: interface to inclass {ipred} 
# arguments:
#	exprObj		exprSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	trainInd	
# 	intLab 		single label or a vector of labels
#			representing names of the factors for intermediate variables
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
# note:
# 	cFUN argument requires a function object which assigns
#	the response labels using the intermediate variable(s).
#	This function should have a single input argument "newdata".
#	Internally inclass applies this cFUN function to new data 
#	that is stored as a data frame in such a way that the 
#	intermediate variables occupy the last columns of the data
#	frame.
#	The example below uses a single intermediate variable ("PS" 
#	values) and samples with PS values > 0.75 are
#	classified as ALL (this is all made up, it's intended to illustrate
#	the function usage), otherwise AML. 
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# psclassify <- function(newdata){
#		xx <- ifelse((newdata[,ncol(newdata)] > 0.75), "ALL", "AML")
#		as.factor(xx)
# }
# a <- inclassB(golubMerge[100:110,], "ALL.AML", train, "PS", cFUN=psclassify)
#####################

setGeneric("inclassB", function(exprObj, classifLab, trainInd, intLab, pFUN, cFUN, metric="euclidean", ...){
		standardGeneric("inclassB")
}) 

setMethod("inclassB", c("exprSet", "character", "integer", "character", "ANY", "ANY", "ANY", "ANY"), 
			function(exprObj, classifLab, trainInd, intLab, pFUN, cFUN, metric, ...){

		if(missing(pFUN)){ pFUN <- list(list(model=lm)) }
	
		cl <- exprObj[[classifLab]]
	
		if(length(intLab) > 1){ 

			intDat <- exprObj[[intLab[1]]]
			for( v in intLab[-1] ){
				intDat <- cbind(intDat, exprObj[[v]])
			}

			intNam <- paste("i", ".", 1:length(intLab), sep="")
			colnames(intDat) <- intNam

			trainDat <- data.frame(intVar=intDat[trainInd,], y=cl[trainInd], t(exprs(exprObj)[,trainInd]))
			testDat <- data.frame(intVar=intDat[-trainInd,], y=cl[-trainInd], t(exprs(exprObj)[,-trainInd]))
	
			intEqu <- paste(colnames(trainDat)[1:length(intLab)], collapse="+")
			equ <- paste("y", intEqu, ".", sep="~")
			equ <- as.formula(equ)
		}

		else{
			intDat <- exprObj[[intLab]]
			trainDat <- data.frame(intVar=intDat[trainInd], y=cl[trainInd], t(exprs(exprObj)[,trainInd]))
			testDat <- data.frame(intVar=intDat[-trainInd], y=cl[-trainInd], t(exprs(exprObj)[,-trainInd]))
			equ <- as.formula(y ~ intVar ~ .)
		}

		dis <- dist(t(exprs(exprObj)[,-trainInd]), method=metric)
		out <- ipred::inclass(equ, pFUN=pFUN, cFUN=cFUN, data=trainDat, ...)
		new("classifPred", sampLabels=predict(out, testDat, ...), distMat=dis, classifObj=out)
})		
