#####################
# title: cvB
# description: leave one out cross-validation classification
# arguments:
#	exprObj		exprSet
#	classifLab	character string specifying what covariate data 
#			to use for classification
#	algofunc	function object that calls the classification algorithm 
#			(can be created by makeCVFunc)
# value:
# 	object of class "classifCV"
# example:
# eset <- golubMerge[100:110,] 
# cvtknn <- cvB(eset, "ALL.AML", makeCVFunc("knn", k=10) )
# cvtnb <- cvB(eset, "ALL.AML", makeCVFunc("naiveBayes") )
####################

setGeneric("cvB", function(exprObj, classifLab, algofunc, metric="euclidean", ...){
	standardGeneric("cvB")
})

setMethod("cvB", c("exprSet", "character", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, algofunc, metric, ...){

	cl <- exprObj[[classifLab]]
	dat <- exprs(exprObj)
	dis <- dist(t(dat), method=metric)
	n <- ncol(dat)
	predCV <- cl

	for( i in 1:n ){
		predCV[i] <- algofunc(t(dat[,-i]), t(dat[,i]), cl[-i], ...)
	}

	tab <- table(predCV, cl)
	diag(tab) <- 0
	err <- paste(round(100*sum(tab)/length(cl),2), "%", sep="")
#	new("classifCV", err=err, sampLabels=predCV, distMat=dis)
                #out <- class::knn(trainDat, testDat, cl, k, l, prob, use.all)
                new("classifOutput", method="knn",
                        predLabels=newPredClass(as.character(predCV)),
#                        predScores=newQualScore(attr(out,"prob")),
                        RObject=err, call=match.call(), distMat=dis)
                                                                                

})

#####################
# title: makeCVFunc
# description: creates function object for input to cvB
# arguments:
#	algorithm	character string specifying the name of the 
#			classification algorithm to be used
#			must be either: 
#				knn, knn1, knn.cv, lvq1, lvq2, lvq3,
#				olvq1, naiveBayes, svm, lda,
#				qda, bagging, ipredknn,
#				slda, randomForest, rpart, nnet, pamr
#			(i.e. all classification algorithms in machLI
#			except for knn.cv {class}, inbagg {ipred}, 
#			inclass {ipred})
#	...		additional parameters specific to the 
#			classification algorithm
# value:
# 	function object
# example:
# train <- c(sample(1:47, 23), sample(48:72, 12))
# aa <- pamrB(golubMerge[100:200,], train, "ALL.AML")
####################

makeCVFunc <- function(algorithm, ...){

	if( algorithm == "knn" ){		
		resfunc <- function(train, test, lab){
				class::knn(train, test, lab, ...)
		}	
	}

	if( algorithm == "knn1" ){		
		resfunc <- function(train, test, lab){
				class::knn1(train, test, lab)
		}	
	}
			
	if( algorithm == "lvq1" ){		
		resfunc <- function(train, test, lab){
				initbk <- class::lvqinit(train, lab, ...)
				trbk <- class::lvq1(train, lab, initbk, ...)
				class::lvqtest(trbk, test)
		}
	}

	if( algorithm == "lvq2" ){		
		resfunc <- function(train, test, lab){
				initbk <- class::lvqinit(train, lab, ...)
				trbk <- class::lvq2(train, lab, initbk, ...)
				class::lvqtest(trbk, test)
		}
	}

	if( algorithm == "lvq3" ){		
		resfunc <- function(train, test, lab){
				initbk <- class::lvqinit(train, lab, ...)
				trbk <- class::lvq3(train, lab, initbk, ...)
				class::lvqtest(trbk, test)
		}
	}

	if( algorithm == "olvq1" ){
		
		resfunc <- function(train, test, lab){
				initbk <- class::lvqinit(train, lab, ...)
				trbk <- class::olvq1(train, lab, initbk, ...)
				class::lvqtest(trbk, test)
		}
	}	

	if( algorithm == "naiveBayes" ){

		resfunc <- function(train, test, lab){
				df <- data.frame(y=lab, train)
				classifObj <- e1071::naiveBayes(y~., data=df, ...)
				predict(classifObj, test, ...)
		}	
	}

	if( algorithm == "svm" ){
		resfunc <- function(train, test, lab){
				classifObj <- e1071::svm(train, lab, ...)
				predict(classifObj, test, ...)
		}	
	}

	if( algorithm == "lda" ){
		resfunc <- function(train, test, lab){
				classifObj <- MASS::lda(train, grouping=lab, ...)
				predict(classifObj, test, ...)$class
		}
	}


	if( algorithm == "qda" ){
		resfunc <- function(train, test, lab){
				classifObj <- MASS::qda(train, grouping=lab, ...)
				predict(classifObj, test, ...)$class
		}
	}

	if( algorithm == "bagging" ){
		resfunc <- function(train, test, lab){
				df <- data.frame(y=lab, train)
				classifObj <- ipred::bagging(y~., data=df, ...)
				predict(classifObj, data.frame(test), type="class", ...)
		}	
	}

	if( algorithm == "ipredknn" ){

		resfunc <- function(train, test, lab){
				df <- data.frame(y=lab, train)
				classifObj <- ipred::ipredknn(y~., data=df, ...)
				ipred::predict.ipredknn(classifObj, data.frame(test), type="class", ...)
		}	
	}

	if( algorithm == "slda" ){

		resfunc <- function(train, test, lab){
				df <- data.frame(y=lab, train)
				classifObj <- ipred::slda(y~., data=df, ...)
				predict(classifObj, data.frame(test), ...)$class
		}	
	}

	if( algorithm == "randomForest" ){
		resfunc <- function(train, test, lab){
				classifObj <- randomForest::randomForest(train, y=lab, ...)
				predict(classifObj, test, ...)
		}	
	}

	if( algorithm == "rpart" ){
		resfunc <- function(train, test, lab){
				df <- data.frame(train, y=lab)
				classifObj <- rpart::rpart(y~., data=df, ...)
				predict(classifObj, data.frame(test), type="class")
		}
	}

	if( algorithm == "nnet" ){
		resfunc <- function(train, test, lab){
				df <- data.frame(train, y=lab)
				classifObj <- nnet::nnet(y~., data=df, ...)
				predict(classifObj, data.frame(test), type="class", ...)
		}	
	}

	if( algorithm == "pamr" ){
		resfunc <- function(train, test, lab, threshold=1){
				df <- list(x=t(train), y=lab)
				classifObj <- pamr::pamr.train(df, ...)		
				pamr::pamr.predict(classifObj, matrix(test, ncol=1), threshold, ...)
		}	
	}
	resfunc
}

