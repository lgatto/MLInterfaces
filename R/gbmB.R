
setGeneric("gbmB", function(exprObj, classifLab, trainInd, 
 distribution="bernoulli", weights=NULL, 
 offset=NULL, var.monotone=NULL, 
 n.trees=100, interaction.depth=1, 
 n.minobsinnode=10, shrinkage=0.001, 
 bag.fraction=0.5, train.fraction=1, keep.data=TRUE, verbose=TRUE, metric="euclidean")
 standardGeneric("gbmB"))

setMethod("gbmB", c("exprSet", "character", "integer", 
			"ANY", "ANY", "ANY", "ANY", "ANY", 
			"ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
 function(exprObj, classifLab, trainInd, 
  distribution="bernoulli", weights=NULL, 
  offset=NULL, var.monotone=NULL, 
  n.trees=100, interaction.depth=1, 
  n.minobsinnode=10, shrinkage=0.001, 
  bag.fraction=0.5, train.fraction=1, keep.data=TRUE, verbose=FALSE,
  metric = "euclidean") {
require(gbm) # no namespace
		cl <- exprObj[[classifLab]][trainInd]				
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
		if (is.null(weights)) weights <- rep(1, length(cl))
		out <- gbm.fit(x=trainDat, y=as.numeric(cl)-1,
		  distribution=distribution, w=weights, 
		  offset=offset, var.monotone=var.monotone, 
		  n.trees=n.trees, interaction.depth=interaction.depth, 
		  n.minobsinnode=n.minobsinnode, shrinkage=shrinkage, 
		  bag.fraction=bag.fraction, 
		train.fraction=train.fraction, keep.data=keep.data, 
		verbose=verbose)
                ps <- predict( out, newdata= testDat, n.trees, type="response" )
                
                new("classifOutput", method="gbm", 
			predLabels=newPredClass(levels(cl)[(ps>0.5)+1]),
			trainInds=trainInd, allClass=as.character(exprObj[[classifLab]]),
#			predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
})

setGeneric("logitboostB",
 function(exprObj, classifLab, trainInd, mfinal=100, presel=0, estimate=0, verbose=FALSE, metric="euclidean")
 standardGeneric("logitboostB"))
setMethod("logitboostB", 
 c("exprSet", "character", "integer", "numeric", "ANY", "ANY", "ANY", "ANY"),
 function(exprObj, classifLab, trainInd, mfinal=100, presel=0, estimate=0, verbose=FALSE, metric="euclidean") {
require(LogitBoost)
#warning("version 1.1 of LogitBoost has bugs.  consult with authors of that package if you run into problems with this method.")
		cl <- exprObj[[classifLab]][trainInd]				
		trainDat <- t(exprs(exprObj)[,trainInd])
		testDat <- t(exprs(exprObj)[,-trainInd])
		dis <- dist(testDat, method=metric)
                out <- logitboost(trainDat, as.numeric(as.factor(cl))-1, 
			testDat, mfinal, presel=presel, 
			estimate=estimate, verbose=verbose)
                if (length(dim(out$probs))==3)
                  predcat <- apply(apply(out$probs,c(1,3),mean),1,which.max)
	 	else predcat <- 1*(apply(out$probs,1,mean)>.5)+1

                new("classifOutput", method="logitboost", 
			predLabels=newPredClass(levels(as.factor(cl))[predcat]),
			trainInds=trainInd, allClass=as.character(exprObj[[classifLab]]),
#			predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
})
