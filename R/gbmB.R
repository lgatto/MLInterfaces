
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
#			predScores=newQualScore(attr(out,"prob")),
                        RObject=out, call=match.call(), distMat=dis)
})

