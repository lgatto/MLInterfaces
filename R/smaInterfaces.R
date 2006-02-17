

setGeneric("stat.diag.daB", function(exprObj, classifLab, trainInd, 
 pool=1, metric="euclidean", ...) standardGeneric("stat.diag.daB") )

setMethod("stat.diag.daB", c("exprSet", "character", "integer", "ANY", "ANY"),
		function(exprObj, classifLab, trainInd, pool,  
                       metric, ...){
		
			require(sma) # nice if it had a namespace
			cl <- exprObj[[classifLab]][trainInd]

			trainDat <- t(exprs(exprObj)[,trainInd])
			testDat <- t(exprs(exprObj)[,-trainInd])

			dis <- dist(testDat, method=metric)
	
			out <- stat.diag.da(trainDat,
   as.integer(factor(cl)), testDat, pool=pool )

			new("classifOutput", method="stat.diag.da",
	predLabels=newPredClass(as.character(out$pred)), 
			trainInds=trainInd, allClass=as.character(exprObj[[classifLab]]),
        call=match.call(),
        distMat=dis, RObject=out)	
})		  
