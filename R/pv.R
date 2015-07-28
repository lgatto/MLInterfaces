setOldClass("prcomp")

setClass("projectedLearner", 
   representation(
    fittedLearner="classifierOutput",
    trainingSetPCA="prcomp",
    trainingLabels="ANY",
    testLabels="ANY",
    gridFeatsProjectedToTrainingPCs = "matrix",
    gridPredictions="ANY",
    trainFeatsProjectedToTrainingPCs = "matrix",
    testFeatsProjectedToTrainingPCs = "matrix",
    trainPredictions="ANY",
    testPredictions="ANY",
    theCall="call")
)

setMethod("show", "projectedLearner", function(object) {
 cat("MLInterfaces projectedLearner instance.\n")
 cat("The call was:\n")
 print(object@theCall)
# cat("Fitted MLearn result:\n")
# show(object@fittedLearner)
 cat("dimensions of projected feature grid:\n")
 print(dim(object@gridFeatsProjectedToTrainingPCs))
})
projectLearnerToGrid = function(fmla, data, learnerSchema, trainInds, ...,
    dropIntercept=TRUE, ngpts=20, predExtras=list(), predWrapper=force) {
#
# 1) check trainInds, get nrow(data), compute testInds
# 2) all steps in 2 can be done in parallel
# 2a) m1 = trained learner
# 2b) mm = model.matrix of full data, with intercept removed by default
# 2c) prcomp.train = PCA reduction of scaled, centered training data
# 2d) fullresp = model.response of full data -> trainingLabels, testLabels
# 2e) build grid in feature space, assign feature names to columns
# 3) must follow 2 but can be parallel
# 3a1) project grid into PC space of 2c
# 3a2) project test features into PC space of 2c
# 3b) obtain trained model predictions (labels) for grid inputs
  stopifnot(is.numeric(trainInds))
  nd = nrow(data)
  testInds = setdiff(1:nd, trainInds)
# 2a:
  m1 = do.call(MLearn, c(list(fmla, data, learnerSchema, trainInds), list(...)))
# 2b:
  mm = model.matrix( fmla, data )
  if (dropIntercept) {
      ind = match("(Intercept)", colnames(mm))
      if (!is.na(ind)) mm = mm[,-ind]
      }
  trfeats = mm[trainInds,]
  tefeats = mm[-trainInds,]
# 2c:
  prcomp.train = prcomp( trfeats, scale=TRUE )
# 2d:
  fullresp = model.response(model.frame(fmla, data))
  trainingLabels = as.character(fullresp[trainInds])
  testLabels = as.character(fullresp[testInds])
# 2e: 
  gpts = lapply( 1:ncol(trfeats),
     function(x) seq(min(trfeats[,x]), max(trfeats[,x]), length=ngpts))
  grid = do.call( expand.grid, gpts )
  colnames(grid) = colnames(trfeats)
# 3a1:
  gridFeatsProjectedToTrainingPCs = predict(prcomp.train, data.frame(grid) ) # just coordinates FOR GRID after rotation
# 3a2:
  trainFeatsProjectedToTrainingPCs = predict(prcomp.train, trfeats ) # just coordinates FOR TRAINING DATA after rotation
  testFeatsProjectedToTrainingPCs = predict(prcomp.train, tefeats ) # just coordinates FOR TEST DATA after rotation
# 3b
  al = list(RObject(m1), newdata=grid)
  al = c(al, predExtras)
  al = c(al, ...)
  curpred = predWrapper(do.call(predict, al))
# object
  new("projectedLearner",
         fittedLearner=m1,
         trainingSetPCA=prcomp.train,
         trainingLabels=trainingLabels,
         testLabels=testLabels,
         gridFeatsProjectedToTrainingPCs = gridFeatsProjectedToTrainingPCs,
         testFeatsProjectedToTrainingPCs = testFeatsProjectedToTrainingPCs,
         trainFeatsProjectedToTrainingPCs = trainFeatsProjectedToTrainingPCs,
         trainPredictions = trainPredictions(m1),
         testPredictions = testPredictions(m1),
         gridPredictions = curpred, theCall=match.call())
}
 
setMethod("plot", "projectedLearner", function(x, y, ...) {
#    oldp = par(no.readonly=TRUE)
#    on.exit(par(oldp))
#    op = par(bg = adjustcolor("goldenrod", offset = -rep(.4, 4), alpha.f=.4), xpd = NA)
#
#    par(op)
    pairs(x@gridFeatsProjectedToTrainingPCs, 
           col=factor(x@gridPredictions), cex=1.5, pch=19)
    }
)

setGeneric("plotOne", function(x, ind1=1, ind2=2, type="showTestLabels")
   standardGeneric("plotOne"))
setMethod("plotOne", "projectedLearner", 
   function(x, ind1=1, ind2=2, type="showTestLabels") {
     proj = x@gridFeatsProjectedToTrainingPCs
     plot(proj[,ind1], proj[,ind2], col=factor(x@gridPredictions), cex=1.5,
           pch=19, xlab=paste0("PC", ind1), ylab=paste0("PC", ind2))
     switch( type,
        "showTestLabels" = {
           points(x@testFeatsProjectedToTrainingPCs[, ind1], 
                  x@testFeatsProjectedToTrainingPCs[, ind2], 
                     pch=as.character(x@testLabels), col="white")
           },
        "showTestPredictions" = {
           points(x@testFeatsProjectedToTrainingPCs[, ind1], 
                  x@testFeatsProjectedToTrainingPCs[, ind2], 
                     pch=as.character(x@testPredictions), col="white")
           },
        "showTrainLabels" = {
           points(x@trainFeatsProjectedToTrainingPCs[, ind1], 
                  x@trainFeatsProjectedToTrainingPCs[, ind2], 
                     pch=as.character(x@trainLabels), col="white")
           },
        "showTrainPredictions" = {
           points(x@trainFeatsProjectedToTrainingPCs[, ind1], 
                  x@trainFeatsProjectedToTrainingPCs[, ind2], 
                     pch=as.character(x@trainPredictions), col="white")
           } )
})


setGeneric("learnerIn3D", function(x, ind1=1, ind2=2, ind3=3) 
   standardGeneric("learnerIn3D"))
setMethod("learnerIn3D", "projectedLearner", 
   function(x, ind1=1, ind2=2, ind3=3)  {
     requireNamespace("rgl")
     proj = x@gridFeatsProjectedToTrainingPCs
     plot3d(proj[,ind1], proj[,ind2], proj[,ind3], 
          col=factor(x@gridPredictions), cex=1.5, pch=19,
          xlab=paste0("PC", ind1),
          ylab=paste0("PC", ind2),
          zlab=paste0("PC", ind3)
          )
     text3d(x@testFeatsProjectedToTrainingPCs[, ind1], 
                  x@testFeatsProjectedToTrainingPCs[, ind2], 
                  x@testFeatsProjectedToTrainingPCs[, ind3], 
                     texts=as.character(x@testLabels), col="black")
})

