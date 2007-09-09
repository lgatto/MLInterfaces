# this is the original 2006 version of MLearn ... to be superseded by
# the schema-based version defined in MLearn.R, probably by bioc 2.2

setGeneric("MLearn", function(formula, data, method, trainInd, mlSpecials, ...)standardGeneric("MLearn"))

#setMethod("MLearn", c("character", "ExpressionSet", "character", "numeric"),
#  function(formula, data, method, trainInd, ...) {
#	switch( method ,
#		knn = knnB(data, formula, as.integer(trainInd), ...),
#		nnet = nnetB( data, formula, as.integer(trainInd), ...),
#		rpart = rpartB( data, formula, as.integer(trainInd), ...),
#		randomForest = randomForestB( data, formula, as.integer(trainInd), ...),
#		gbm = gbmB( data, formula, as.integer(trainInd), ...)
#	      )
#  })
			
es2df = function(x,keep=NULL) {
#
# the keep parameter says which pData vars are kept in
#
   if (is.null(keep)) return(data.frame(t(exprs(x)),pData(x)))
   else {
        tmp = data.frame(t(exprs(x)),pData(x)[[keep]])
	names(tmp)[ncol(tmp)] = keep
        return(tmp)
        }
}

setMethod("MLearn", c("formula", "ExpressionSet", "character", "numeric",
   "ANY"),
  function(formula, data, method, trainInd, mlSpecials=NULL, ...) {
#
# the keep setting below says just keep the response variable
# from pData
#
        data = es2df(data, keep=as.character(as.list(formula)[[2]]))
        MLearn( formula, data, method, trainInd, mlSpecials, ... )
  })

setMethod("MLearn", c("formula", "ExpressionSet", "character", "numeric",
   "missing"),
  function(formula, data, method, trainInd, mlSpecials=NULL, ...) {
#
# the keep setting below says just keep the response variable
# from pData
#
        data = es2df(data, keep=as.character(as.list(formula)[[2]]))
        MLearn( formula, data, method, trainInd, mlSpecials=NULL, ... )
  })
    
setMethod("MLearn", c("formula", "data.frame", "character", "numeric",
   "ANY" ),
  function(formula, data, method, trainInd, mlSpecials, ...) {
#
# extending MLInterfaces to work with formulas
#
# create training data (sdata) and test data (tdata) using trainInd
#
	sdata <- data[trainInd,]
	tdata <- data[-trainInd,]
        allClass <- data[[ as.character(as.list(formula)[[2]]) ]] # model.response(model.frame(formula,data))
#
# to add a new method, add a switch element with tag the method name,
#   and return a list with components rob (R object output by method),
#   pred (MLLabel instance of predictions on the test data) and
#   [optionally] pScores (MLScore instance of quality scores for predictions)
#
	dometh <- switch( method ,
		knn = { sdata <- model.frame(formula,data=sdata)[,-1] # drop intercept
				tdata <- model.frame(formula,data=tdata)[,-1]
				list( 
#rob = ROB <- class::knn( train=sdata, test=tdata, cl = allClass[trainInd], ...),
rob = ROB <- knnP(sdata, tdata, allClass[trainInd], ...),  # should keep training classes?
				pred = newPredClass(as.character(OUT <- knnP(sdata, tdata, allClass[trainInd], ...))),
				predTr = newPredClass(as.character(OUT <- knnP(sdata, sdata, allClass[trainInd], ...))),
				pScores = newQualScore(attr(OUT,"prob"))
                        	) },
		dlda = { require(sma) # no namespace and no formula interface
			        sdata <- data.matrix(model.frame(formula,data=sdata)[,-1]) # drop intercept
				tdata <- data.matrix(model.frame(formula,data=tdata)[,-1])
				ncl = as.numeric(allClass[trainInd])
				if (is(allClass,"factor")) lcl = levels(allClass)
				else lcl = sort(unique(allClass))
				tmp = stat.diag.da( sdata, as.numeric(allClass[trainInd]), tdata, ... )
				tmptr = stat.diag.da( sdata, as.numeric(allClass[trainInd]), sdata, ... )
				list( rob = ROB <- tmp ,
					pred = newPredClass(as.character(lcl[tmp$pred])),
					predTr = newPredClass(as.character(lcl[tmptr$pred])) 
				) },
		lda = { list( rob = ROB <- MASS::lda( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict( ROB, tdata)$class)),
				predTr = newPredClass(as.character(predict( ROB, sdata)$class))
                        	) },
		qda = { list( rob = ROB <- MASS::qda( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict( ROB, tdata)$class)),
				predTr = newPredClass(as.character(predict( ROB, sdata)$class))
                        	) },
		nnet = { list( rob = ROB <- nnet::nnet( formula =formula, data=sdata, ...),
				pred = newPredClass(predict( ROB, tdata, type="class")),
				predTr = newPredClass(predict( ROB, sdata, type="class")),
				pScores = newProbMat(predict(ROB, newdata=tdata))
                        	) },
		logistic = { if (missing("mlSpecials") || is.null(mlSpecials$thresh)) 
                                stop("logistic learning requires thresh parm passed\nas element in mlSpecials list")
				list( rob = ROB <- glm( formula = formula, data=sdata, fam=binomial ),
				pred = newPredClass( predict( ROB, tdata, type="response") > mlSpecials$thresh ),
				predTr = newPredClass( predict( ROB, sdata, type="response") > mlSpecials$thresh ),
				pScores = newProbMat(matrix(predict(ROB, newdata=tdata, type="response"),,1))
				) },
		randomForest = { list( rob = ROB <- randomForest::randomForest( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict(ROB, tdata))),
				predTr = newPredClass(as.character(predict(ROB, sdata)))
				) },
		rpart = { list( rob = ROB <- rpart::rpart( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict(ROB, tdata, type="class"))),
				predTr = newPredClass(as.character(predict(ROB, sdata, type="class"))),
				pScores = newQualScore(attr(ROB,"prob"))
				) },
		svm = { list( rob = ROB <- e1071::svm( formula =formula, data=sdata, ...),
				pred = newPredClass(as.character(predict(ROB, tdata, decision.values=FALSE))),
				predTr = newPredClass(as.character(predict(ROB, sdata, decision.values=FALSE)))
				) },

		RAB = { 
## caller must supply mlSpecials$isOne = [level of response that codes to 1 vs -1]
			respn = as.character(as.list(formula)[[2]])
#          emsg1 = "a parameter named isOne must be supplied"
#          emsg2 = "as an element of mlSpecials list."
#	  emsg3 = "isOne must be bound to a string naming a variable"
#	  emsg4 = "that will be mapped to 1 (as opposed to -1)"
#          emsg = paste(emsg1, emsg2, emsg3, emsg4, collapse="\n")
#			if (missing(mlSpecials)) stop(emsg)
#			if (length(mlSpecials$isOne) == 0) stop(emsg)
#			if (!(mlSpecials$isOne %in% data[[respn]] )) stop(emsg)
			nn = names(sdata)
# exclude the response variable from the RHS of formula
			nn = nn[ which(nn != respn) ]
    			formula = mkfmla(respn, nn)
#    			tmp = tonp(sdata[[respn]], mlSpecials$isOne)
#    			ttmp = tonp(tdata[[respn]], mlSpecials$isOne)
# create the -1,1 coding
			resp = sdata[[respn]]
			if (is.factor(resp))  targ = max(levels(resp))
			else targ = max(resp)
    			tmp = tonp(sdata[[respn]], targ)
    			ttmp = tonp(tdata[[respn]], targ)
			sdata = sdata[, -which(names(sdata) == respn) ]
			tdata = tdata[, -which(names(tdata) == respn) ]
    			sdata = data.frame(sdata, tmp)
    			tdata = data.frame(tdata, ttmp)
    			names(sdata)[ncol(sdata)] = respn
    			names(tdata)[ncol(tdata)] = respn
			list( rob = ROB <- RAB( formula=formula, data=sdata, ...),
                                pred = newPredClass(as.character(Predict(ROB, newdata=tdata))),
                                predTr = newPredClass(as.character(Predict(ROB, newdata=sdata)))
                                ) },

		rdacv = {   # the API for rda involves x (GxN), y (Nx1 1-based class index), xnew, ynew 
                            # we will use its CV interface to get approximately optimal alpha and delta
                            # for the data specified by trainInds
			 rdaOpt = function(...) {
			 	x1 = rda(...)
                                dots = list(...)
                                dots = dots[ - which(names(dots) %in% c("xnew", "ynew", "genelist")) ]
				nargl = list(fit=x1)
				dn = names(dots)
				for (j in 1:length(dots)) 
					nargl[[dn[j]]] = dots[[j]]
			 	x = do.call("rda.cv", nargl)
			 	minerr <- min(x$cv.err)/x$n
			 	one.se <- ceiling((sqrt(minerr * (1 - minerr)/x$n) *
					1.645 + minerr) * x$n)
                         pos = NULL
		         while( length(pos) < 1) {
			 	pos <- which(x$cv.err == one.se, TRUE)
				if (length(pos)<1) print("could not relax one full S.E., diminish by 1")
				one.se = one.se-1
                         }
#if (length(pos) < 1) {
#  print(pos)
#  print(x$cv.err)
#  print(one.se)
#stop("can't pick alpha/delta")
#}
				if (is.matrix(pos)) pos=pos[1,]
			 	minpos <- which(x$cv.err == min(x$cv.err), TRUE)
 			 	opt = list(separms=c(alpha=x$alpha[pos[1]], delta=x$delta[pos[2]]), inds.se=pos)
			        postop = rda(..., alpha=opt$separms["alpha"], delta=opt$separms["delta"])
				opt[["base"]] = postop
				opt
				}
                           #set up the data
		         X <- t(data.matrix(model.frame(formula,data=sdata)[,-1])) # drop intercept
			 NEWX <- t(data.matrix(model.frame(formula,data=tdata)[,-1]))
                         Y = as.numeric(factor(allClass))[trainInd]
                         NEWY = as.numeric(factor(allClass))[-trainInd]
                           # choose alpha and delta, and get a fit
                         opt = rdaOpt( x=X, y=Y, xnew=NEWX, ynew=NEWY, genelist=TRUE, ...)
print("Using CV-optimal parameters relaxed by one SE")
print(opt$separms)
                         tmp = predict( opt$base, x=X, y=Y, xnew=NEWX, alpha=opt$separms["alpha"], delta=opt$separms["delta"] )
                         trtmp = predict( opt$base, x=X, y=Y, xnew=X, alpha=opt$separms["alpha"], delta=opt$separms["delta"] )
                         preds = levels(allClass)[tmp]
                         trpreds = levels(allClass)[trtmp]
                         list(rob = ROB <- opt$base, pred=newPredClass(preds), predTr=newPredClass(trpreds)
                          ) }
       
	      	) # end  switch
	if (is.null( dometh$pScores )) dometh$pScores <- new("probMat") # placeholder
   	new("classifOutput", method=method, predLabels=dometh$pred, trainInds=trainInd,
		allClass=as.character(allClass), predLabelsTr=dometh$predTr,
		predScores=dometh$pScores, call=match.call(),
		RObject=dometh$rob)
  })

tellMLearnMethods = function() {
  .Deprecated("MLearn")  # only use the schema-based MLearn in future
  tmp = body(getMethod("MLearn", c("formula", "data.frame", "character", "numeric", "ANY"))@.Data)
  sw = as.list(as.list(tmp)[[5]])[[3]]
  names(sw)[-c(1,2)]
}


