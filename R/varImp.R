
setClass("varImpStruct", representation(method="character",
 varnames="character"), contains="matrix")

if(!isGeneric("getVarImp"))setGeneric("getVarImp",
 function(object,fixNames)standardGeneric("getVarImp"))

#setMethod("getVarImp", c("classifOutput","logical"), function(object,fixNames) {
## watch out, people are using compound S3 classes c("randomForest.formula", "randomForest")
#	if (any(class(object@RObject) == "randomForest")) {
#		imp <- object@RObject$importance
#		return(new("varImpStruct", data.matrix(imp[,-1]), 
#			method="randomForest", varnames= row.names(imp)))
#		}
#	else if (any(class(object@RObject) == "gbm")) {
#		imp <- summary(object@RObject, plotit=FALSE)
#		return(new("varImpStruct", data.matrix(imp[,-1]), 
#                         method="gbm", varnames=as.character(imp[,1])))
#	        }
#	else stop("getVarImp defined only for randomForestB or gbmB output")
#})

setMethod("getVarImp", c("classifierOutput","missing"), function(object,fixNames) {
   getVarImp(object, TRUE)
})

setMethod("getVarImp", c("classifierOutput","logical"), function(object,fixNames) {
# watch out, people are using compound S3 classes c("randomForest.formula", "randomForest")
 	fixupNames = function(x) gsub("\\.", "-", gsub("^X", "", x))
	if (any(class(object@RObject) == "randomForest")) {
		imp <- object@RObject$importance
		dm = data.matrix(imp)
		if (fixNames) rownames(dm) = fixupNames(row.names(imp))
		return(new("varImpStruct", dm,
			method="randomForest", varnames= rownames(dm)))
		}
	else stop("getVarImp defined only for randomForestI-based classifierOutput")
})

setMethod("show", "varImpStruct", function(object) {
 cat("varImpStruct for method", object@method, "\n")
 cat("first five varnames:\n")
 print(object@varnames[1:5])
 cat("dims importance:\n")
 print(dim(object@.Data))
})


mapPSvec = function (vn, plat, toktype) 
{
        ans = unlist(lookUp(vn, plat, toktype))
        if (any(is.na(ans)))  # just pass psid if no mapping
            ans[is.na(ans)] = names(ans)[is.na(ans)]
        return(ans)
}


setMethod("plot", "varImpStruct", function(x, y, ..., n=20, plat, toktype) {
        vn <- x@varnames

# but the key thing is to get ifnotfound right below, once you have done these steps
	if (!missing(plat)) vn = mapPSvec(vn, plat, toktype)
	if (x@method=="randomForest") 
		{
		mda <- x@.Data[,"MeanDecreaseAccuracy"]
                omda <- order(-mda)[n:1]
		barplot(mda[omda], names=vn[omda], horiz=TRUE,
			xlab="Mean decrease in accuracy", ...)
		} })
           
setGeneric("report", function(x, n=10, plat, toktype) standardGeneric("report"))

setMethod("report", "varImpStruct", function(x, n=10, plat, toktype) {
        vn <- x@varnames

# but the key thing is to get ifnotfound right below, once you have done these steps
        if (!missing(plat))
		{
		vn = mapPSvec(vn, plat, toktype)
		}
	if (x@method=="gbm") 
		{
		return(data.frame(imp=x@.Data[n:1], names=vn[n:1]))
		}
	else if (x@method=="randomForest") 
		{
		mda <- x@.Data[,"MeanDecreaseAccuracy"]
                omda <- order(-mda)[1:n]
		return(data.frame(MnDecrAcc=mda[omda], names=vn[omda]))
		} })
