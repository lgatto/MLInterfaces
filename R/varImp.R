
setClass("varImpStruct", representation(method="character",
 varnames="character"), contains="matrix")

if(!isGeneric("getVarImp"))setGeneric("getVarImp",
 function(object)standardGeneric("getVarImp"))

setMethod("getVarImp", "classifOutput", function(object) {
# watch out, people are using compound S3 classes c("randomForest.formula", "randomForest")
	if (any(class(object@RObject) == "randomForest")) {
		imp <- object@RObject$importance
		return(new("varImpStruct", data.matrix(imp[,-1]), 
			method="randomForest", varnames= row.names(imp)))
		}
	else if (any(class(object@RObject) == "gbm")) {
		imp <- summary(object@RObject, plotit=FALSE)
		return(new("varImpStruct", data.matrix(imp[,-1]), 
                         method="gbm", varnames=as.character(imp[,1])))
	        }
	else stop("getVarImp defined only for randomForestB or gbmB output")
})

setMethod("show", "varImpStruct", function(object) {
 cat("varImpStruct for method", object@method, "\n")
 cat("first five varnames:\n")
 print(object@varnames[1:5])
 cat("dims importance:\n")
 print(dim(object@.Data))
})

setMethod("plot", "varImpStruct", function(x, y, ..., n=20, resolveenv=NULL) {
        vn <- x@varnames
	vn = gsub("^X", "", vn) # undo formula mangling -- very hokey
	vn = gsub("AFFX\\.", "AFFX-", vn) # undo formula mangling -- very hokey
# but the key thing is to get ifnotfound right below, once you have done these steps
        if (!is.null(resolveenv))
		{
		nn <- unlist(mget(vn, resolveenv, ifnotfound=list(function(x)x)))
		nn[is.na(nn)] <- vn[is.na(nn)]
		vn <- nn
		}
	if (x@method=="gbm") 
		{
		barplot(x@.Data[n:1], names=vn[n:1], horiz=TRUE,
			xlab="Relative importance")
		}
	else if (x@method=="randomForest") 
		{
		mda <- x@.Data[,"MeanDecreaseAccuracy"]
                omda <- order(-mda)[n:1]
		barplot(mda[omda], names=vn[omda], horiz=TRUE,
			xlab="Mean decrease in accuracy")
		} })
           
