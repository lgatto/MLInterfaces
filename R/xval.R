setGeneric("xval", function(data, classLab, proc, xvalMethod, strata, ...)
 standardGeneric("xval"))

chkMLInterfaceProc <- function(x) {
 if (!is(x, "genericFunction")) stop("requires generic function [not name] as proc")
 xn <- x@generic
#
# the following gets the 'defined' type signature for the generic
# [note that the definition of signature for a generic 
# by JMC is the list of formal parameters -- i need the signature
# for a method.
#
 tySig <- methods:::.methodSignatureMatrix(getMethod(xn, c("exprSet", "character", "integer")))[2,]
 if( x@package != "MLInterfaces" || !all(tySig == c("exprSet", "character",
	"integer"))) stop(paste(x, "not bearing proper signature"))
 else return(TRUE)
}

setMethod("xval", c("exprSet", "character", "nonstandardGeneric", "character", "integer", "ANY"),
	function(data, classLab, proc, xvalMethod="LOO", strata=0:0) {
		if (xvalMethod != "LOO") stop("only supporting LOO xval")
		if (chkMLInterfaceProc(proc))
		X <- t(exprs(data))
		inds <- 1:nrow(X)
		out <- rep(NA, nrow(X))
		for (i in 1:length(inds))
			out[i] <- proc( data, classLab, inds[-i], ... )@predLabels@.Data
		out
})
		
