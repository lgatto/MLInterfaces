setGeneric("xval", function(data, classLab, proc, xvalMethod, group, indFun, niter, ...)
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

setMethod("xval", c("exprSet", "character", "nonstandardGeneric", "character", "integer", "ANY", "ANY", "ANY"),
	function(data, classLab, proc, xvalMethod=c("LOO","LOG", "FUN")[1], group=0:0, indFun, niter, ...) {
		if (!(xvalMethod %in% c("LOO","LOG","FUN"))) stop("unrecognized xvalMethod")
		if (chkMLInterfaceProc(proc))
		X <- t(exprs(data))
		N <- nrow(X)
		inds <- 1:N
                if (xvalMethod == "LOO")
                 {
		 out <- rep(NA, N)
		 for (i in 1:N)
			out[i] <- proc( data, classLab, inds[-i], ... )@predLabels@.Data
		 return(out)
                 }
		else if (xvalMethod == "LOG")
		 {
	 	 ug <- unique(group)
		 Nu <- length(ug)
		 out <- NULL
		 for (i in 1:Nu)
		    out <- c(out, proc(data, classLab, inds[group != ug[i]], ...)@predLabels@.Data)
		 return(out)
	         }
		else if (xvalMethod == "FUN")
                 {
		 out <- NULL
		 for (i in 1:niter)
			{
                        tinds <- indFun( data, classLab, i )
			out <- c(out, proc( data, classLab, tinds, ...)@predLabels@.Data)
			}
		 return(out)
		 }
})
		
