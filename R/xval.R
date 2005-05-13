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

setMethod("xval", c("exprSet", "character", "genericFunction", "character", "integer", "ANY", "ANY", "ANY"),
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
		

balKfold <- function(K) function( data, clab, iternum ) {
 clabs <- data[[clab]]
 narr <- nrow(pData(data))
 cnames <- unique(clabs)
 ilist <- list()
 for (i in 1:length(cnames))
   ilist[[cnames[i]]] <- which( clabs == cnames[i] )
 clens <- lapply(ilist,length)
 nrep <- lapply(clens, function(x) ceiling(x/K))
 grpinds <- list()
 for (i in 1:length(nrep))
   grpinds[[i]] <- rep(1:K, nrep[[i]])[1:clens[[i]]]
 (1:narr)[ - which( unlist(grpinds)==iternum ) ]
}

setMethod("xval", c("exprSet", "character", "genericFunction", "character", "missing", "ANY", "ANY", "ANY"),
	function(data, classLab, proc, xvalMethod=c("LOO","LOG", "FUN")[1], group=0:0, indFun, niter, ...) 
		xval(data=data, classLab=classLab, proc=proc, 
			xvalMethod=xvalMethod, group=0:0, indFun=function(){},
			niter=0, ...))
