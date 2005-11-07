setGeneric("xval", function(data, classLab, proc, xvalMethod, group, indFun, niter, fsFun=NULL, fsNum=NULL,
 decreasing=TRUE, ...)
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

setMethod("xval", c("exprSet", "character", "genericFunction", "character", "integer", "ANY", "ANY", "ANY",
  "ANY", "ANY", "ANY" ),
function(data, classLab, proc, xvalMethod="LOO", group, indFun, niter, fsFun=NULL, fsNum=10, decreasing=TRUE, ...) 
{
#
# some mods by Stephen Henderson to support feature selection
#
	if (!(xvalMethod %in% c("LOO", "LOG", "FUN"))) 
		stop("unrecognised xvalMethod")
	if(!any(classLab == names(pData(data))))
		stop("unrecognised classLab")

	if (chkMLInterfaceProc(proc)) 
		X <- t(exprs(data))
	N <- nrow(X)
    	inds <- 1:N
	fs.inds<-1:ncol(X)
	fs.memory<-vector()
	
    	if (xvalMethod == "LOO") 
	{
       	out <- rep(NA, N)
        	for (i in 1:N)
		{
			if (is.function(fsFun))
			{
				fs.scores<-fsFun(data[,-i], classLab)
				fs.inds<-sort(fs.scores, index.return=TRUE, decreasing=decreasing)$ix[1:fsNum]
	 			fs.memory<-c(fs.memory, fs.inds)
			}
			out[i] <- proc(data[fs.inds,], classLab, inds[-i], ...)@predLabels@.Data
        	}
		
    	}
    	else if (xvalMethod == "LOG")
	{
        	ug <- unique(group)
        	Nu <- length(ug)
        	out <- NULL
        	for (i in 1:Nu)
		{			
			if (is.function(fsFun))
			{
				fs.scores<-fsFun(data[,group !=ug[i]], classLab)
				fs.inds<-sort(fs.scores, index.return=TRUE, decreasing=decreasing)$ix[1:fsNum]
	 			fs.memory<-c(fs.memory, fs.inds)
			}
			out <- c(out, proc(data[fs.inds,], classLab, inds[group != ug[i]], ...)@predLabels@.Data)
        	}
#		if (is.function(fsFun)) return(list(fs.memory=fs.memory, out=out))
#		else return(out)
    	}
    	else if (xvalMethod == "FUN")
	{
        	out <- NULL
        	for (i in 1:niter) 
		{
			tinds <- indFun(data, classLab, i)
			if (is.function(fsFun))
			{
				fs.scores<-fsFun(data[,tinds], classLab)
				fs.inds<-sort(fs.scores, index.return=TRUE, decreasing=decreasing)$ix[1:fsNum]
	 			fs.memory<-c(fs.memory, fs.inds)
			}
			
            	out <- c(out, proc(data[fs.inds,], classLab, tinds, ...)@predLabels@.Data)
      		}
	}
	if (is.function(fsFun))
	{
		return(list(fs.memory=fs.memory, out=out))
	}
	else
	{
		return(out)
	}
})

setMethod("xval", c("exprSet", "character", "genericFunction", "character", "missing", "ANY", "ANY", "ANY",
  "ANY", "ANY", "ANY" ),
function(data, classLab, proc, xvalMethod="LOO", group=0:0, indFun, niter, fsFun=NULL, fsNum=10, decreasing=TRUE, ...) 
{
    xval(data, classLab, proc, xvalMethod="LOO", group=0:0)
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

