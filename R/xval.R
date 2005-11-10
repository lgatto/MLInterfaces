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
          function(data, classLab, proc, xvalMethod="LOO", group, indFun, niter, fsFun=NULL, fsNum=10, decreasing=TRUE, ...) {

          if (!(xvalMethod %in% c("LOO", "LOG", "FUN"))) 
              stop("unrecognised xvalMethod")
          if(!any(classLab == names(pData(data))))
              stop("unrecognised classLab")

          if (chkMLInterfaceProc(proc))
              X <- t(exprs(data))
          N <- nrow(X)
          inds <- 1:N
          fs.inds <- 1:ncol(X)
          fs.memory <- vector()
          
          if (xvalMethod == "LOO")
          {
              n <- N
              selnProc <- function(i) -i
          }
          else if (xvalMethod == "LOG")
          {
              ug <- unique(group)
              n <- length(ug)
              selnProc <- function(i) group != ug[i]
          }
          else                     # default: if (xvalMethod == "FUN")
          {
              n <- niter
              selnProc <- function(i) indFun( data, classLab, i )
          }
          
          xvalidator <- function(i) {
              ## ith cross-validation
              idx <- selnProc(i)
              if (is.function(fsFun))
              { ## by Stephen Henderson, to support feature selection
                  fs.scores <- fsFun(data[,idx], classLab)
                  fs.inds <- sort(fs.scores, index.return=TRUE, decreasing=decreasing)$ix[1:fsNum]
                  fs.memory <<- c(fs.memory, fs.inds) # intentional side-effect
              }
              proc( data[fs.inds,], classLab, inds[idx], ... )@predLabels@.Data
          }

          out <- unlist( lapply( 1:n, xvalidator ) )

          if (is.function(fsFun))
              return(list(fs.memory=fs.memory, out=out))
          else
              return(out)
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

