setGeneric("xval", function(data, classLab, proc, xvalMethod, group, indFun, niter, fsFun=NULL, fsNum=NULL,
 decreasing=TRUE, cluster = NULL, ...)
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
 tySig <- methods::methodSignatureMatrix(getMethod(xn, c("ExpressionSet", "character", "integer")))[2,]
 if( x@package != "MLInterfaces" || !all(tySig == c("ExpressionSet", "character",
	"integer"))) stop(paste(x, "not bearing proper signature"))
 else return(TRUE)
}

setMethod("xval", c("ExpressionSet", "character", "genericFunction", "character", "integer", "ANY", "ANY", "ANY",
                    "ANY", "ANY", "ANY" ),
          function(data, classLab, proc, xvalMethod="LOO", group, indFun, niter, fsFun=NULL, fsNum=10, decreasing=TRUE, cluster = NULL,...) {

          if (!(xvalMethod %in% c("LOO", "LOG", "FUN"))) 
              stop("unrecognised xvalMethod")
          if(!any(classLab == names(pData(data))))
              stop("unrecognised classLab")
          xvalLoop <- xvalLoop(cluster)
	  N = ncol(exprs(data))
          inds <- 1:N

          ## cross-validation selection procedure
          if (xvalMethod == "LOO")
          {
              n <- length(inds)
              selnProc <- function(i) -i
          }
          else if (xvalMethod == "LOG")
          {
              ug <- unique(group)
              n <- length(ug)
              selnProc <- function(i) group != ug[i]
          }
          else                          # FUN
          {
              n <- niter
              selnProc <- function(i) indFun( data, classLab, i )
          }

          fs.idx <- 1:nrow(exprs(data))           # feature set index
          ## feature set selection
          if (missing(fsFun))
              fsProc <- function(idx, fs.idx) fs.idx
          else
          { ## original by Stephen Henderson, to support feature selection
              if (!is.function(fsFun)) stop("fsFun should be a function")
              fsProc <- function(idx, fs.idx) {
                  fs.scores <- fsFun(data[,idx], classLab)
                  sort(fs.scores, index.return=TRUE, decreasing=decreasing)$ix[1:fsNum]
              }
          }

          ## cross-validator
          xvalidator <- function(i, ...) {
              idx <- selnProc(i)
              fs.idx <- fsProc(idx, fs.idx)
              list( proc( data[fs.idx,], classLab, inds[idx], ...)@predLabels@.Data, fs.idx )
          }

          out <- xvalLoop( 1:n, xvalidator, ... )
          classif <- unlist( sapply( out, function(x) x[[1]] ) )

          if (!is.function(fsFun))
              return(classif)
          else {
              fs.memory <- as.vector( sapply( out, function(x) x[[2]] ) )
              return(list(fs.memory=fs.memory, out=classif))
         }
      })

setMethod("xval", c("ExpressionSet", "character", "genericFunction", "character", "missing", "ANY", "ANY", "ANY",
  "ANY", "ANY", "ANY" ),
function(data, classLab, proc, xvalMethod="LOO", group=0:0, indFun, niter, fsFun=NULL, fsNum=10, decreasing=TRUE, cluster = NULL, ...) 
{
    xval(data, classLab, proc, xvalMethod="LOO", group=0:0)
})


balKfold <- function(K) function( data, clab, iternum ) {
 clabs <- pData(data)[[clab]]
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

## xvalLoop is a 'hook' to customize how xval execute. The default is
## a simple lapply

setGeneric("xvalLoop", function( cluster, ... ) standardGeneric("xvalLoop") )

setMethod("xvalLoop", signature( cluster = "ANY" ), # default method -- return function 'lapply'
          function( cluster, ... ) lapply )

setGeneric("xvalML", 
          function(formula, data, proc, xvalMethod="LOO", 
            group, indFun, niter, fsFun=NULL, fsNum=10, 
            decreasing=TRUE, cluster = NULL,...) 
  { standardGeneric("xvalML") })

setMethod("xvalML", c("formula", "ExpressionSet", "character", 
                    "character", "numeric", "ANY", "ANY", "ANY",
                    "ANY", "ANY", "ANY" ),
          function(formula, data, proc, 
                     xvalMethod="LOO", group, 
                     indFun, niter, fsFun=NULL, fsNum=10, 
                     decreasing=TRUE, cluster = NULL,...) {

          if (!(xvalMethod %in% c("LOO", "LOG", "FUN"))) 
              stop("unrecognised xvalMethod")
          xvalLoop <- xvalLoop(cluster)
          
	  classLab = respn = as.character(as.list(formula)[[2]])
          if(!any(classLab == names(pData(data))))
              stop("unrecognised response variable")
          N <- ncol(exprs(data))
          inds <- 1:N

          ## cross-validation selection procedure
          if (xvalMethod == "LOO")
          {
              n <- length(inds)
              selnProc <- function(i) -i
          }
          else if (xvalMethod == "LOG")
          {
              ug <- unique(group)
              n <- length(ug)
              selnProc <- function(i) group != ug[i]
          }
          else                          # FUN
          {
              n <- niter
              selnProc <- function(i) indFun( data, classLab, i )
          }

	  #reddata = exprs(data)[ attr(terms(formula), "term.labels")
          fs.idx <- 1:nrow(exprs(data))           # feature set index
          ## feature set selection
          if (missing(fsFun))
              fsProc <- function(idx, fs.idx) fs.idx
          else
          { ## original by Stephen Henderson, to support feature selection
              if (!is.function(fsFun)) stop("fsFun should be a function")
              fsProc <- function(idx, fs.idx) {
                  fs.scores <- fsFun(data[,idx], classLab)
                  sort(fs.scores, index.return=TRUE, decreasing=decreasing)$ix[1:fsNum]
              }
          }

          ## cross-validator
          allGN = rownames(exprs(data))
          xvalidator <- function(i, ...) {
              idx <- selnProc(i)
              fs.idx <- fsProc(idx, fs.idx)
              newfmla = mkfmla(respn, allGN[fs.idx])
              list( MLearn(newfmla, data[fs.idx,], proc, inds[idx], ...)@predLabels@.Data, fs.idx )
          }

          out <- xvalLoop( 1:n, xvalidator, ... )
          classif <- unlist( sapply( out, function(x) x[[1]] ) )

          if (!is.function(fsFun))
              return(classif)
          else {
              fs.memory <- as.vector( sapply( out, function(x) x[[2]] ) )
              return(list(fs.memory=fs.memory, out=classif))
         }
      })

setMethod("xvalML", c("formula", "ExpressionSet", "character", 
                    "character", "missing", "missing", "missing", "missing",
                    "missing", "missing", "missing" ),
          function(formula, data, proc, 
                     xvalMethod="LOO", group, 
                     indFun, niter, fsFun=NULL, fsNum=10, 
                     decreasing=TRUE, cluster = NULL,...) {
xvalML( formula, data, proc, xvalMethod, 0, 
		...)
})

setMethod("xvalML", c("formula", "ExpressionSet", "character", 
                    "character", "missing", "missing", "missing", "function",
                    "missing", "missing", "missing" ),
          function(formula, data, proc, 
                     xvalMethod="LOO", group, 
                     indFun, niter, fsFun=NULL, fsNum=10, 
                     decreasing=TRUE, cluster = NULL,...) {
xvalML( formula, data, proc, xvalMethod, 0, , , fsFun ,
		...)
})
