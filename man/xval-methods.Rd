\name{xval-methods}
\docType{methods}
\alias{xval-methods}
\alias{xval}
\alias{xval,exprSet,character,nonstandardGeneric,character,missing-method}
\alias{xval,exprSet,character,nonstandardGeneric,character,integer-method}
\alias{balKfold}
\alias{xval,exprSet,character,genericFunction,character,integer-method}
\alias{xval,exprSet,character,genericFunction,character,missing-method}

\title{ support for cross-validatory machine learning with exprSets}
\description{
support for cross-validatory machine learning with exprSets
}
\usage{
xval( data, classLab, proc, xvalMethod, group, indFun, niter, ... )
balKfold(K)
}
\arguments{
 \item{data}{instance of class \code{\link{exprSet}}}
 \item{classLab}{character string identifying phenoData variable to be regarded}
 \item{proc}{an MLInterfaces method that returns an instance of \code{\link{classifOutput}}}
 \item{xvalMethod}{character string identifying cross-validation procedure to use:
default is "LOO" (leave one out), alternatives are "LOG" (leave group out)
and "FUN" (user-supplied partition extraction function, see Details below)}
 \item{group}{a vector (length equal to number of samples) enumerating groups for LOG xval method}
 \item{indFun}{a function that returns a set of indices to be saved as a test set;
this function must have parameters \code{data}, \code{clab}, \code{iternum}; see Details}
 \item{niter}{number of iterations for user-specified partition function to be  run}
 \item{K}{number of partitions to be used if \code{balKfold} is used as \code{indFun}}
 item{...}{arguments passed to the MLInterfaces generic \code{proc}}
}
\section{Details}{
If \code{xvalMethod} is \code{"FUN"}, then \code{indFun} must be a function
with parameters \code{data}, \code{clab}, and \code{iternum}.
This function returns
indices that identify the training set for a given
cross-validation iteration passed as the value of \code{iternum}.  An example
function is printed out when the example of this page is executed.
}
%\section{Methods}{
%\describe{
%
%\item{data = "exprSet", classLab = "character", proc = "nonstandardGeneric", xvalMethod = "character", group = "integer"}{ \code{classLab} is the name of
%a component of the phenoData of the exprSet passed as \code{data}.
%\code{proc} is an actual MLInterfaces method (not the name of
%a method).  \code{xvalMethod} may have value "LOO" for leave-one-out
%or "LOG" for leave-group-out.  The latter makes use of the
%\code{group} parameter.  samples sharing a value of \code{group}
%are left out in one iteration of the cross-validation procedure,
%and predictions are made for them together on the basis of the
%fit from which they were excluded.
%}}}
\examples{
library(golubEsets)
data(golubMerge)
smallG <- golubMerge[200:250,]
lk1 <- xval(smallG, "ALL.AML", knnB, xvalMethod="LOO", group=as.integer(0))
table(lk1,smallG$ALL.AML)
lk2 <- xval(smallG, "ALL.AML", knnB, xvalMethod="LOG", group=as.integer(
 rep(1:8,each=9)))
table(lk2,smallG$ALL.AML)
balKfold
lk3 <- xval(smallG, "ALL.AML", knnB, xvalMethod="FUN", 0:0, indFun=balKfold(5), niter=5)
table(lk3, smallG$ALL.AML)
#
# illustrate the xval FUN method in comparison to LOO
#
LOO2 <- xval(smallG, "ALL.AML", knnB, "FUN", 0:0, function(x,y,i) {
  (1:ncol(exprs(x)))[-i] }, niter=72 )
table(lk1, LOO2)
}
\keyword{methods}
