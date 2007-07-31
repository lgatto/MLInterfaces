\name{xval-methods}
\docType{methods}
\alias{xval-methods}
\alias{xval}
\alias{xvalML}
\alias{xval,ExpressionSet,character,nonstandardGeneric,character,missing-method}
\alias{xval,ExpressionSet,character,nonstandardGeneric,character,integer-method}
\alias{balKfold}
\alias{xval,ExpressionSet,character,genericFunction,character,integer-method}
\alias{xval,ExpressionSet,character,genericFunction,character,missing-method}
\alias{xvalML,formula,ExpressionSet,character,character,numeric-method}
\alias{xvalML,formula,ExpressionSet,character,character,missing-method}
\alias{xvalML,formula,ExpressionSet,character,character,numeric,ANY,ANY,ANY,ANY,ANY-method}
\alias{xvalML,formula,ExpressionSet,character,character,numeric,ANY,ANY,ANY,ANY,ANY,ANY-method}
\alias{xvalML,formula,ExpressionSet,character,character,missing,missing,missing,missing,missing,missing-method}
\alias{xvalML,formula,ExpressionSet,character,character,missing,missing,missing,missing,missing,missing,missing-method}
\alias{xvalML,formula,ExpressionSet,character,character,missing,missing,missing,function,missing,missing,missing-method}
\alias{xvalML,formula,ExpressionSet,character,character,missing,missing,missing,function,numeric,missing,missing-method}
\title{ support for cross-validatory machine learning with ExpressionSets}
\description{
support for cross-validatory machine learning with ExpressionSets
}
\usage{
xval( data, classLab, proc, xvalMethod, group, indFun, niter,
fsFun=NULL, fsNum=NULL, decreasing=TRUE, cluster=NULL, \dots )
balKfold(K)
xvalML( formula, data, proc, xvalMethod="LOO", group, indFun, niter,
fsFun=NULL, fsNum=10, decreasing=TRUE, cluster=NULL, \dots )
}
\arguments{
 \item{data}{instance of class \code{\link[Biobase]{ExpressionSet}}}
 \item{formula}{a model formula, typically with a dot on the RHS,
and response variable chosen from \code{pData} columns.}
 \item{classLab}{character string identifying phenoData variable to
   label classifications}
 \item{proc}{an MLInterfaces method that returns an instance of 
    \code{"\linkS4class{classifOutput}"}}
 \item{xvalMethod}{character string identifying cross-validation procedure to use:
default is "LOO" (leave one out), alternatives are "LOG" (leave group out)
and "FUN" (user-supplied partition extraction function, see Details below)}
 \item{group}{a vector (length equal to number of samples) enumerating groups for 
    LOG xval method}
 \item{indFun}{a function that returns a set of indices to be saved as a test set;
this function must have parameters \code{data}, \code{clab}, \code{iternum}; see 
    Details}
 \item{niter}{number of iterations for user-specified partition function to be  run}
 \item{fsFun}{function computing ranks of features for feature selection}
 \item{fsNum}{number of features to be kept for learning in each iteration}
 \item{decreasing}{logical, should be TRUE if \code{fsFun} provides high scores for high-performing features
(e.g., is absolute value of a test statistics) and false if it provides low scores
for high-performing features (e.g., p-value of a test).}
  \item{cluster}{NULL or an S4-class object with a defined
\code{\link{xvalLoop}} method. Use this to execute \code{xval} on
several nodes in a computer cluster. See documentation for
\code{\link{xvalLoop}} for more information}
 \item{\dots}{arguments passed to the MLInterfaces generic \code{proc}}
 \item{K}{number of partitions to be used if \code{balKfold} is used as \code{indFun}}
}
\section{Details}{
If \code{xvalMethod} is \code{"FUN"}, then \code{indFun} must be a function
with parameters \code{data}, \code{clab}, and \code{iternum}.
This function returns
indices that identify the training set for a given
cross-validation iteration passed as the value of \code{iternum}.  An example
function is printed out when the example of this page is executed.

if \code{fsFun} is not \code{NULL}, then it must be a function with two
arguments: the first can be transformed to a feature matrix (rows are objects,
columns are features) and the second is a vector of class labels.
The function returns a vector of scores, one for each object.  The
scores will be interpreted according to the value of \code{decreasing},
to select \code{fsNum} features.  Thanks to Stephen Henderson of University
College London for
this functionality.

Note that if \code{fsFun} is non-null, then the RHS of
\code{formula} will be
ignored, and it is assumed that the RHS is ".".  We will attempt
to ameliorate this in a future revision.  If you wish to subset
the features in \code{data} before applying cross-validated
feature selection, do this manually, not by specifying a nontrivial
formula.
}
%\section{Methods}{
%\describe{
%
%\item{data = "ExpressionSet", classLab = "character", proc = "nonstandardGeneric", xvalMethod = "character", group = "integer"}{ \code{classLab} is the name of
%a component of the phenoData of the ExpressionSet passed as \code{data}.
%\code{proc} is an actual MLInterfaces method (not the name of
%a method).  \code{xvalMethod} may have value "LOO" for leave-one-out
%or "LOG" for leave-group-out.  The latter makes use of the
%\code{group} parameter.  samples sharing a value of \code{group}
%are left out in one iteration of the cross-validation procedure,
%and predictions are made for them together on the basis of the
%fit from which they were excluded.
%}}}
\value{
  For fixed feature sets (\code{fsFun} not specified),
  a vector or matrix with length equal to the number of cross-validation
  assignments. Each element contains the label resulting from the
  cross-validation.

  For dynamic feature sets (\code{fsFun} specified), a list with element
  \code{out} containing labels from cross-validations, and element
  \code{fs.memory} recording features used in each cross-validation.
  }
\examples{
library(golubEsets)
data(Golub_Merge)
smallG <- Golub_Merge[200:250,]
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
#
# use Stephen Henderson's feature selection extensions
#
t.fun<-function(data, fac)
{
	require(genefilter)
	# deal with the integer storage of golubTrain@exprs!
	xd <- matrix(as.double(exprs(data)), nrow=nrow(exprs(data)))
        return(abs(rowttests(xd,pData(data)[[fac]], tstatOnly=FALSE)$statistic))
}
lk3f <- xval(smallG, "ALL.AML", knnB, xvalMethod="LOO", 0:0, fsFun=t.fun)
table(lk3f$out, smallG$ALL.AML)
# use MLearn xval
XXml = xvalML(ALL.AML~., smallG, "knn", "LOO")
# show that it agrees with the fB approach
table(XXml, lk1)
# use MLearn xval with feature selection
XXmlfs = xvalML(ALL.AML~., smallG, "knn", "LOO", fsFun=t.fun)
# show that it agrees with the previous approach
table(XXmlfs$out, lk3f$out)
}
\keyword{methods}
