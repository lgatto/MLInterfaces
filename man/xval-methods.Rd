\name{xval-methods}
\docType{methods}
\alias{xval-methods}
\alias{xval,exprSet,character,nonstandardGeneric,character,integer-method}
\alias{balKfold}
\title{ support for cross-validatory machine learning with exprSets}
\description{
support for cross-validatory machine learning with exprSets
}
\usage{
balKfold(K)
}
\arguments{
 \item{K}{number of balanced cross-validation partitions}
}
\section{Methods}{
\describe{

\item{data = "exprSet", classLab = "character", proc = "nonstandardGeneric", xvalMethod = "character", group = "integer"}{ \code{classLab} is the name of
a component of the phenoData of the exprSet passed as \code{data}.
\code{proc} is an actual MLInterfaces method (not the name of
a method).  \code{xvalMethod} may have value "LOO" for leave-one-out
or "LOG" for leave-group-out.  The latter makes use of the
\code{group} parameter.  samples sharing a value of \code{group}
are left out in one iteration of the cross-validation procedure,
and predictions are made for them together on the basis of the
fit from which they were excluded.
}}}
\examples{
library(golubEsets)
data(golubMerge)
smallG <- golubMerge[200:250,]
lk1 <- xval(smallG, "ALL.AML", knnB, xvalMethod="LOO", group=as.integer(0))
table(lk1,smallG$ALL.AML)
lk2 <- xval(smallG, "ALL.AML", knnB, xvalMethod="LOG", group=as.integer(
 rep(1:8,each=9)))
table(lk2,smallG$ALL.AML)
}
\keyword{methods}
