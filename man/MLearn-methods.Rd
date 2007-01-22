\name{MLearn-methods}
\docType{methods}
\alias{MLearn}
\alias{es2df}
\alias{MLearn-methods}
\alias{MLearn,formula,ExpressionSet,character,numeric-method}
\alias{MLearn,formula,data.frame,character,numeric-method}
\title{ ~~ Methods for Function MLearn in Package `MLInterfaces' ~~}
\description{
test version of more lightweight interface
}
\section{Methods}{
\describe{

The parameters of the generic are \code{formula}, \code{data}, \code{method},
and \code{trainInds}.

The fundamental method employs a formula and a data.frame instance and
applies a machine learning algorithm identified by \code{method}, specifying
the training set indices for the training run.  An instance of \code{\link{MLOutput-class}}
is returned.

An adaptation allows an \code{\link[Biobase]{ExpressionSet}} instance to be bound to
the \code{data} parameter.  The ExpressionSet and phenoData will be converted
to a data.frame instance using the internal \code{es2df} function, and this can be large.  Typically the genes will
be filtered before applying this procedure.

 }
}
\examples{
library(MASS)
data(Pima.tr)
pm = MLearn(type~., data=Pima.tr, "lda", 1:150 )
confuMat(pm)
library(golubEsets)
data(Golub_Merge)
rp = MLearn(ALL.AML~., Golub_Merge[1:200,], "rpart", 1:35 )
confuMat(rp)
sv = MLearn(ALL.AML~., Golub_Merge[1:200,], "svm", 1:35 )
confuMat(sv)
}
\keyword{methods}
