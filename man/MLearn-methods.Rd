\name{MLearn-methods}
\docType{methods}
\alias{MLearn}
\alias{es2df}
\alias{tellMLearnMethods}
\alias{MLearn-methods}
\alias{MLearn,formula,ExpressionSet,character,numeric-method}
\alias{MLearn,formula,data.frame,character,numeric-method}
\alias{confuMatTrain,classifOutput-method}
\alias{confuMatTrain}
\alias{predLabelsTr}
\alias{predLabelsTr,classifOutput-method}
\title{ Methods for Function MLearn in Package `MLInterfaces' }
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

\code{MLearn} is an evolving interface.  To determine what 
values for \code{method} are currently supported, issue
the command \code{tellMLearnMethods()}.  This looks at the \code{MLearn}
code in real time and describes branch points in a switch statement.

An adaptation allows an \code{\link[Biobase]{ExpressionSet}} instance to be bound to
the \code{data} parameter.  The ExpressionSet and phenoData will be converted
to a data.frame instance using the internal \code{es2df} function, and this can be large.  Typically the genes will
be filtered before applying this procedure.

For this interface, one can obtain the training data confusion matrix using
\code{confuMatTrain}.  A slot \code{predLabelsTr} is populated for
this purpose, and an extractor method exists.
 }
}
\examples{
tellMLearnMethods()
library(MASS)
data(Pima.tr)
pm = MLearn(type~., data=Pima.tr, "lda", 1:150 )
confuMatTrain(pm)  # on training data
confuMat(pm)  # on held-out test data
library(golubEsets)
data(Golub_Merge)
rp = MLearn(ALL.AML~., Golub_Merge[1:200,], "rpart", 1:35 )
confuMat(rp)
sv = MLearn(ALL.AML~., Golub_Merge[1:200,], "svm", 1:35 )
confuMat(sv)
confuMatTrain(sv)
}
\keyword{methods}
