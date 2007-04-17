\name{confuMat-methods}
\docType{methods}
\alias{confuMat}
\alias{confuMat-methods}
\alias{confuMat,classifOutput-method}
\title{ Methods for Function confuMat in Package `MLInterfaces' }
\description{
compute the confusion matrix for a classifier's output
}
\section{Methods}{
\describe{
\item{obj = "classifOutput"}{ Typically, an instance of class \code{"\linkS4class{classifOutput}"}
is built on a ``training'' subset of the input data, and the ``test'' subset's
true class labels are available.  This method cross-tabulates the true labels
of the test set against the predictions of the fitted classifier.
}}
}
\examples{
library(golubEsets)
data(Golub_Merge)
smallG <- Golub_Merge[1:50,]
k1 <- knnB(smallG, "ALL.AML", 1:30)
confuMat(k1)
}
\keyword{methods}
\keyword{ classif }
