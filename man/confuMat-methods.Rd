\name{confuMat-methods}
\docType{methods}
\alias{confuMat}
\alias{confuMat-methods}
\alias{confuMat,classifOutput-method}
\title{ Compute the confusion matrix for a classifier. }
\description{
This function will compute the confusion matrix for a classifier's output
}
\section{Methods}{
\describe{
\item{obj = "classifOutput"}{ Typically, an instance of class 
\code{"\linkS4class{classifOutput}"}
is built on a training subset of the input data. The model is then
used to predict the class of samples in the test set.  When the
true class labels for the test set are available the confusion matrix
is the cross-tabulation of the true labels
of the test set against the predictions from the classifier.
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
