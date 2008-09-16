\name{confuMat-methods}
\docType{methods}
\alias{confuMat}
\alias{confuMat-methods}
\alias{confuMat,classifierOutput-method}
\alias{confuMat,classifierOutput,character-method}
\alias{confuMat,classifierOutput,missing-method}
\title{ Compute the confusion matrix for a classifier. }
\description{
This function will compute the confusion matrix for a classifier's output
}
\section{Methods}{
\describe{
\item{obj = "classifOutput"}{ Typically, an instance of class 
\code{"\linkS4class{classifierOutput}"}
is built on a training subset of the input data. The model is then
used to predict the class of samples in the test set.  When the
true class labels for the test set are available the confusion matrix
is the cross-tabulation of the true labels
of the test set against the predictions from the classifier.
    } % end item
\item{obj = "classifierOutput", type="character"}{ 
For instances of \linkS4class{classifierOutput}, it is possible
to specify the \code{type} of confusion matrix desired.
The default is \code{test}, which tabulates classes from the
test set against the associated predictions.  If \code{type} is
\code{train}, the training class vector is tabulated against the
predictions on the training set.
    } % end item
  } % end describe
}  % end Methods section
\examples{
library(golubEsets)
data(Golub_Merge)
smallG <- Golub_Merge[101:150,]
k1 <- MLearn(ALL.AML~., smallG, knnI(k=1), 1:30)
confuMat(k1)
confuMat(k1, "train")
}
\keyword{methods}
\keyword{ classif }
