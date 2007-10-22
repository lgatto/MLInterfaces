\name{classifierOutput-class}
\docType{class}
\alias{classifierOutput-class}
\alias{confuMat,classifierOutput-method}
\alias{RObject,classifierOutput-method}
\alias{show,classifierOutput-method}
\alias{testScores,classifierOutput-method}
\alias{testPredictions,classifierOutput-method}
\alias{testPredictions}
\alias{trainPredictions}
\alias{trainPredictions,classifierOutput-method}
\alias{fsHistory,classifierOutput-method}
\alias{testScores}
\alias{testPredictions}
%\alias{fsHistory}

\title{Class "classifierOutput"}
\description{
This class summarizes the output values from different classifiers.}
\section{Objects from the Class}{
Objects are typically created during the application of a supervised 
machine learning algorithm to data and are the value returned.  It is very
unlikely that any user would create such an object by hand.
}
\section{Slots}{
	 \describe{
    \item{\code{testOutcomes}:}{Object of class \code{"factor"} ~~ }
    \item{\code{testPredictions}:}{Object of class \code{"factor"} ~~ }
    \item{\code{testScores}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{trainOutcomes}:}{Object of class \code{"factor"} ~~ }
    \item{\code{trainPredictions}:}{Object of class \code{"factor"} ~~ }
    \item{\code{trainScores}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{RObject}:}{Object of class \code{"ANY"} ~~ }
    \item{\code{call}:}{Object of class \code{"call"} ~~ }
  }
}
\section{Methods}{
  \describe{
    \item{confuMat}{\code{signature(obj = "classifierOutput")}: Compute
    the confusion matrix. }
    \item{RObject}{\code{signature(obj = "classifierOutput")}: The R object
     returned by the underlying classifier. This can then be passed on to
     specific methods for those objects, when they exist. }
    \item{show}{\code{signature(object = "classifierOutput")}: A print method
      that provides a summary of the output of the classifier. }
    \item{testScores}{\code{signature(object = "classifierOutput")}: ... }
    \item{testPredictions}{\code{signature(object = "classifierOutput")}: Print
     the predicted classes for each sample/individual in the test set. }
    \item{trainPredictions}{\code{signature(object = "classifierOutput")}: Print
     the predicted classes for each sample/individual in the training set. }
    \item{fsHistory}{\code{signature(object = "classifierOutput")}: ... }
	 }
}
\author{ V. Carey }
%\seealso{
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%	or \code{\linkS4class{CLASSNAME}} for links to other classes
%}
\examples{
showClass("classifierOutput")
}
\keyword{classes}
