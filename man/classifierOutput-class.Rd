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
    \item{\code{testOutcomes}:}{Object of class \code{"factor"} that
lists the actual outcomes in the records on the test set }
    \item{\code{testPredictions}:}{Object of class \code{"factor"} that
lists the predictions of outcomes in the test set }
    \item{\code{testScores}:}{Object of class \code{"ANY"} -- this
element will include matrices or vectors or arrays that include
information that is typically related to the posterior probability
of occupancy of the predicted class or of all classes.  The actual
contents of this slot can be determined by inspecting the converter
element of the learnerSchema used to select the model. }
    \item{\code{trainOutcomes}:}{Object of class \code{"factor"} that
lists the actual outcomes in records on the training set }
    \item{\code{trainPredictions}:}{Object of class \code{"factor"} that
lists the predicted outcomes in the training set }
    \item{\code{trainScores}:}{Object of class \code{"ANY"} see
the description of \code{testScores} above; the same information
is returned, but applicable to the training set records.
 }
    \item{\code{RObject}:}{Object of class \code{"ANY"} -- when
the \code{trainInd} parameter of the \code{MLearn} call is
numeric, this slot holds
the return value of the underlying R function that carried out
the predictive modeling.  For example, if \code{rpartI} was used
as MLearn \code{method}, \code{Robject} holds an instance of the
\code{rpart} S3 class, and \code{plot} and \code{text} methods
can be applied to this.  When the \code{trainInd} parameter
of the \code{MLearn} call is an instance of
\code{\linkS4class{xvalSpec}}, this slot holds a \code{list} of
results of cross-validatory iterations.  Each element of this
list has two elements: \code{test.idx}, giving the numeric
indices of the test cases for the associated cross-validation
iteration, and \code{mlans}, which is the \code{\linkS4class{classifierOutput}}
for the associated iteration.   }
    \item{\code{call}:}{Object of class \code{"call"} -- records the
call used to generate the classifierOutput RObject }
  }
}
\section{Methods}{
  \describe{
    \item{confuMat}{\code{signature(obj = "classifierOutput")}: Compute
    the confusion matrix for test records. }
    \item{confuMatTrain}{\code{signature(obj = "classifierOutput")}: Compute
    the confusion matrix for training set.  Typically yields optimistically biased
    information on misclassification rate. }
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
