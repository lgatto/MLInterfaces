\name{learnerSchema-class}
\docType{class}
\alias{learnerSchema-class}
\alias{nonstandardLearnerSchema-class}
\alias{show,learnerSchema-method}

\title{Class "learnerSchema" -- convey information on a machine learning
function to the MLearn wrapper }
\description{	 conveys information about machine learning functions in
CRAN packages, for example, to MLearn wrapper }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("learnerSchema", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{packageName}:}{Object of class \code{"character"} string naming the
package in which the function to be used is defined. }
    \item{\code{mlFunName}:}{Object of class \code{"character"} string naming
the function to be used }
    \item{\code{converter}:}{Object of class \code{"function"} function with parameters
obj, data, trainInd, that will produce a classifierOutput instance }
  }
}
\section{Methods}{
  \describe{
    \item{MLearn}{\code{signature(formula = "formula", data = "ExpressionSet", method = "learnerSchema", trainInd = "numeric")}: execute desired learner passing a formula
and ExpressionSet}
    \item{MLearn}{\code{signature(formula = "formula", data = "data.frame", method = "learnerSchema", trainInd = "numeric")}: execute desired learner passing a formula }
    \item{show}{\code{signature(object = "learnerSchema")}: concise display }
	 }
}
%\references{ ~put references to the literature/web site here ~ }
\author{ Vince Carey <stvjc@channing.harvard.edu>}
%\note{ ~~further notes~~ }

% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%	or \code{\linkS4class{CLASSNAME}} for links to other classes
%}
\examples{
showClass("learnerSchema")
}
\keyword{classes}
