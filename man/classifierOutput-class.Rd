\name{classifierOutput-class}
\docType{class}
\alias{classifierOutput-class}
\alias{confuMat,classifierOutput-method}
\alias{RObject,classifierOutput-method}
\alias{show,classifierOutput-method}
\alias{testScores,classifierOutput-method}
\alias{testPredictions,classifierOutput-method}
\alias{testScores}
\alias{testPredictions}

\title{Class "classifierOutput" ~~~ }
\description{	 ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("classifierOutput", ...)}.
	 ~~ describe objects here ~~ 
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
    \item{confuMat}{\code{signature(obj = "classifierOutput")}: ... }
    \item{RObject}{\code{signature(obj = "classifierOutput")}: ... }
    \item{show}{\code{signature(object = "classifierOutput")}: ... }
    \item{testScores}{\code{signature(object = "classifierOutput")}: ... }
    \item{testPredictions}{\code{signature(object = "classifierOutput")}: ... }
	 }
}
%\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%	or \code{\linkS4class{CLASSNAME}} for links to other classes
%}
\examples{
showClass("classifierOutput")
}
\keyword{classes}
