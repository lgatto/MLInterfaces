\name{classifOutput-class}
\docType{class}
\alias{classifOutput-class}
\alias{show,classifOutput-method}
\alias{silhouetteB,classifOutput-method}

\title{Class "classifOutput" ~~~ }
\description{  ~~ A concise (1-5 lines) description of what the class is.  ~~}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("classifOutput", ...)}.
    ~~ describe objects here ~~ 
}
\section{Slots}{
  \describe{
    \item{\code{method}:}{Object of class \code{"character"} tag identifying
classification method in use }
    \item{\code{predLabels}:}{Object of class \code{"MLLabel"} vector
of labels predicted for test cases }
    \item{\code{predScores}:}{Object of class \code{"MLScore"} structure
of scores on classification events (for example, vote proportions
or fitted probabilities of class membership
 }
    \item{\code{call}:}{Object of class \code{"call"} call engendering the
classifOutput object }
    \item{\code{classifRobject}:}{Object of class \code{"ANY"} fitted model
object }
    \item{\code{distMat}:}{Object of class \code{"dist"} distance matrix 
of test set feature vectors }
  }
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "classifOutput")}: ... }
    \item{silhouetteB}{\code{signature(out = "classifOutput")}: ... }
  }
}
\references{ ~put references to the literature/web site here ~ }
\author{ ~~who you are~~ }
\note{ ~~further notes~~ }

 ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
  or \code{\link{CLASSNAME-class}} for links to other classes
}
\examples{
##---- Should be DIRECTLY executable !! ----
}
\keyword{classes}
