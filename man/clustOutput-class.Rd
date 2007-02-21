\name{clustOutput-class}
\docType{class}
\alias{clustOutput-class}
\alias{fclust-class}
\alias{bclust-class}
\alias{hclust-class}
\alias{show,clustOutput-method}
\alias{silhouetteB,clustOutput-method}


\title{Class "clustOutput" container for cluster analysis results }
\description{	 container for cluster analysis results}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("clustOutput", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{clustIndices}:}{Object of class \code{\link{MLLabel}}  }
    \item{\code{clustScores}:}{Object of class \code{\link{MLScore}}, when
a numerical figure of clustering is available  }
    \item{\code{method}:}{Object of class \code{"character"} tag indicating
name of procedure }
    \item{\code{RObject}:}{Object of class \code{"ANY"} unprocessed output of the
clustering function }
    \item{\code{call}:}{Object of class \code{"call"} audit of the call that
created the object }
    \item{\code{distMat}:}{Object of class \code{"dist"} distance among objects }
  }
}
\section{Extends}{
Class \code{"\linkS4class{MLOutput}"}, directly.
}
\section{Methods}{
  \describe{
    \item{show}{\code{signature(object = "clustOutput")}: concise report }
    \item{silhouetteB}{\code{signature(out = "clustOutput")}: compute the \code{\link[cluster]{silhouette}}
object corresponding to the partition }
  }
}
%\references{ ~put references to the literature/web site here ~ }
\author{VJ Carey <stvjc@channing.harvard.edu> }


\seealso{
	\code{\linkS4class{"classifOutput"}}
}
\examples{
showClass("clustOutput")

 library(golubEsets)
 data(Golub_Merge)
 train <- c(sample(1:47, 23), sample(48:72, 12))
 agOut <- agnesB(Golub_Merge[100:200,], 3)
 agOut
 sil.ag <- silhouetteB(agOut)
 plot(sil.ag, main="Silhouette Plot for Agnes Clustering")

}
\keyword{classes}
