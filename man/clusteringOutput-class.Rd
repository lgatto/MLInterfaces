\name{clusteringOutput-class}
\docType{class}
\alias{clusteringOutput-class}
\alias{RObject,clusteringOutput-method}
\alias{plot,clusteringOutput,ANY-method}
\alias{show,clusteringOutput-method}
\alias{show,clusteringSchema-method}
\alias{getConverter,clusteringSchema-method}
\alias{getDist,clusteringSchema-method}
\alias{getConverter}
\alias{getDist}

\title{container for clustering outputs in uniform structure}
\description{container for clustering outputs in uniform structure}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("clusteringOutput", ...)}.
}
\section{Slots}{
	 \describe{
    \item{\code{partition}:}{Object of class \code{"integer"}, labels for
observations as clustered }
    \item{\code{silhouette}:}{Object of class \code{"silhouette"},
structure from Rousseeuw cluster package measuring cluster
membership strength per observation}
    \item{\code{distEnv}:}{Object of class \code{"environment"} not in use }
    \item{\code{prcomp}:}{Object of class \code{"prcompObj"} a wrapped
instance of stats package prcomp output }
    \item{\code{metric}:}{Object of class \code{"character"} string
identifying the distance function used to orient objects in feature space }
    \item{\code{call}:}{Object of class \code{"call"} for auditing }
    \item{\code{learnerSchema}:}{Object of class \code{"learnerSchema"}, a
formal object indicating the package, function, and other attributes
of the clustering algorithm employed to generate this object  }
    \item{\code{RObject}:}{Object of class \code{"ANY"}, the
unaltered output of the function called according to learnerSchema }
  }
}
\section{Methods}{
  \describe{
    \item{RObject}{\code{signature(x = "clusteringOutput")}: extract
the unaltered output of the R function or method called according
to learnerSchema }
    \item{plot}{\code{signature(x = "clusteringOutput", y = "ANY")}: 
a 4-panel plot showing features of the clustering, including the
scree plot for a principal components transformation and a display
of the partition in PC1xPC2 plane.  For a clustering method that
does not have a native plot procedure, such as kmeans, the parameter
y should be bound to a data frame or matrix with feature data for all records;
an image plot of robust feature z-scores (z=(x-median(x))/mad(x)) and
the cluster indices is produced in the northwest panel.  }
    \item{show}{\code{signature(object = "clusteringOutput")}: concise
report }
	 }
}
%\references{ ~put references to the literature/web site here ~ }
\author{ VJ Carey <stvjc@channing.harvard.edu>}
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%	~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%	or \code{\linkS4class{CLASSNAME}} for links to other classes
%}
\examples{
showClass("clusteringOutput")
}
\keyword{classes}
