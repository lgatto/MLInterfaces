\name{clusterIntf}
\alias{clusterIntf}
\title{An interface to clustering procedures for exprSets }
\description{
 These functions provide wrappers to clustering procedures
in various packages, for application to instances
 of class \code{exprSet}. These interfaces provide a common calling
 sequence and common return value for clustering code.
}
\usage{
agnesB(exprObj, k, height, stand = FALSE, 
	method = "average", keep.diss = TRUE, 
	keep.data = TRUE, metric = "euclidean", ...)
bclustB(exprObj, k, height = 0, iter.base = 10, 
	minsize = 0, dist.method = "euclidian", 
	hclust.method = "average", base.method = "kmeans", 
 	base.centers = 5, verbose = TRUE, final.kmeans = FALSE, 
	docmdscale = FALSE, resample = TRUE, weights, maxcluster = 5, ...)
claraB(exprObj, k, height = 0, stand = FALSE, samples = 5, 
	sampsize, trace = 0, keep.data = TRUE, keepdata, 
	rngR = FALSE, metric = "euclidean")
cmeansB(exprObj, k, height = 0, iter.max = 100, verbose = FALSE, 
	dist = "euclidean", method = "cmeans", m = 2, rate.par = NULL)
cshellB(exprObj, k, height = 0, iter.max = 20, verbose = FALSE, 
	dist = "euclidean", method = "cshell", m = 2, radius)
dianaB(exprObj, k, height, diss, stand = FALSE, 
	keep.diss, keep.data = TRUE, metric = "euclidean")
fannyB(exprObj, k, height = 0, diss, stand = FALSE, metric = "euclidean")
hclustB(exprObj, classifLab, method = "complete", 
	members, metric = "euclidean")
kmeansB(exprObj, classifLab, centers, iter.max = 10, 
	metric = "euclidean")
lcaB(exprObj, k, niter = 100, matchdata = TRUE, verbose = FALSE, 
	metric = "euclidean")
pamB(exprObj, k, height = 0, diss, stand = FALSE, keep.diss = TRUE, 
	keep.data = TRUE, metric = "euclidean")
}
\arguments{
  \item{exprObj}{An instance of the \code{exprset} class. }
  \item{k}{The stipulated number of clusters}
  \item{height}{height in the clustering tree to be used
for defining clusters, when appropriate}
  \item{metric}{ See \code{\link{dist}} for a complete description. }
}
\details{
  See \code{\link{knn}} for a complete description.
}
\value{
 An object of class \code{\link{clustOutput}}.
}

\author{Jess Mar, VJ Carey}

\examples{
library(golubEsets)
data(golubMerge)
smallG <- golubMerge[1:60,]
agnesB( smallG, k=5, stand= FALSE )
dianaB( smallG, k=5, stand=FALSE )
pamB( smallG, k=5, stand=FALSE )
bclustB( smallG, k=5)
cmeansB( smallG, k=5)
cshellB( smallG, k=5, iter.max=1)
hclustB( smallG, k=5)
kmeansB( smallG, k=5) 
}
\keyword{ classif }

