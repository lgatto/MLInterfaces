\name{MLIntInternals}
 \alias{chkMLInterfaceProc}
 \alias{getGrid}
 \alias{getGrid,data.frame-method}
 \alias{getGrid,ExpressionSet-method}
 \alias{makeCVFunc}
 \alias{newGroupIndex}
 \alias{newMembMat}
 \alias{newPredClass}
 \alias{newProbArray}
 \alias{newProbMat}
 \alias{newQualScore}
 \alias{newSilhouetteVec}
 \alias{predict.knnP}
 \alias{print.knnP}
 \alias{wrapClust}
\title{MLInterfaces infrastructure}
\description{
These functions are internal tools for \code{MLInterfaces}.
Users will generally not call these functions directly.
}
\usage{
chkMLInterfaceProc(x)
getGrid(x)
makeCVFunc(algorithm, ...)
newGroupIndex(x)
newMembMat(x)
newPredClass(x)
newProbArray(x)
newProbMat(x)
newQualScore(x)
newSilhouetteVec(x)
predict.knnP(object, ...)
print.knnP(x, ...)
wrapClust(out, k, height, dis)
}

\arguments{
  \item{x}{a vector or matrix or ExpressionSet}
  \item{algorithm}{ function }
  \item{out}{output of cluster procedure}
  \item{k}{number of clusters}
  \item{height}{see \code{\link{hclust}}}
  \item{dis}{distance matrix}
  \item{object}{instance of S3 class knnP}
  \item{...}{arguments to print}
}
\details{
Forthcoming.
}
\value{
Functions with `new' as prefix are constructor helpers.
}

\author{VJ Carey <stvjc@channing.harvard.edu>}

\examples{
}
\keyword{ classif }

