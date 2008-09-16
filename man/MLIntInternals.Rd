\name{MLIntInternals}
 \alias{chkMLInterfaceProc}
 \alias{getGrid}
 \alias{getGrid,data.frame-method}
 \alias{getGrid,ExpressionSet-method}
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
\alias{MLLabel }
\alias{MLOutput }
\alias{MLScore }
\alias{SOMBout }
\alias{classifOutput }
\alias{clustOutput }
\alias{groupIndex}
\alias{membMat }
\alias{probArray }
\alias{probMat }
\alias{qualScore }
\alias{silhouetteVec }
\alias{somout}

\title{MLInterfaces infrastructure}
\description{
These functions are internal tools for \code{MLInterfaces}.
Users will generally not call these functions directly.
}
\usage{
chkMLInterfaceProc(x)
getGrid(x)
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

