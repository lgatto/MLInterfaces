\name{MLIclust}
\alias{hclustB}
\alias{agnesB}
\alias{dianaB}
\alias{pamB}
\alias{bclustB}
 \alias{trainInds}
 \alias{silhouetteB}
 \alias{claraB}
 \alias{distMat}
 \alias{fannyB}
 \alias{isoMDSB}

\alias{cmeansB}
\alias{cshellB}
\alias{kmeansB}
 \alias{agnesB,ExpressionSet,numeric-method}
  \alias{bclustB,ExpressionSet,numeric-method}
  \alias{claraB,ExpressionSet,numeric-method}
  \alias{cmeansB,ExpressionSet,numeric-method}
  \alias{hclustB,ExpressionSet,numeric,missing-method}
  \alias{hclustB,ExpressionSet,numeric,ANY-method}
  \alias{hclustB,ExpressionSet,missing,numeric-method}
  \alias{cshellB,ExpressionSet,numeric-method}
  \alias{dianaB,ExpressionSet,numeric-method}
  \alias{fannyB,ExpressionSet,numeric-method}
  \alias{kmeansB,ExpressionSet,numeric-method}
  \alias{pamB,ExpressionSet,numeric-method}

\title{An interface to various unsupervised machine learning methods for ExpressionSets }
\description{
This document describes a family of wrappers to R functions
that perform clustering of multivariate datasets.
 For example, the
\code{kmeansB} function is a wrapper for a call to \code{\link{kmeans}} for objects
 of class \code{ExpressionSet}. The interfaces of \code{MLInterfaces} provide a common calling
 sequence and common return value for machine learning code.
}
\usage{
kmeansB(exprObj, k , iter.max, metric = "euclidean") 
}

\arguments{
  \item{exprObj}{An instance of the \code{exprset} class. }
  \item{k}{The number of clusters}
  \item{iter.max}{The maximum number of iterations to perform.}
  \item{metric}{ See \code{knn} for a complete description. }
}
\details{
  See \code{\link{kmeans}} for a complete description, and for other
functions named [f]B, see the documentation on \code{f}.
}
\value{
 An object of class \code{\link{clustOutput-class}}.
}

\author{Jess Mar, VJ Carey <stvjc@channing.harvard.edu>}

\seealso{ \code{\link{ldaB}} }
\examples{
##get these from the source files
library(golubEsets)
data(Golub_Merge)
smallG <- Golub_Merge[1:60,]
set.seed(1234) # needed for nnet initialization
kmeansB( smallG, k=4 )
agnesB( smallG, k=5, height=0, FALSE )
dianaB( smallG, k=5, height=0, FALSE )
pamB( smallG, k=5, height=0, FALSE )
bclustB( smallG, k=5, height=0 )
cmeansB( smallG, k=5, height=0 )
cshellB( smallG, k=5, height=0 , iter.max=1)
#lcaB( BsmG, k=3 )
hclustB( smallG, k=4 )
hclustB( smallG, height=40000 )
}
\keyword{ classif }

