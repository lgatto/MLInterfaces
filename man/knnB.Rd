\name{knnB}
\alias{knnB}
\title{An interface to knn for exprSets }
\description{
 This function is a wrapper for a call to \code{\link{knn}} for objects
 of class \code{exprSet}. These interfaces provide a common calling
 sequence and common return value for machine learning code.
}
\usage{
knnB(exprObj, classLab, trainInd, k = 1, l = 1, prob = TRUE,
  use.all = TRUE, metric = "euclidean") 
}

\arguments{
  \item{exprObj}{An instance of the \code{exprset} class. }
  \item{classLab}{A vector of class labels. }
  \item{trainInd}{Which are the training set. }
  \item{k}{The number of nearest neighbors. }
  \item{l}{See \code{knn} for a complete description. }
  \item{prob}{See \code{knn} for a complete description. }
  \item{use.all}{See \code{knn} for a complete description. }
  \item{metric}{ See \code{knn} for a complete description. }
}
\details{
  See \code{\link{knn}} for a complete description.
}
\value{
 An object of class \code{\link{classifOutput}}.
}

\author{Jess Mar}

\seealso{ \code{\link{ldaB}} }
\examples{
##get these from the source files
library(golubEsets)
data(golubMerge)
smallG <- golubMerge[1:60,]
knnB( smallG, "ALL.AML", 1:40 )
set.seed(1234) # needed for nnet initialization
nnetB( smallG, "ALL.AML", 1:40, size=5, decay=.01 )
lvq1B( smallG, "ALL.AML", 1:40 )
agnesB( smallG, k=5, height=0, FALSE )
dianaB( smallG, k=5, height=0, FALSE )
pamB( smallG, k=5, height=0, FALSE )
bclustB( smallG, k=5, height=0 )
cmeansB( smallG, k=5, height=0 )
cshellB( smallG, k=5, height=0 , iter.max=1)
#lcaB( BsmG, k=3 )
svmB( smallG, "ALL.AML", 1:40 )
baggingB( smallG, "ALL.AML", 1:40 )
ipredknnB( smallG, "ALL.AML", 1:40 )
sldaB( smallG, "ALL.AML", 1:40 )
}
\keyword{ classif }

