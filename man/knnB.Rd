\name{knnB}
\alias{allClass}
 \alias{stat.diag.daB}
 \alias{stat.diag.daB,ExpressionSet,character,integer-method}
 \alias{cvB}
 \alias{knn1B}
 \alias{knnP}
 \alias{lcaB}
 \alias{logitboostB}
 \alias{lvq2B}
 \alias{lvq3B}
 \alias{olvq1B}
 \alias{predLabels}
 \alias{RObject}
\alias{knnB}
\alias{nnetB}
\alias{lvq1B}
\alias{naiveBayesB}
\alias{svmB}
\alias{baggingB}
\alias{ipredknnB}
\alias{sldaB}
\alias{ldaB}
\alias{qdaB}
\alias{pamrB}
\alias{rpartB}
\alias{randomForestB}
\alias{gbmB}

  \alias{allClass,classifOutput-method}
  \alias{cvB,ExpressionSet,character-method}
  \alias{predLabels,MLOutput-method}
  \alias{predLabels,classifOutput-method}
\alias{last.warning}
  \alias{distMat,MLOutput-method}
  \alias{RObject,MLOutput-method}
  \alias{trainInds,classifOutput-method}
  \alias{show,probMat-method}
  \alias{show,probArray-method}
  \alias{show,membMat-method}
  \alias{show,qualScore-method}
  \alias{show,silhouetteVec-method}
  \alias{show,MLOutput-method}

  \alias{baggingB,ExpressionSet,character,integer-method}
  \alias{gbmB,ExpressionSet,character,integer-method}
  \alias{ipredknnB,ExpressionSet,character,integer-method}
  \alias{knn1B,ExpressionSet,character,integer-method}
  \alias{knnB,ExpressionSet,character,integer-method}
  \alias{lcaB,ExpressionSet,numeric-method}
  \alias{ldaB,ExpressionSet,character,integer-method}
  \alias{logitboostB,ExpressionSet,character,integer,numeric-method}
  \alias{lvq1B,ExpressionSet,character,integer-method}
  \alias{lvq2B,ExpressionSet,character,integer-method}
  \alias{lvq3B,ExpressionSet,character,integer-method}
  \alias{naiveBayesB,ExpressionSet,character,integer-method}
  \alias{nnetB,ExpressionSet,character,integer-method}
  \alias{olvq1B,ExpressionSet,character,integer-method}
  \alias{pamrB,ExpressionSet,character,integer-method}
  \alias{qdaB,ExpressionSet,character,integer-method}
  \alias{randomForestB,ExpressionSet,character,integer-method}
  \alias{rpartB,ExpressionSet,character,integer-method}
  \alias{sldaB,ExpressionSet,character,integer-method}
  \alias{svmB,ExpressionSet,character,integer-method}


\title{An interface to various machine learning methods for ExpressionSets }
\description{
 This document describes a family of
wrappers of calls to machine learning classifiers
distributed through various R packages.
This particular document concerns the classifiers
for which training-vs-test set application makes sense.

For example, \code{knnB} is a wrapper for a call to \code{\link[class]{knn}} for objects
 of class \code{ExpressionSet}. These interfaces, of the form \code{[f]B} provide a common calling
 sequence and common return value for machine learning code in function \code{[f]}.

For details on the additional arguments that may be passed to any covered
machine learning function \code{f}, check the manual page for that function.
This will require loading the package in which \code{f} is found.
}
\usage{
knnB(exprObj, classifLab, trainInd, k = 1, l = 1, prob = TRUE,
  use.all = TRUE, metric = "euclidean") 
}

\arguments{
  \item{exprObj}{An instance of the \code{exprset} class. }
  \item{classifLab}{The name of the phenotype variable to use for classification. }
  \item{trainInd}{ integer vector: Which elements are the training set. }
  \item{k}{The number of nearest neighbors. }
  \item{l}{See \code{knn} for a complete description. }
  \item{prob}{See \code{knn} for a complete description. }
  \item{use.all}{See \code{knn} for a complete description. }
  \item{metric}{ See \code{knn} for a complete description. }
}
\details{
  See \code{\link[class]{knn}} for a complete description of
parameters to and details of the k-nearest neighbor procedure
in the \code{class} package.
}
\value{
 An object of class \code{\link{classifOutput-class}}.
}

\author{Jess Mar, VJ Carey <stvjc@channing.harvard.edu>}

\seealso{ \code{\link{ldaB}} }
\examples{
# access and trim an ExpressionSet
library(golubEsets)
data(Golub_Merge)
smallG <- Golub_Merge[1:60,]
# set a PRNG seed for reproducibilitiy
set.seed(1234) # needed for nnet initialization
# now run the classifiers
knnB( smallG, "ALL.AML", 1:40 )
nnetB( smallG, "ALL.AML", 1:40, size=5, decay=.01 )
lvq1B( smallG, "ALL.AML", 1:40 )
naiveBayesB( smallG, "ALL.AML", 1:40 )
svmB( smallG, "ALL.AML", 1:40 )
baggingB( smallG, "ALL.AML", 1:40 )
ipredknnB( smallG, "ALL.AML", 1:40 )
sldaB( smallG, "ALL.AML", 1:40 )
ldaB( smallG, "ALL.AML", 1:40 )
qdaB( smallG[1:10,], "ALL.AML", 1:40 )
pamrB( smallG, "ALL.AML", 1:40 )
rpartB( smallG, "ALL.AML", 1:35 )
randomForestB( smallG, "ALL.AML", 1:35 )
gbmB( smallG, "ALL.AML", 1:40, n.minobsinnode=3 , n.trees=6000)
stat.diag.daB( smallG, "ALL.AML", 1:40 )
}
\keyword{ classif }

