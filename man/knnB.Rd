\name{knnB}
\alias{allClass}
 \alias{balKfold}
 \alias{stat.diag.daB}
 \alias{chkMLInterfaceProc}
 \alias{claraB}
 \alias{confuMat}
 \alias{cvB}
 \alias{distMat}
 \alias{fannyB}
 \alias{getGrid}
 \alias{isoMDSB}
 \alias{knn1B}
 \alias{knn.cvB}
 \alias{knnP}
 \alias{lcaB}
 \alias{logitboostB}
 \alias{lvq2B}
 \alias{lvq3B}
 \alias{makeCVFunc}
 \alias{newGroupIndex}
 \alias{newMembMat}
 \alias{newPredClass}
 \alias{newProbArray}
 \alias{newProbMat}
 \alias{newQualScore}
 \alias{newSilhouetteVec}
 \alias{olvq1B}
 \alias{predict.knnP}
 \alias{predLabels}
 \alias{print.knnP}
 \alias{RObject}
 \alias{silhouetteB}
 \alias{somB}
 \alias{SOMB}
 \alias{trainInds}
 \alias{wrapClust}
 \alias{xval}
\alias{knnB}
\alias{nnetB}
\alias{lvq1B}
\alias{agnesB}
\alias{dianaB}
\alias{pamB}
\alias{bclustB}
\alias{cmeansB}
\alias{cshellB}
\alias{naiveBayesB}
\alias{svmB}
\alias{baggingB}
\alias{ipredknnB}
\alias{sldaB}
\alias{ldaB}
\alias{qdaB}
\alias{pamrB}
\alias{kmeansB}
\alias{rpartB}
\alias{randomForestB}
\alias{hclustB}
\alias{gbmB}

 \alias{agnesB,exprSet,numeric-method}
  \alias{allClass,classifOutput-method}
  \alias{baggingB,exprSet,character,integer-method}
  \alias{bclustB,exprSet,numeric-method}
  \alias{claraB,exprSet,numeric-method}
  \alias{cmeansB,exprSet,numeric-method}
  \alias{confuMat,classifOutput-method}
  \alias{cshellB,exprSet,numeric-method}
  \alias{cvB,exprSet,character-method}
  \alias{dianaB,exprSet,numeric-method}
  \alias{distMat,MLOutput-method}
  \alias{fannyB,exprSet,numeric-method}
  \alias{gbmB,exprSet,character,integer-method}
  \alias{hclustB,exprSet,numeric,missing-method}
  \alias{hclustB,exprSet,numeric,ANY-method}
  \alias{hclustB,exprSet,missing,numeric-method}
  \alias{ipredknnB,exprSet,character,integer-method}
  \alias{kmeansB,exprSet,numeric-method}
  \alias{knn1B,exprSet,character,integer-method}
  \alias{knnB,exprSet,character,integer-method}
  \alias{knn.cvB,exprSet,character-method}
  \alias{lcaB,exprSet,numeric-method}
  \alias{ldaB,exprSet,character,integer-method}
  \alias{logitboostB,exprSet,character,integer,numeric-method}
  \alias{lvq1B,exprSet,character,integer-method}
  \alias{lvq2B,exprSet,character,integer-method}
  \alias{lvq3B,exprSet,character,integer-method}
  \alias{naiveBayesB,exprSet,character,integer-method}
  \alias{nnetB,exprSet,character,integer-method}
  \alias{olvq1B,exprSet,character,integer-method}
  \alias{pamB,exprSet,numeric-method}
  \alias{pamrB,exprSet,character,integer-method}
  \alias{predLabels,MLOutput-method}
  \alias{predLabels,classifOutput-method}
\alias{last.warning}
  \alias{qdaB,exprSet,character,integer-method}
  \alias{randomForestB,exprSet,character,integer-method}
  \alias{RObject,MLOutput-method}
  \alias{rpartB,exprSet,character,integer-method}
  \alias{show,probMat-method}
  \alias{show,probArray-method}
  \alias{show,membMat-method}
  \alias{show,qualScore-method}
  \alias{show,silhouetteVec-method}
  \alias{show,MLOutput-method}
  \alias{show,SOMBout-method}
  \alias{sldaB,exprSet,character,integer-method}
  \alias{somB,exprSet,character-method}
  \alias{SOMB,exprSet,character-method}
  \alias{svmB,exprSet,character,integer-method}
  \alias{trainInds,classifOutput-method}
  \alias{xval,exprSet,character,nonstandardGeneric,character,missing-method}


\title{An interface to various machine learning methods for exprSets }
\description{
 This function is a wrapper for a call to \code{\link{knn}} for objects
 of class \code{exprSet}. These interfaces provide a common calling
 sequence and common return value for machine learning code.
}
\usage{
knnB(exprObj, classifLab, trainInd, k = 1, l = 1, prob = TRUE,
  use.all = TRUE, metric = "euclidean") 
}

\arguments{
  \item{exprObj}{An instance of the \code{exprset} class. }
  \item{classifLab}{A vector of class labels. }
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
 An object of class \code{\link{classifOutput-class}}.
}

\author{Jess Mar, VJ Carey <stvjc@channing.harvard.edu>}

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
naiveBayesB( smallG, "ALL.AML", 1:40 )
svmB( smallG, "ALL.AML", 1:40 )
baggingB( smallG, "ALL.AML", 1:40 )
ipredknnB( smallG, "ALL.AML", 1:40 )
sldaB( smallG, "ALL.AML", 1:40 )
ldaB( smallG, "ALL.AML", 1:40 )
qdaB( smallG[1:10,], "ALL.AML", 1:40 )
pamrB( smallG, "ALL.AML", 1:40 )
LOO1 <- xval(smallG, "ALL.AML", knnB, "LOO", 0:0)
LOO2 <- xval(smallG, "ALL.AML", knnB, "FUN", 0:0, function(x,y,i) {
  (1:ncol(exprs(x)))[-i] }, niter=72 )
table(LOO1, LOO2)
kmeansB( smallG, k=4 )
rpartB( smallG, "ALL.AML", 1:35 )
randomForestB( smallG, "ALL.AML", 1:35 )
hclustB( smallG, k=4 )
hclustB( smallG, height=40000 )
gbmB( smallG, "ALL.AML", 1:40, n.minobsinnode=3 , n.trees=6000)
if (require(LogitBoost)) logitboostB( smallG, "ALL.AML", 1:40, 200 ) # summarize won't work with polych
stat.diag.daB( smallG, "ALL.AML", 1:40 )
}
\keyword{ classif }

