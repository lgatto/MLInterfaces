\name{classifOutput-class}
\docType{class}
\alias{classifOutput-class}
\alias{agnes-class}
\alias{bagging-class}
\alias{bclust-class}
\alias{classbagg-class}
\alias{clustOutput-class}
\alias{cshell-class}
\alias{diana-class}
\alias{fclust-class}
\alias{gbm-class}
\alias{groupIndex-class}
\alias{hclust-class}
\alias{ica-class}
\alias{ipredknn-class}
\alias{kmeans-class}
\alias{knnP-class}
\alias{lca-class}
\alias{lda-class}
\alias{logitboost-class}
\alias{membMat-class}
\alias{MLLabel-class}
\alias{MLOutput-class}
\alias{MLScore-class}
\alias{naiveBayes-class}
\alias{nnet.formula-class}
\alias{nsc-class}
\alias{pam-class}
\alias{pamr-class}
\alias{prcomp-class}
\alias{predClass-class}
\alias{probArray-class}
\alias{probMat-class}
\alias{qda-class}
\alias{qualScore-class}
\alias{randomForest-class}
\alias{rpart-class}
\alias{silhouetteVec-class}
\alias{slda-class}
\alias{SOMBout-class}
\alias{svm-class}
\alias{show,classifOutput-method}
\alias{silhouetteB,classifOutput-method}

\title{Class "classifOutput": container for output of
classification procedures in R }
\description{  container with uniform contents for classifier outputs }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("classifOutput", ...)}.
These objects include label vectors for test set elements, scores on labeling
events, call history information, native classifier object access,
and a distance matrix for test set feature vectors.
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
\author{ VJ Carey <stvjc@channing.harvard.edu> }

\seealso{
}
\examples{
}
\keyword{classes}
