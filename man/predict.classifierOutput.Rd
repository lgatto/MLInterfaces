\name{predict.classifierOutput}
\alias{predict.classifierOutput}


\title{
  Predict method for \code{classifierOutput} objects
}

\description{
  This function predicts values based on models trained with
  MLInterfaces' \code{MLearn} interface to many machine learning
  algorithms. 
}

\usage{
\method{predict}{classifierOutput}(object, newdata, ...)
}

\arguments{
  \item{object}{
    An instance of class \code{\linkS4class{classifierOutput}}.
  }
  \item{newdata}{
    An object containing the new input data: either a \code{matrix}, a
    \code{data.frame} or an \code{ExpressionSet}.
  }
  \item{\dots}{
    Other arguments to be passed to the algorithm-specific predict
    methods. 
  }
}

\details{
  This S3 method will extract the ML model from the
  \code{\linkS4class{classifierOutput}} instance and call either a
  generic predict method or, if available, a specficly written wrapper
  to do classes prediction and class probabilities. 
}

\value{
  Currently, a list with 
  \item{testPredictions}{A factor with class predictions.}
  \item{testScores}{A \code{numeric} or \code{matrix} with class
    probabilities.}
}

\author{
  Laurent Gatto <lg390@cam.ac.uk>
}

\note{
  The function output will most likely be updated in a near future to a
  \code{classifierOutput} (or similar) object.
}

\seealso{
  \code{\link{MLearn}} and \code{\linkS4class{classifierOutput}}.
}

\examples{
data(sample.ExpressionSet)
trainInd <- 1:16

clout.svm <- MLearn(type~., sample.ExpressionSet[100:250,], svmI, trainInd)
predict(clout.svm, sample.ExpressionSet[100:250,-trainInd])

clout.ksvm <- MLearn(type~., sample.ExpressionSet[100:250,], ksvmI, trainInd)
predict(clout.ksvm, sample.ExpressionSet[100:250,-trainInd])

clout.nnet <- MLearn(type~., sample.ExpressionSet[100:250,], nnetI, trainInd, size=3, decay=.01 )
predict(clout.nnet, sample.ExpressionSet[100:250,-trainInd])

clout.knn <- MLearn(type~., sample.ExpressionSet[100:250,], knnI(k=3), trainInd)
predict(clout.knn, sample.ExpressionSet[100:250,-trainInd],k=1)
predict(clout.knn, sample.ExpressionSet[100:250,-trainInd],k=3)

clout.plsda <- MLearn(type~., sample.ExpressionSet[100:250,], plsdaI, trainInd)
predict(clout.plsda, sample.ExpressionSet[100:250,-trainInd])

clout.nb <- MLearn(type~., sample.ExpressionSet[100:250,], naiveBayesI, trainInd)
predict(clout.nb, sample.ExpressionSet[100:250,-trainInd])

clout.rf <- MLearn(type~., sample.ExpressionSet[100:250,], randomForestI, trainInd)
predict(clout.rf, sample.ExpressionSet[100:250,-trainInd])
}

