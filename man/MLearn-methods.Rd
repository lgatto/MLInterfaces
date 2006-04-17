\name{MLearn-methods}
\docType{methods}
\alias{MLearn-methods}
\alias{MLearn}
\alias{MLearn,character,exprSet,character,numeric-method}
\alias{MLearn,formula,data.frame,character,numeric-method}
\title{ unified interface to machine learning methods }
\description{
 unified interface to machine learning methods -- new approach (August 2005)
}
\section{Introduction}{
Use of \code{MLInterfaces} methods to date (version 1.1.3) involves a large
number of generics with names indicating the method to be employed.
For example \code{knnB()} is used to apply $k$-nearest neighbors analysis
to an instance of the \code{\link[Biobase]{exprSet}} class.  In this design, the generic
has to ``know'' about the parameters to the underlying R function implementing
the method of interest, and set defaults.  This is a somewhat fragile design,
in that changes to the calling sequences to underlying R functions can break the
interfaces defined here.

A new, fully backwards-compatible design is now being introduced.  Here there
is one generic \code{MLearn}.  Its parameters are \code{formula}, \code{data},
\code{method}, and \code{trainInd}, and additional parameters to underlying
implementations of machine learning algorithms are passed through \dots{}.
This new design allows use of ordinary formulas and data frames as well as
\code{\link[Biobase]{exprSet}} instances.

The machine learning methods accommodated in the new design are described
before the examples below.

}
\section{Methods}{
\describe{
\item{formula = "formula", data = "data.frame", method = "character", trainInd = "numeric"}{ 
The behavior with this signature is comparable to
that of the standard R modeling tools, with the exception of the handling 
of the common \code{subset} parameter.  Because \code{MLInterfaces} wishes to
inhibit the use of resubstitution estimates of generalization error, all \code{MLInterfaces}
procedures impose the requirement of the decomposition of input data into training
and test subsets.  If you want the behavior of a \code{subset} parameter setting,
please form the subset manually prior to invoking \code{MLearn}.

  Possible values for \code{method} are described below, under ``Machine learning resources available''.

Parameter \code{trainInd} defines the indices of the records in the input dataset that are
used for training; remaining records are used as a test dataset for evaluation of the fitted
learner.
}

\item{formula = "character", data = "exprSet", method = "character", trainInd = "numeric"}{ 
  This method works for instances of the \code{\link[Biobase]{exprSet}} class.

Parameter \code{formula} is to be the name of a variable in the \code{pData}
slot of the exprSet's \code{\link[Biobase]{phenoData}}.  In general this will be a factor
encoding a categorical variable.

Parameter \code{data} is to be an instance of class \code{\link[Biobase]{exprSet}}.

  Possible values for \code{method} are described below, under ``Machine learning resources available''.

Parameter \code{trainInd} defines the indices of the records in the input dataset that are
used for training; remaining records are used as a test dataset for evaluation of the fitted
learner.
}

Any additional parameters to be set for \code{method} can be passed in after \code{trainInd}.
For example, if \code{``nnet''} is supplied as \code{method}, the parameter \code{size} must
be set and passed in.}

}
\section{Value}{An instance of class \code{\link{MLOutput-class}}.}
}
\section{Machine learning resources available}{
Here we provide links to tools that may be identified in the \code{method} parameter.  Just
use a string naming the method.  For each method, we may have a ``Do not pass parameters''
clause, because the interface constructs values of these parameters on the basis of
parameters set in the call to \code{MLearn}.  You may (and in some cases must) set
and pass parameters not listed in the ``Do not pass'' list.

\describe{
\item{\code{\link[class]{knn}}}{k-nearest neighbors.  Do not pass parameters \code{train}, \code{test}, or \code{cl};
any other parameters defined in the calling sequence can be passed.}
\item{\code{\link[nnet]{nnet}}}{neural nets.  Do not pass parameters \code{x}, \code{y}, \code{data} or \code{subset}.}
\item{\code{\link[randomForest]{randomForest}}}{random forests.  Do not pass parameters \code{x}, \code{y}, \code{xtest} or \code{ytest}.}
}}
\examples{
data(iris)
tinds <- sample(1:150, 45)
MLearn(Species~., data=iris, method="nnet", tinds, size=4, decay=.01 )
MLearn(Species~., data=iris, method="knn", tinds )
rfdemo <- MLearn(Species~., data=iris, method="randomForest", tinds, importance=TRUE )
opar <- par(no.readonly=TRUE)
par(las=2,mar=c(5,8,5,5))
plot(getVarImp(rfdemo))
par(opar)
# genomics examples
library(golubEsets)
data(golubMerge)
MLearn("ALL.AML", golubMerge[1:50,], "rpart", 1:36 )
MLearn("ALL.AML", golubMerge[1:50,], "knn", 1:36 )
}
\keyword{methods}
