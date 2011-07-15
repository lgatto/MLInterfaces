\name{xvalSpec}
\alias{xvalSpec}
\alias{xvalSpec-class}

\title{container for information specifying a cross-validated
machine learning exercise}
\description{container for information specifying a cross-validated
machine learning exercise}
\usage{
xvalSpec( type, niter=0, partitionFunc=function(data, classLab, iternum ) {
				(1:nrow(data))[-iternum] },
	                 fsFun = function(formula, data) formula )
}
\arguments{

  \item{type}{a string, "LOO" indicating leave-one-out
cross-validation, or "LOG" indicating leave-out-group, or
"NOTEST", indicating the entire dataset is used in a single
training run.}

  \item{niter}{numeric specification of the number of
cross-validation iterations to use.  Ignored if \code{type}
is \code{"LOO"}.}
  \item{partitionFunc}{function, with parameters data (bound
to data.frame), clab (bound to character string), iternum (bound
to numeric index into sequence of 1:\code{niter}).  This function's job
is to provide the indices of training cases for each cross-validation
step.  An example is \code{\link{balKfold.xvspec}}, which computes
a series of indices that are approximately balanced with respect
to frequency of outcome types.}
  \item{fsFun}{function, with parameters formula, data.  The function
must return a formula suitable for defining a model on the basis of the
main input data.  A candidate fsFun  is given in example for fsHistory function.}
}
\details{
If \code{type == "LOO"}, no other parameters are inspected.
If \code{type == "LOG"} a value for \code{partitionFunc} must be
supplied.  We recommend using \code{balKfold.xvspec(K)}.  The
values of \code{niter} and \code{K} in this usage must be the same.
This redundancy will be removed in a future upgrade.

If the \code{multicore} package is attached, cross-validation will
be distributed to cores using \code{\link[multicore]{mclapply}}.
} % end details
\value{
An instance of \code{\linkS4class{classifierOutput}}, with a special
structure.  The \code{RObject} return slot is populated with a
list of \code{niter} cross-validation results.  Each element of this list
is itself a list with two elements: \code{test.idx} (the indices
of the test set for the associated cross-validation  iteration,
and \code{mlans}, the \code{\linkS4class{classifierOutput}} generated at
each iteration.  Thus there are \code{classifierOutput}
instances nested within the main \code{classifierOutput} returned
when a \code{xvalSpec} is used.
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(crabs)
set.seed(1234)
#
# demonstrate cross validation
#
nn1cv = MLearn(sp~CW+RW, data=crabs, nnetI, xvalSpec("LOG",
   5, balKfold.xvspec(5)), size=3, decay=.01 )
nn1cv
confuMat(nn1cv)
names(RObject(nn1cv)[[1]])
RObject(RObject(nn1cv)[[1]]$mlans)
}

\keyword{ models }
