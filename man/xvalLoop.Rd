\name{xvalLoop}
\alias{xvalLoop}
\title{Cross-validation in clustered computing environments}
\description{Use cross-validation in a clustered computing environment}
\usage{xvalLoop( cluster, ... )}
\arguments{
 \item{cluster}{Any S4-class object, used to indicate how to perform clustered
   computations.}
 \item{...}{Additional arguments used to inform the clustered computation.}
}
\details{
  Cross-validiation usually involves repeated calls to the same
  function, but with different arguments. This provides an obvious
  place for using clustered computers to enhance execution. The method
  \code{\link{xval}} is structured to exploit this; \code{xvalLoop}
  provides an easy mechanism to change how \code{xval} performs
  cross-validation.

  The idea is to write an \code{xvalLoop} method that returns a
  function. The function is then used to execute the
  cross-validation. For instance, the default method returns the
  function \code{lapply}, so the cross-validation is performed by using
  \code{lapply}. A different method might return a function that
  executed lapply-like functions, but sent different parts of the
  function to different computer nodes.

  An accompanying vignette illustrates the technique in greater
  detail. An effective division of labor is for experienced cluster
  programmers to write lapply-like methods for their favored clustering
  environment. The user then only has to add the cluster object to the
  list of arguments to \code{xval} to get clustered calculations.
}
\value{A function taking arguments like those for \code{\link{lapply}}}
\examples{
\dontrun{
library(golubEsets)
data(golubMerge)
smallG <- golubMerge[200:250,]

# Evaluation on one node

lk1 <- xval(smallG, "ALL.AML", knnB, xvalMethod="LOO", group=as.integer(0))
table(lk1,smallG$ALL.AML)

# Evaluation on several nodes -- a cluster programmer might write the following...

library(snow)
setOldClass("spawnedMPIcluster")

setMethod("xvalLoop", signature( cluster = "spawnedMPIcluster"),
## use the function returned below to evalutae
## the central cross-validation loop in xval
function( cluster, ... ) {
    clusterExportEnv <- function (cl, env = .GlobalEnv)
    {
        unpackEnv <- function(env) {
            for ( name in ls(env) ) assign(name, get(name, env), .GlobalEnv )
            NULL
        }
        clusterCall(cl, unpackEnv, env)
    }
    function(X, FUN, ...) { # this gets returned to xval
        ## send all visible variables from the parent (i.e., xval) frame
        clusterExportEnv( cluster, parent.frame(1) )
        parLapply( cluster, X, FUN, ... )
    }
})

# ... and use the cluster like this...

cl <- makeCluster(2, "MPI")
clusterEvalQ(cl, library(MLInterfaces))

lk1 <- xval(smallG, "ALL.AML", knnB, xvalMethod="LOO", group=as.integer(0), cluster = cl)
table(lk1,smallG$ALL.AML)
}}
\keyword{methods}
