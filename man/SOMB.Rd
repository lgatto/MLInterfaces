\name{SOMB}
\alias{SOMB}
\alias{somB}
  \alias{somB,exprSet,character-method}
  \alias{SOMB,exprSet,character-method}
  \alias{show,SOMBout-method}
  \alias{show,somout-method}
  \alias{somout-class}


\title{An interface to self-organizing map methods for exprSets }
\description{
Self-organizing maps are covered in various machine learning
reviews.  These can be viewed as clustering procedures
analogous to \code{\link{kmeans}} but with a constraint
on the complexity of the subspace in which the cluster-defining
prototypes are identified.

The outputs of \code{\link{som}} and \code{\link{SOM}} are
not easily formulated as \code{\link{clustOutput-class}} instances,
so classes that extend \code{list} are used as output containers.

SOMB(exprObj, classifLab)
somB(exprObj, classifLab)
}

\arguments{
  \item{exprObj}{An instance of the \code{exprset} class. }
  \item{classifLab}{A vector of class labels. }
}
\details{
You may pass additional parameters, see \code{\link{SOM}}
or \code{\link{som}} for details.
}
\value{
 An object of class \code{SOMBoutput} or {somBoutput}
}

\author{Jess Mar, VJ Carey <stvjc@channing.harvard.edu>}

\examples{
# access and trim an exprSet
library(golubEsets)
data(golubMerge)
smallG <- golubMerge[1:60,]
# set a PRNG seed for reproducibilitiy
set.seed(1234) # needed for nnet initialization
# now run the classifiers
somB( smallG, "ALL.AML")
SOMB( smallG, "ALL.AML")
}
\keyword{ cluster }

