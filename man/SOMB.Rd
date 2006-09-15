\name{SOMB}
\alias{SOMB}
\alias{somB}
  \alias{somB,ExpressionSet,character-method}
  \alias{SOMB,ExpressionSet,character-method}
  \alias{show,SOMBout-method}
  \alias{show,somout-method}
  \alias{somout-class}


\title{An interface to self-organizing map methods for ExpressionSets }
\description{
Self-organizing maps are covered in various machine learning
reviews.  These can be viewed as clustering procedures
analogous to \code{\link{kmeans}} but with a constraint
on the complexity of the subspace in which the cluster-defining
prototypes are identified.

The outputs of \code{\link[som]{som}} and \code{\link[class]{SOM}} are
not easily formulated as \code{\link{clustOutput-class}} instances,
so classes that extend \code{list} are used as output containers.

SOMB(exprObj, classifLab)
somB(exprObj, classifLab)
}

\arguments{
  \item{exprObj}{An instance of the \code{ExpressionSet} class. }
  \item{classifLab}{A vector of class labels. }
}
\details{
You may pass additional parameters, see \code{\link[class]{SOM}}
or \code{\link[som]{som}} for details.
}
\value{
 An object of class \code{SOMBoutput} or \code{somBoutput}
}

\author{Jess Mar, VJ Carey <stvjc@channing.harvard.edu>}

\examples{
# access and trim an ExpressionSet
library(golubEsets)
data(Golub_Merge)
smallG <- Golub_Merge[1:60,]
# set a PRNG seed for reproducibilitiy
set.seed(1234) # needed for nnet initialization
# now run the classifiers
#somB( smallG, "ALL.AML", 3)
SOMB( smallG, "ALL.AML", 3)
}
\keyword{ cluster }

