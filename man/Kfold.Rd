\name{Kfold}
\alias{Kfold}
\title{for given n, generate a K-fold partition of 1:n}
\description{for given n, generate a K-fold partition of 1:n}
\usage{
Kfold(n, K=5)
}
\arguments{
  \item{n}{ number of elements to be partitioned }
  \item{K}{ number of partitions}
}
%\details{
%}
\value{
a vector of length n with elements 1:K, as balanced as possible, sorted
increasing
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
Kfold(23)
}
\keyword{ models }
