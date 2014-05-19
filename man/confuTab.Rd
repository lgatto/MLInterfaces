\name{confuTab}
\alias{confuTab}


\title{
  Compute confusion tables for a confusion matrix.
}

\description{
  Given a \code{n} by \code{n} confusion matrix, the function returns a
  \code{list} of \code{n} 2 by 2 tables with false positives, false
  negatives, false positives and true negative for each initial variables.
}

\usage{
confuTab(obj)
}


\arguments{
  \item{obj}{
    An instance of class \code{table}. Must be square.
  }
}

\value{
  A \code{list} of length \code{nrow(obj)} and names
  \code{rownames(obj)}. 
}

\author{
  Laurent Gatto <lg390@cam.ac.uk>
}

\seealso{
  The \code{\link{tp}}, \code{\link{tn}}, \code{\link{fp}},
  \code{\link{fn}}, methods to extract the respective classification
  outcomes from a contingency matrix.
}

\examples{
## the confusion matrix
cm <- table(iris$Species, sample(iris$Species))
## the 3 confusion tables
(ct <- confuTab(cm))
}
