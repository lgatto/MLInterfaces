\name{performance-analytics}
\docType{methods}
\alias{precision-methods}
\alias{precision,classifierOutput,character-method}
\alias{precision,classifierOutput,missing-method}
\alias{precision,classifierOutput,numeric-method}
\alias{precision,table,missing-method}
\alias{precision}
\alias{recall-methods}
\alias{recall,classifierOutput,character-method}
\alias{recall,classifierOutput,missing-method}
\alias{recall,classifierOutput,numeric-method}
\alias{recall,table,missing-method}
\alias{recall}
\alias{sensitivity-methods}
\alias{sensitivity,classifierOutput,character-method}
\alias{sensitivity,classifierOutput,missing-method}
\alias{sensitivity,classifierOutput,numeric-method}
\alias{sensitivity,table,missing-method}
\alias{sensitivity}
\alias{macroF1-methods}
\alias{macroF1,classifierOutput,character-method}
\alias{macroF1,classifierOutput,missing-method}
\alias{macroF1,classifierOutput,numeric-method}
\alias{macroF1,table,missing-method}
\alias{macroF1,numeric,numeric-method}
\alias{macroF1}
\alias{acc,table-method}
\alias{acc}
\alias{specificity,table-method}
\alias{specificity}
\alias{tp,table-method}
\alias{tp}
\alias{tn,table-method}
\alias{tn}
\alias{fp,table-method}
\alias{fp}
\alias{fn,table-method}
\alias{fn}
\alias{F1,table-method}
\alias{F1}

\title{ Assessing classifier performance }

\description{
  Methods to calculate the number of true positives (\code{tp}),
  true negatives (\code{tn}), false negatives (\code{fn}),
  false positive (\code{fp}), accuracy (\code{acc}),  
  \code{precision}, \code{recall} (same as \code{sensitivity}),
  \code{specificity}, \code{F1} and \code{macroF1} scores.

  Each method also accepts an \code{naAs0} argument definiting if
  \code{NAs} should be replaced by \code{0} (default is \code{FALSE}).
}

\section{Methods}{
  Methods \code{tp}, \code{tn}, \code{fp}, \code{fn}, \code{F1},
  \code{acc} and \code{specificity}:
  \describe{
    \item{\code{signature(obj = "table")}}{ }
  }
  Methods \code{recall} (\code{sensitivity}), \code{precision} and
  \code{macroF1}: 
  \describe{
    \item{\code{signature(obj = "classifierOutput", type = "character")}}{
      %%  ~~describe this method here~~
    }
    \item{\code{signature(obj = "classifierOutput", type = "missing")}}{
      %%  ~~describe this method here~~
    }
    \item{\code{signature(obj = "classifierOutput", type = "numeric")}}{
      %%  ~~describe this method here~~
    }
    \item{\code{signature(obj = "table")}}{ }
  }
}

\examples{
## the confusion matrix
cm <- table(iris$Species, sample(iris$Species))
tp(cm)
tn(cm)
fp(cm)
fn(cm)
acc(cm)
precision(cm)
recall(cm)
F1(cm)
macroF1(cm)
}

\keyword{methods}
