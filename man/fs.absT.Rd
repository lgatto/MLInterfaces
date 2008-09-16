\name{fs.absT}
\alias{fs.absT}
\alias{fs.probT}
\alias{fs.topVariance}
\title{ support for feature selection in cross-validation}
\description{ support for feature selection in cross-validation}
\usage{
fs.absT(N)
fs.probT(p)
fs.topVariance(p)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{N}{ number of features to retain; features are ordered
by descending value of abs(two-sample t stat.), and the top N are
used. }
  \item{p}{ cumulative probability (in (0,1)) in the distribution of absolute t statistics
above which we retain features}
}
\details{
This function returns a function that will be used as a parameter
to \code{\link{xvalSpec}} in applications of \code{\link{MLearn}}.
}
\value{
a function is returned, that will itself return a formula
consisting of the selected features for application of \code{\link{MLearn}}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ VJ Carey <stvjc@channing.harvard.edu> }
\note{ The functions \code{fs.absT} and \code{fs.probT} are
two examples of approaches to embedded feature selection that make
sense for two-sample prediction problems.  For selection based on
linear models or other discrimination measures, you will need to create
your own selection helper, following the code in these functions as
examples.

fs.topVariance performs non-specific feature selection based on the 
variance.  Argument p is the variance percentile beneath which features are
discarded.
}
\seealso{ \code{\link{MLearn}} }
\examples{
# we will demonstrate this procedure with the crabs data.
# first, create the closure to pick 3 features
demFS = fs.absT(3)
# run it on the entire dataset with features excluding sex
demFS(sp~.-sex, crabs)
# emulate cross-validation by excluding last 50 records
demFS(sp~.-sex, crabs[1:150,])
# emulate cross-validation by excluding first 50 records -- different features retained
demFS(sp~.-sex, crabs[51:200,])
}
\keyword{ models }% __ONLY ONE__ keyword per line
