\name{balKfold.xvspec}
\alias{balKfold.xvspec}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ generate a partition function for cross-validation,
where the partitions are approximately balanced with respect to
the distribution of a response variable}
\description{generate a partition function for cross-validation,
where the partitions are approximately balanced with respect to
the distribution of a response variable}
\usage{
balKfold.xvspec(K)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{K}{ number of partitions to be computed }
}
\details{
This function returns a closure.  The symbol \code{K} is
bound in the environment of the returned function.
}
\value{
A closure consisting of a function that can be used
as a \code{partitionFunc} for passage in \code{\link{xvalSpec}}.
}
%\references{ ~put references to the literature/web site here ~ }
\author{ VJ Carey <stvjc@channing.harvard.edu> }
%\note{ ~~further notes~~ 

% ~Make other sections like Warning with \section{Warning }{....} ~
%}
%\seealso{ ~~objects to See Also as \code{\link{help}}, ~~~ }
\examples{
## The function is currently defined as
function (K) 
function(data, clab, iternum) {
    clabs <- data[[clab]]
    narr <- nrow(data)
    cnames <- unique(clabs)
    ilist <- list()
    for (i in 1:length(cnames)) ilist[[cnames[i]]] <- which(clabs == 
        cnames[i])
    clens <- lapply(ilist, length)
    nrep <- lapply(clens, function(x) ceiling(x/K))
    grpinds <- list()
    for (i in 1:length(nrep)) grpinds[[i]] <- rep(1:K, nrep[[i]])[1:clens[[i]]]
    (1:narr)[-which(unlist(grpinds) == iternum)]
  }
# try it out
data(crabs)
p1c = balKfold.xvspec(5)
inds = p1c( crabs, "sp", 3 )
table(crabs$sp[inds] )
inds2 = p1c( crabs, "sp", 4 )
table(crabs$sp[inds2] )
allc = 1:200
# are test sets disjoint?
intersect(setdiff(allc,inds), setdiff(allc,inds2))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
\keyword{ manip }
