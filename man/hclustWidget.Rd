\name{hclustWidget}
\alias{hclustWidget}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
shiny-oriented GUI for cluster exploration
}
\description{
shiny-oriented GUI for cluster exploration
}
\usage{
hclustWidget(mat, featureName = "feature", minfeats = 2, auxdf = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mat}{
matrix with feature vectors in rows
}
  \item{featureName}{
name to be used for control that asks for number of features to use
}
  \item{minfeats}{
lower bound on number of features to use
}
  \item{auxdf}{
data.frame with number of rows equal to nrow(mat), with metadata
to be displayed in hovering tooltip
}
}
\details{
Experimental tool to illustrate impacts of choice of distance,
agglomeration method, etc.
}
\value{
a shinyApp result that will display in active browser
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
VJ Carey <stvjc@channing.harvard.edu>
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
# should run with example(hclustWidget, ask=FALSE)
if (interactive()) {
 library(shiny)
 library(MASS)
 data(crabs)
 cr = data.matrix(crabs[,-c(1:3)])
 au = crabs[,1:3]
 show(hclustWidget(cr, auxdf=au))
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ models }
