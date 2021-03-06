\name{varImpStruct-class}
\docType{class}
\alias{varImpStruct-class}
\alias{plot}
\alias{plot,varImpStruct-method}
\alias{plot,varImpStruct,ANY-method}
\alias{show,varImpStruct-method}
\alias{report,varImpStruct-method}
\alias{report}
\alias{getVarImp}
\alias{getVarImp,classifOutput,logical-method}
\alias{getVarImp,classifierOutput,logical-method}
\alias{getVarImp,classifierOutput,missing-method}
\title{Class "varImpStruct" -- collect data on variable importance
 from various machine learning methods}
\description{  collects data on variable importance }
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("varImpStruct", ...)}.
These are matrices of importance measures with separate 
slots identifying algorithm generating the measures and
variable names.
}
\section{Slots}{
  \describe{
    \item{\code{.Data}:}{Object of class \code{"matrix"} actual importance
measures }
    \item{\code{method}:}{Object of class \code{"character"} tag }
    \item{\code{varnames}:}{Object of class \code{"character"} conformant
vector of names of variables }
  }
}
\section{Extends}{
Class \code{"matrix"}, from data part.
Class \code{"structure"}, by class \code{"matrix"}.
Class \code{"array"}, by class \code{"matrix"}.
Class \code{"vector"}, by class "matrix", with explicit coerce.
Class \code{"vector"}, by class "matrix", with explicit coerce.
}
\section{Methods}{
  \describe{
    \item{plot}{\code{signature(x = "varImpStruct")}: make a bar plot,
you can supply arguments \code{plat}
and \code{toktype} which will use \code{lookUp(...,plat,toktype)}
from the \code{annotate} package to translate probe names to, e.g.,
gene symbols.}
    \item{show}{\code{signature(object = "varImpStruct")}: simple abbreviated
display }
    \item{getVarImp}{\code{signature(object = "classifOutput", fixNames="logical")}: extractor
of variable importance structure; fixNames parameter is to remove leading X used
to make variable names syntactic by randomForest (ca 1/2008).  You can set fixNames to false
if using hu6800 platform, because all featureNames are syntactic as given.}
    \item{report}{\code{signature(object = "classifOutput", fixNames="logical")}: extractor
of variable importance data, with annotation; fixNames parameter is to remove leading X used
to make variable names syntactic by randomForest (ca 1/2008).  You can set fixNames to false
if using hu6800 platform, because all featureNames are syntactic as given.}
  }
}
%\references{ ~put references to the literature/web site here ~ }
%\author{ ~~who you are~~ }
%\note{ ~~further notes~~ }
%
% ~Make other sections like Warning with \section{Warning }{....} ~
%
%\seealso{
%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%  or \code{\link{CLASSNAME-class}} for links to other classes
%}
\examples{
library(golubEsets)
data(Golub_Merge)
library(hu6800.db)
smallG <- Golub_Merge[1001:1060,]
set.seed(1234)
opar=par(no.readonly=TRUE)
par(las=2, mar=c(10,11,5,5))
rf2 <- MLearn(ALL.AML~., smallG, randomForestI, 1:40, importance=TRUE,
 sampsize=table(smallG$ALL.AML[1:40]), mtry=sqrt(ncol(exprs(smallG))))
plot( getVarImp( rf2, FALSE ), n=10, plat="hu6800", toktype="SYMBOL")
par(opar)
report( getVarImp( rf2, FALSE ), n=10, plat="hu6800", toktype="SYMBOL")
}
\keyword{classes}
