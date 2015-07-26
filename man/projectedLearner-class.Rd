\name{projectedLearner-class}
\Rdversion{1.1}
\docType{class}
\alias{projectedLearner-class}
\alias{learnerIn3D,projectedLearner-method}
\alias{plot,projectedLearner,ANY-method}
\alias{plotOne,projectedLearner-method}
\alias{show,projectedLearner-method}
\alias{plotOne}
\alias{learnerIn3D}

\title{Class \code{"projectedLearner"}}
\description{
helps depict prediction hyperregions from high-dimensional models
}
\section{Objects from the Class}{
Objects can be created by calls of the form \code{new("projectedLearner", ...)}.
}
\section{Slots}{
  \describe{
    \item{\code{fittedLearner}:}{Object of class \code{"classifierOutput"}  }
    \item{\code{trainingSetPCA}:}{Object of class \code{"prcomp"} }
    \item{\code{trainingLabels}:}{Object of class \code{"ANY"} given
labels for features used in training }
    \item{\code{testLabels}:}{Object of class \code{"ANY"} given labels
for features used in testing }
    \item{\code{gridFeatsProjectedToTrainingPCs}:}{Object of class \code{"matrix"} rotated coordinates of training features }
    \item{\code{gridPredictions}:}{Object of class \code{"ANY"} predicted
labels for all grid points }
    \item{\code{trainFeatsProjectedToTrainingPCs}:}{Object of class \code{"matrix"} rotated coordinates of training features }
    \item{\code{testFeatsProjectedToTrainingPCs}:}{Object of class \code{"matrix"} rotated coordinates of test features }
    \item{\code{trainPredictions}:}{Object of class \code{"ANY"} predicted
labels for training features }
    \item{\code{testPredictions}:}{Object of class \code{"ANY"} predicted
labels for test features }
    \item{\code{theCall}:}{Object of class \code{"call"} call used to
generate this wonderful thing }
  }
}
\section{Methods}{
  \describe{
    \item{learnerIn3D}{\code{signature(x = "projectedLearner")}: uses rgl
to give a dynamic 3d-like projection of labels in colored regions }
    \item{plot}{\code{signature(x = "projectedLearner", y = "ANY")}: pairs
plot of the tesselated PCA of the training features }
    \item{plotOne}{\code{signature(x = "projectedLearner")}: a 2d plot
of tesselation projection for selected axes of the PCA}
    \item{show}{\code{signature(object = "projectedLearner")}: object
housing numerical resources for the renderings  }
	 }
}
\references{
None.
}
\author{
VJ Carey <stvjc@channing.harvard.edu>
}
\note{
\code{plot} may need to be modified when there are many features/PCs in use

\code{plotOne} has additional arguments \code{ind1}, \code{ind2}, and \code{type}.  ind1 and ind2 specify the PCs to display.  type is one of 
\code{"showTestPredictions"} (default),
\code{"showTrainPredictions"},  
\code{"showTestLabels"},  
\code{"showTrainLabels"}.  These indicate what will be used to locate
glyphs with labels in the projected scatterplots.

}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%%  ~~objects to See Also as \code{\link{~~fun~~}}, ~~~
%%  ~~or \code{\linkS4class{CLASSNAME}} for links to other classes ~~~
%}
\examples{
showClass("projectedLearner")
}
\keyword{classes}
