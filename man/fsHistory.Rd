\name{fsHistory}
\alias{fsHistory}
\title{extract history of feature selection for a cross-validated
machine learner  }
\description{extract history of feature selection for a cross-validated
machine learner  }
\usage{
fsHistory(x)
}
\arguments{
  \item{x}{ instance of \code{\linkS4class{classifierOutput}}}
}
\details{
returns a list of names of selected features
}
\value{
a list; the names of variables are made 'syntactic'
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(iris)
iris2 = iris[ iris$Species \%in\% levels(iris$Species)[1:2], ]
iris2$Species = factor(iris2$Species) # drop unused levels
x1 = MLearn(Species~., iris2, ldaI, xvalSpec("LOG", 3, 
   balKfold.xvspec(3), fs.absT(3)))
fsHistory(x1)
}
\keyword{ models }
