\name{RAB}
\alias{RAB}
\alias{RAB4es}
\alias{DAB}
\alias{Predict}
\alias{tonp}
\alias{mkfmla}
\alias{Predict,raboostCont-method}
\alias{Predict,daboostCont-method}
\title{real adaboost (Friedman et al) (and discrete adaboost (DAB))  }
\description{
}
\usage{
RAB(formula, data, maxiter=200, maxdepth=1)
}
\arguments{
  \item{formula}{ formula -- the response variable must be coded -1, 1}
  \item{data}{ data}
  \item{maxiter}{ maxiter}
  \item{maxdepth}{ maxdepth -- passed to rpart}
}
%\details{
%}
\value{
an instance of raboostCont
}
\references{Friedman et al Ann Stat 28/2 337  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
library(MASS)
data(Pima.tr)
data(Pima.te)
Pima.all = rbind(Pima.tr, Pima.te)
tonp = ifelse(Pima.all$type == "Yes", 1, -1)
tonp = factor(tonp)
Pima.all = data.frame(Pima.all[,1:7], mtype=tonp)
fit1 = RAB(mtype~ped+glu+npreg+bmi+age, data=Pima.all[1:200,], maxiter=10, maxdepth=5)
pfit1 = Predict(fit1, newdata=Pima.tr)
table(Pima.tr$type, pfit1)
}
\keyword{ models }
