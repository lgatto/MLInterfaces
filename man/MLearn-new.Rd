\name{MLearn_new}
\alias{MLearn_new}
\alias{MLearn}
\alias{baggingI}
\alias{dlda}
\alias{glmI.logistic} 
\alias{knnI} 
\alias{ldaI} 
\alias{lvqI} 
\alias{naiveBayesI} 
\alias{nnetI}
\alias{qdaI} 
\alias{RABI}
\alias{randomForestI} 
\alias{rpartI}
\alias{svmI}
\alias{dlda2}
\alias{dldaI}
\alias{knn2}
\alias{ldaI.predParms}
\alias{lvq}
\alias{rab}
\alias{MLearn,formula,ExpressionSet,character,numeric,missing-method}
\alias{MLearn,formula,ExpressionSet,learnerSchema,numeric,missing-method}
\alias{MLearn,formula,data.frame,learnerSchema,numeric,missing-method}
\alias{xvalSpec}
\alias{xvalSpec-class}
\alias{balKfold.xvspec}
\alias{MLearn,formula,data.frame,learnerSchema,xvalSpec,missing-method}
\alias{MLearn,formula,ExpressionSet,learnerSchema,xvalSpec,missing-method}


\title{revised MLearn interface for machine learning}
\description{revised MLearn interface for machine learning, emphasizing
a schematic description of external learning functions like knn, lda, nnet, etc.
}
\usage{
MLearn( formula, data, method, trainInd, mlSpecials, ... )
}
\arguments{
  \item{formula}{ standard model formula }
  \item{data}{ data.frame or ExpressionSet instance}
  \item{method}{ instance of learnerSchema }
  \item{trainInd}{ obligatory numeric vector of indices of data to be used for training;
all other data are used for testing, or instance of the xvalSpec class }
  \item{mlSpecials}{ see help(MLearn-OLD) for this parameter; learnerSchema design
obviates need for this parameter, which is retained only for back-compatibility.}
  \item{\dots}{ additional named arguments passed to external learning function }
}
\details{
This implementation attempts to reduce complexity of the basic
MLInterfaces engine.  The primary MLearn method, which includes 
"learnerSchema" in its signature, is very concise.  Details
of massaging inputs and outputs are left to a learnerSchema class instance.
The MLint\_devel vignette describes the schema formulation.  learnerSchema instances
are provided for following methods; the naming convention is that the
method basename is prepended to `I'.

Note that some schema instances are presented as functions.  The
parameters must be set to use these models.

To obtain documentation on the older (pre bioc 2.1) version of the MLearn
method, please use help(MLearn-OLD).
\describe{
\item{randomForestI}{\link[randomForest]{randomForest}.  Note, that to obtain the
default performance of randomForestB, you need to set mtry and sampsize parameters
to sqrt(number of features) and table([training set response factor]) respectively,
as these were not taken to be the function's defaults.}
\item{knnI(k=1,l=0)}{\link[class]{knn}; special support bridge required, defined in MLint}
\item{dldaI}{\link[sma]{stat.diag.da}; special support bridge required, defined in MLint}
\item{nnetI}{\link[nnet]{nnet}}
\item{rpartI}{\link[rpart]{rpart}}
\item{ldaI}{\link[MASS]{lda}}
\item{svmI}{\link[e1071]{svm}}
\item{qdaI}{\link[MASS]{qda}}
\item{logisticI(threshold)}{\link[stats]{glm} -- with binomial family, expecting a dichotomous
factor as response variable, not bulletproofed against other responses yet.  If response
probability estimate exceeds threshold, predict 1, else 0}
\item{RABI}{\link[MLint]{RAB} -- an experimental implementation of real Adaboost
of Friedman Hastie Tibshirani Ann Stat 2001}
\item{lvqI}{\link[class]{lvqtest} after building codebook with lvqinit and updating with
olvq1.  You will need to write your own detailed schema if you want to tweak tuning
parameters.}
\item{naiveBayesI}{\link[e1071]{naiveBayes}}
\item{baggingI}{\link[ipred]{bagging}}
} % end list of schemas
} % end details
\value{
Instances of classifierOutput or clusteringOutput
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


%\seealso{  }

\examples{
data(crabs)
set.seed(1234)
kp = sample(1:200, size=120)
rf1 = MLearn(sp~CW+RW, data=crabs, randomForestI, kp, ntree=600 )
rf1
nn1 = MLearn(sp~CW+RW, data=crabs, nnetI, kp, size=3, decay=.01 )
nn1
RObject(nn1)
knn1 = MLearn(sp~CW+RW, data=crabs, knnI(k=3,l=2), kp)
knn1
names(RObject(knn1))
dlda1 = MLearn(sp~CW+RW, data=crabs, dldaI, kp )
dlda1
names(RObject(dlda1))
lda1 = MLearn(sp~CW+RW, data=crabs, ldaI, kp )
lda1
names(RObject(lda1))
svm1 = MLearn(sp~CW+RW, data=crabs, svmI, kp )
svm1
names(RObject(svm1))
ldapp1 = MLearn(sp~CW+RW, data=crabs, ldaI.predParms(method="debiased"), kp )
ldapp1
names(RObject(ldapp1))
qda1 = MLearn(sp~CW+RW, data=crabs, qdaI, kp )
qda1
names(RObject(qda1))
logi = MLearn(sp~CW+RW, data=crabs, glmI.logistic(threshold=0.5), kp, family=binomial ) # need family
logi
names(RObject(logi))
rp2 = MLearn(sp~CW+RW, data=crabs, rpartI, kp)
rp2
# recode data for RAB
nsp = ifelse(crabs$sp=="O", -1, 1)
nsp = factor(nsp)
ncrabs = cbind(nsp,crabs)
rab1 = MLearn(nsp~CW+RW, data=ncrabs, RABI, kp, maxiter=10)
rab1
lvq.1 = MLearn(sp~CW+RW, data=crabs, lvqI, kp )
lvq.1
nb.1 = MLearn(sp~CW+RW, data=crabs, naiveBayesI, kp )
confuMat(nb.1)
bb.1 = MLearn(sp~CW+RW, data=crabs, baggingI, kp )
confuMat(bb.1)
#
# ExpressionSet illustration
# 
data(sample.ExpressionSet)
X = MLearn(type~., sample.ExpressionSet[100:250,], randomForestI, 1:16, importance=TRUE )
library(randomForest)
varImpPlot(RObject(X))
#
# demonstrate cross validation
#
nn1cv = MLearn(sp~CW+RW, data=crabs[c(1:20,101:120),], nnetI, xvalSpec("LOO"), size=3, decay=.01 )
confuMat(nn1cv)
nn2cv = MLearn(sp~CW+RW, data=crabs[c(1:20,101:120),], nnetI, 
   xvalSpec("LOG",5, balKfold.xvspec(5)), size=3, decay=.01 )
confuMat(nn2cv)
}

\keyword{ models }
