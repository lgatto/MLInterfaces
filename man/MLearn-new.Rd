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
\alias{sldaI}
\alias{knn2}
\alias{ldaI.predParms}
\alias{lvq}
\alias{rab}
\alias{MLearn,formula,ExpressionSet,character,numeric,missing-method}
\alias{MLearn,formula,ExpressionSet,learnerSchema,numeric,missing-method}
\alias{MLearn,formula,data.frame,learnerSchema,numeric,missing-method}
\alias{xvalSpec}
\alias{xvalSpec-class}
\alias{MLearn,formula,data.frame,learnerSchema,xvalSpec,missing-method}
\alias{MLearn,formula,ExpressionSet,learnerSchema,xvalSpec,missing-method}
\alias{plotXvalRDA}
\alias{rdacvI}
\alias{rdacvML}


\title{revised MLearn interface for machine learning}
\description{revised MLearn interface for machine learning, emphasizing
a schematic description of external learning functions like knn, lda, nnet, etc.
}
\usage{
MLearn( formula, data, method, trainInd, mlSpecials, ... )
xvalSpec( type, niter=0, partitionFunc= function(data, classLab, iternum ) {
				(1:nrow(data))[-iternum] },
	                 fsFun = function(formula, data) formula )
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
  \item{type}{"LOO" to specify leave-one-out cross-validation; any
other token implies use of a \code{partitionFunc}}
  \item{niter}{numeric, specifying number of cross-validation iterations,
i.e., the number of partitions to be formed; ignored if \code{type} is "LOO".}
  \item{partitionFunc}{function, with parameters data (bound
to data.frame), clab (bound to character string), iternum (bound
to numeric index into sequence of 1:\code{niter}).  This function's job
is to provide the indices of training cases for each cross-validation
step.  An example is \code{\link{balKfold.xvspec}}, which computes
a series of indices that are approximately balanced with respect
to frequency of outcome types.}
  \item{fsFun}{function, with parameters formula, data.  The function
must return a formula suitable for defining a model on the basis of the
main input data.  A candidate fsFun  is given in examples below.}
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
\item{sldaI}{\link[ipred]{slda}}
\item{rdacvI}{\link[rda]{rda.cv}.  This interface is complicated.  The typical
use includes cross-validation internal to the rda.cv function.  That process
searches a tuning parameter space and delivers an ordering on parameters.
The interface selects the parameters by looking at all parameter
configurations achieving the smallest min+1SE cv.error estimate, and taking
the one among them that employed the -most- features (agnosticism).
A final run of rda is then conducted with the tuning parameters set at
that 'optimal' choice.  The bridge code can be modified to facilitate
alternative choices of the parameters in use.  \code{plotXvalRDA}
is an interface to the plot method for objects of class rdacv defined in
package rda.}
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
slda1 = MLearn(sp~CW+RW, data=crabs, sldaI, kp )
slda1
names(RObject(slda1))
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
#
# illustrate feature selection -- following function keeps features that
# discriminate in the top 25 percent of all features according to rowttests
#
fsFun.rowtQ3 = function(formula, data) {
 # facilitation of a rowttests with a formula/data.frame takes a little work
 mf = model.frame(formula, data)
 mm = model.matrix(formula, data)
 respind = attr( terms(formula, data=data), "response" )
 x = mm
 if ("(Intercept)" \%in\% colnames(x)) x = x[,-which(colnames(x) == "(Intercept)")]
 y = mf[, respind]
 respname = names(mf)[respind]
 nuy = length(unique(y))
 if (nuy > 2) warning("number of unique values of response exceeds 2")
 #dm = t(data.matrix(x))
 #dm = matrix(as.double(dm), nr=nrow(dm)) # rowttests seems fussy
 ans = abs( rowttests(t(x), factor(y), tstatOnly=TRUE)[[1]] )
 names(ans) = colnames(x)
 ans = names( ans[ which(ans > quantile(ans, .75) ) ] )
 btick = function(x) paste("`", x, "`", sep="")  # support for nonsyntactic varnames
 as.formula( paste(respname, paste(btick(ans), collapse="+"), sep="~"))
}

nn3cv = MLearn(sp~CW+RW+CL+BD+FL, data=crabs[c(1:20,101:120),], nnetI, 
   xvalSpec("LOG",5, balKfold.xvspec(5), fsFun=fsFun.rowtQ3), size=3, decay=.01 )
confuMat(nn3cv)
nn4cv = MLearn(sp~.-index-sex, data=crabs[c(1:20,101:120),], nnetI, 
   xvalSpec("LOG",5, balKfold.xvspec(5), fsFun=fsFun.rowtQ3), size=3, decay=.01 )
confuMat(nn4cv)
#
# try with expression data
#
library(golubEsets)
data(Golub_Train)
litg = Golub_Train[ 100:150, ]
g1 = MLearn(ALL.AML~. , litg, nnetI, xvalSpec("LOG",5, balKfold.xvspec(5), fsFun=fsFun.rowtQ3), size=3, decay=.01 )
confuMat(g1)
#
# illustrate rda.cv interface from package rda (requiring local bridge)
#
library(ALL)
data(ALL)
#
# restrict to BCR/ABL or NEG
#
bio <- which( ALL$mol.biol \%in\% c("BCR/ABL", "NEG"))
#
# restrict to B-cell
#
isb <- grep("^B", as.character(ALL$BT))
kp <- intersect(bio,isb)
all2 <- ALL[,kp]
mads = apply(exprs(all2),1,mad)
kp = which(mads>1)  # get around 250 genes
vall2 = all2[kp, ]
vall2$mol.biol = factor(vall2$mol.biol) # drop unused levels

r1 = MLearn(mol.biol~., vall2, rdacvI, 1:40)
confuMat(r1)
RObject(r1)
plotXvalRDA(r1)  # special interface to plots of parameter space

}

\keyword{ models }
