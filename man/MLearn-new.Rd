\name{MLearn}
\alias{MLearn_new}
\alias{MLearn}
\alias{baggingI}
\alias{dlda}
\alias{glmI.logistic} 
\alias{knnI} 
\alias{knn.cvI} 
\alias{ksvmI} 
\alias{ldaI} 
\alias{lvqI} 
\alias{naiveBayesI} 
\alias{nnetI}
\alias{qdaI} 
\alias{RABI}
\alias{randomForestI} 
\alias{rpartI}
\alias{svmI}
\alias{svm2}
\alias{ksvm2}
\alias{plsda2}
\alias{plsdaI}
\alias{dlda2}
\alias{dldaI}
\alias{sldaI}
\alias{blackboostI}
\alias{knn2}
\alias{knn.cv2}
\alias{ldaI.predParms}
\alias{lvq}
\alias{rab}
\alias{adaI}
\alias{MLearn,formula,ExpressionSet,character,numeric-method}
\alias{MLearn,formula,ExpressionSet,learnerSchema,numeric-method}
\alias{MLearn,formula,data.frame,learnerSchema,numeric-method}
\alias{MLearn,formula,data.frame,learnerSchema,xvalSpec-method}
\alias{MLearn,formula,ExpressionSet,learnerSchema,xvalSpec-method}
\alias{MLearn,formula,data.frame,clusteringSchema,ANY-method}
\alias{plotXvalRDA}
\alias{rdacvI}
\alias{rdaI}
\alias{BgbmI}
\alias{gbm2}
\alias{rdaML}
\alias{rdacvML}
\alias{hclustI}
\alias{kmeansI}
\alias{pamI}
\alias{makeLearnerSchema}
\alias{standardMLIConverter}



\title{revised MLearn interface for machine learning}
\description{revised MLearn interface for machine learning, emphasizing
a schematic description of external learning functions like knn, lda, nnet, etc.
}
\usage{
MLearn( formula, data, .method, trainInd, ... )
makeLearnerSchema(packname, mlfunname, converter, predicter)
}
\arguments{
  \item{formula}{ standard model formula }
  \item{data}{ data.frame or ExpressionSet instance}
  \item{.method}{ instance of learnerSchema }
  \item{trainInd}{ obligatory numeric vector of indices of data to be used for training;
all other data are used for testing, or instance of the xvalSpec class }
  \item{\dots}{ additional named arguments passed to external learning function }
  \item{packname}{character -- name of package harboring a learner function}
  \item{mlfunname}{character -- name of function to use}
  \item{converter}{function -- with parameters (obj, data, trainInd) that tells
    how to convert the material in obj [produced by [packname::mlfunname] ] into a
    classifierOutput instance.}
  \item{predicter}{function -- with parameters (obj, newdata, ...) that
    tells how to use the material in \code{obj} to predict
    \code{newdata}.}
}

\details{
%This implementation attempts to reduce complexity of the basic
%MLInterfaces engine.  The primary MLearn method, which includes 
%"learnerSchema" in its signature, is very concise.  Details
%of massaging inputs and outputs are left to a learnerSchema class instance.
%The MLint\_devel vignette describes the schema formulation.  learnerSchema instances
%are provided for following methods; the naming convention is that the
%method basename is prepended to `I'.
%
%Note that some schema instances are presented as functions.  The
%parameters must be set to use these models.
The purpose of the MLearn methods is to provide a uniform calling sequence
to diverse machine learning algorithms.  In R package, machine learning functions
can have parameters \code{(x, y, ...)} or \code{(formula, data, ...)} or some
other sequence, and these functions can return lists or vectors or other
sorts of things.  With MLearn, we
always have calling sequence \code{MLearn(formula, data, .method, trainInd, ...)}, 
and \code{data} can be a \code{data.frame} or \code{ExpressionSet}.  \code{MLearn}
will always return an S4 instance of \code{classifierObject} or \code{clusteringObject}.

At this time (1.13.x), NA values
in predictors trigger an error.

To obtain documentation on the older (pre bioc 2.1) version of the MLearn
method, please use help(MLearn-OLD).

\describe{
\item{randomForestI}{\link[randomForest]{randomForest}.  Note, that to obtain the
default performance of randomForestB, you need to set mtry and sampsize parameters
to sqrt(number of features) and table([training set response factor]) respectively,
as these were not taken to be the function's defaults.   Note you can use
xvalSpec("NOTEST") as trainInd, to use all the samples; the RObject() result
will print the misclassification matrix estimate along with OOB error rate estimate.}
\item{knnI(k=1,l=0)}{\link[class]{knn}; special support bridge required, defined in MLint}
\item{knn.cvI(k=1,l=0)}{\link[class]{knn.cv}; special support bridge required, defined in MLint.  This option uses the embedded leave-one-out
cross-validation of \code{knn.cv}, and thereby
achieves high performance.  You can have more general cross-validation
using \code{knnI} with an \code{xvalSpec}, but it will be slower.
When using this learner schema, you should use the
numerical \code{trainInd} setting with \code{1:N} where
\code{N} is the number of samples.}
\item{dldaI}{\link[sfsmisc]{diagDA}; special support bridge required, defined in MLint}
\item{nnetI}{\link[nnet]{nnet}}
\item{rpartI}{\link[rpart]{rpart}}
\item{ldaI}{\link[MASS]{lda}}
\item{svmI}{\link[e1071]{svm}}
\item{qdaI}{\link[MASS]{qda}}
\item{logisticI(threshold)}{\link[stats]{glm} -- with binomial family, expecting a dichotomous
factor as response variable, not bulletproofed against other responses yet.  If response
probability estimate exceeds threshold, predict 1, else 0}
%\item{RABI}{\link[MLint]{RAB} -- an experimental implementation of real Adaboost
%of Friedman Hastie Tibshirani Ann Stat 2001}
\item{adaI}{\link[ada]{ada}}
\item{BgbmI}{\link[gbm]{gbm}, forcing the Bernoulli loss function.}
\item{blackboostI}{\link[mboost]{blackboost} -- you MUST supply
a family parameter relevant for mboost package procedures}
\item{lvqI}{\link[class]{lvqtest} after building codebook with lvqinit and updating with
olvq1.  You will need to write your own detailed schema if you want to tweak tuning
parameters.}
\item{naiveBayesI}{\link[e1071]{naiveBayes}}
\item{baggingI}{\link[ipred]{bagging}}
\item{sldaI}{\link[ipred]{slda}}
\item{rdaI}{\link[rda]{rda} -- you must supply the alpha and delta parameters to
use this.  Typically cross-validation is used to select these.  See \code{rdacvI} below.}
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
package rda.  You can use xvalSpec("NOTEST") with this procedure to
use all the samples to build the discriminator.}
\item{ksvmI}{\link[kernlab]{ksvm}}
\item{hclustI(distMethod, agglomMethod)}{\link[stats]{hclust} --
you must explicitly specify distance and agglomeration procedure.}
\item{kmeansI(centers, algorithm)}{\link[stats]{kmeans} --
you must explicitly specify centers and algorithm name.}
} % end list of schemas

If the \code{parallel} package is attached, cross-validation will
be distributed to cores using \code{\link[parallel]{mclapply}}.

} % end details
\value{
Instances of classifierOutput or clusteringOutput
}
%\references{  }
\author{Vince Carey <stvjc@channing.harvard.edu>}
%\note{  }


\seealso{Try \code{example(hclustWidget, ask=FALSE)} for an interactive
approach to cluster analysis tuning.  }

\examples{
library("MASS")
data(crabs)
set.seed(1234)
kp = sample(1:200, size=120)
rf1 = MLearn(sp~CW+RW, data=crabs, randomForestI, kp, ntree=600 )
rf1
nn1 = MLearn(sp~CW+RW, data=crabs, nnetI, kp, size=3, decay=.01,
    trace=FALSE )
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
## recode data for RAB
#nsp = ifelse(crabs$sp=="O", -1, 1)
#nsp = factor(nsp)
#ncrabs = cbind(nsp,crabs)
#rab1 = MLearn(nsp~CW+RW, data=ncrabs, RABI, kp, maxiter=10)
#rab1
#
# new approach to adaboost
#
ada1 = MLearn(sp ~ CW+RW, data = crabs, .method = adaI, 
    trainInd = kp, type = "discrete", iter = 200)
ada1
confuMat(ada1)
#
lvq.1 = MLearn(sp~CW+RW, data=crabs, lvqI, kp )
lvq.1
nb.1 = MLearn(sp~CW+RW, data=crabs, naiveBayesI, kp )
confuMat(nb.1)
bb.1 = MLearn(sp~CW+RW, data=crabs, baggingI, kp )
confuMat(bb.1)
#
# new mboost interface -- you MUST supply family for nonGaussian response
#
require(party)  # trafo ... killing cmd check
blb.1 = MLearn(sp~CW+RW+FL, data=crabs, blackboostI, kp, family=mboost::Binomial() )
confuMat(blb.1)
#
# ExpressionSet illustration
# 
data(sample.ExpressionSet)
#  needed to increase training set size to avoid a new randomForest condition
# on empty class
set.seed(1234)
X = MLearn(type~., sample.ExpressionSet[100:250,], randomForestI, 1:19, importance=TRUE )
library(randomForest)
library(hgu95av2.db)
opar = par(no.readonly=TRUE)
par(las=2)
plot(getVarImp(X), n=10, plat="hgu95av2", toktype="SYMBOL")
par(opar)
#
# demonstrate cross validation
#
nn1cv = MLearn(sp~CW+RW, data=crabs[c(1:20,101:120),], 
   nnetI, xvalSpec("LOO"), size=3, decay=.01, trace=FALSE )
confuMat(nn1cv)
nn2cv = MLearn(sp~CW+RW, data=crabs[c(1:20,101:120),], nnetI, 
   xvalSpec("LOG",5, balKfold.xvspec(5)), size=3, decay=.01,
   trace=FALSE )
confuMat(nn2cv)
nn3cv = MLearn(sp~CW+RW+CL+BD+FL, data=crabs[c(1:20,101:120),], nnetI, 
   xvalSpec("LOG",5, balKfold.xvspec(5), fsFun=fs.absT(2)), size=3, decay=.01,
   trace=FALSE )
confuMat(nn3cv)
nn4cv = MLearn(sp~.-index-sex, data=crabs[c(1:20,101:120),], nnetI, 
   xvalSpec("LOG",5, balKfold.xvspec(5), fsFun=fs.absT(2)), size=3, decay=.01,
   trace=FALSE )
confuMat(nn4cv)
#
# try with expression data
#
library(golubEsets)
data(Golub_Train)
litg = Golub_Train[ 100:150, ]
g1 = MLearn(ALL.AML~. , litg, nnetI, 
   xvalSpec("LOG",5, balKfold.xvspec(5), 
   fsFun=fs.probT(.75)), size=3, decay=.01, trace=FALSE )
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

# illustrate clustering support

cl1 = MLearn(~CW+RW+CL+FL+BD, data=crabs, hclustI(distFun=dist, cutParm=list(k=4)))
plot(cl1)

cl1a = MLearn(~CW+RW+CL+FL+BD, data=crabs, hclustI(distFun=dist, cutParm=list(k=4)), 
   method="complete")
plot(cl1a)

cl2 = MLearn(~CW+RW+CL+FL+BD, data=crabs, kmeansI, centers=5, algorithm="Hartigan-Wong")
plot(cl2, crabs[,-c(1:3)])

c3 = MLearn(~CL+CW+RW, crabs, pamI(dist), k=5)
c3
plot(c3, data=crabs[,c("CL", "CW", "RW")])


#  new interfaces to PLS thanks to Laurent Gatto

set.seed(1234)
kp = sample(1:200, size=120)

plsda.1 = MLearn(sp~CW+RW, data=crabs, plsdaI, kp, probMethod="Bayes")
plsda.1
confuMat(plsda.1)
confuMat(plsda.1,t=.65) ## requires at least 0.65 post error prob to assign species

plsda.2 = MLearn(type~., data=sample.ExpressionSet[100:250,], plsdaI, 1:16)
plsda.2
confuMat(plsda.2)
confuMat(plsda.2,t=.65) ## requires at least 0.65 post error prob to assign outcome

## examples for predict
clout <- MLearn(type~., sample.ExpressionSet[100:250,], svmI , 1:16)
predict(clout, sample.ExpressionSet[100:250,17:26])

}

\keyword{ models }
