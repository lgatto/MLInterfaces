README.txt for MLInterfaces, Jun 19 2015

hclustWidget has been added to demonstrate roles for shiny, fpc, and ggvis in
tuning cluster analysis.

README.txt for MLInterfaces, Sep 16 2008

The *B methods have been removed.  The NAMESPACE is used to reduce
the number of symbols/classes/methods in play.  Handling of 
unsupervised learning via MLearn is in design.

README.txt for MLInterfaces, Sept 9 2007

Version 2.0 of MLInterfaces is imminent; I hope it will be available by
Sept 24.  Version 1.11.15+ includes a number of new approaches to
simplify maintenance and extensibility

Basic approach:
A learner function defined in a package with a namespace, that uses a formula interface,
and that has a predict method, is the most reasonable case and the one that the
interface design is based on.  A schema object is defined to provide access
to such learners and to define how their outputs are converted to standard
structures.

1) It is intended that all use will be through MLearn(formula, data, ...)
  a) The simplest usage is MLearn(formula, data, learnerSchema, trainInds).
Instances of learnerSchema identify the package and function constituting
the learner, and include a converter function with standard calling sequence
that converts results of the learner function into a classifierOutput
or clusteringOutput instance.  classifierOutput is currently defined, and
it manages the predictions (test and train) and any goodness of prediction
information produced.  It also includes the call and the full representation
of the learner's output.  When feature selection has been used in cross-validation,
the 'history' of feature selection is retained.
  b) Cross-validation is carried out using 
      MLearn(formula, data, learnerSchema, xvalSpec, ...)
Instances of xvalSpec define the partitioning of the data.  Of note is that
the former implementations of xval/xvalML did not generate MLOutput instances,
but the new implementation will generate objects identical in nature to those
produced in the train/test scheme described in 1a)
  At present MLearn+xvalSpec yields has all functionality of previous versions (including
support for functionally specified partition) but the way of handling feature selection
is different.  Instead of returning a set of feature scores, the function defining
feature selection needs to return a formula that includes the chosen features.  An
example of this is given with help(MLearn), and also shown below.  One feature
that is not retained with MLearn+xvalSpec is the xvalLoop generic with its support
for cluster computing.  I will introduce that ASAP, when I have a working example.
  c) So far, only supervised learning is handled in the new approach.  Now that
cross-validation is working, I will start to deal with unsupervised methods.
  d) A brief vignette that describes basic architecture with examples is in in
MLint_devel.Rnw.  This will be elaborated soon.

2) Back-compatibility and deprecation.  I have left the *B methods in place,
but they have been sequestered into a single file Bmethods.R.  Eventually
they will be deprecated, possibly by Sept 24.  The old version of MLearn,
which uses a string to identify the learner and a switch statement to
act, is retained for now but I do not intend to improve it in any way.
There was a problem with parameter capture in this design that motivated
the move to a schematic approach to learner specification.  It can stay 
indefinitely; we will just discourage use of strings and encourage the use of
schema objects to specify learners.

3) Benefits.  The primary MLearn method definition is 18 lines long.
Converter functions are typically around 8 lines long, and vary primarily
because of the different approaches to the 'predict' generic taken in
various machine learning packages.  We will not need a separate xval
wrapper.

4) Costs.  The calling sequence is modified slightly, so we would have
MLearn(sp~CL+RW, crabs, ldaI, c(1:25, 101:125)) instead of
MLearn(sp~CL+RW, crabs, "lda", c(1:25, 101:125)).  In most cases,
tuning parameters are passed using ... after the trainInd or
xvalSpec.  

Some learners have such idiosyncratic implementations (no formula interface, 
no predict method) that bridge methods need to be defined, and the schemas are
specified using closures to fix the tuning parameters: knnI(k=3,l=1) for example.
I would like to handle all tuning parameters uniformly, but this minority
of learners cannot be allowed to force major complexities on this package.


5) Examples

a) A simple formula+data frame test vs train exercise:

rf1 = MLearn(sp~CW+RW, data=crabs, randomForestI, kp, ntree=600 )
confuMat(rf1)

b) Cross-validation, LOO:

nn1cv = MLearn(sp~CW+RW, data=crabs[c(1:20,101:120),], nnetI, xvalSpec("LOO"), size=3, decay=.01 )

c) 5-fold cross-validation, partitions are balanced with respect to outcome class frequencies

nn2cv = MLearn(sp~CW+RW, data=crabs[c(1:20,101:120),], nnetI,
                  xvalSpec("LOG",5, balKfold.xvspec(5)), size=3, decay=.01 )

d) 5-fold cross-validation, feature selection using top 25% of features when ranked by
   two-sample t.  First define the fsFun:

fsFun.rowtQ3 = function(formula, data) {
 # facilitation of a rowttests with a formula/data.frame takes a little work
 mf = model.frame(formula, data)
 mm = model.matrix(formula, data)
 respind = attr( terms(formula, data=data), "response" )
 x = mm
 if ("(Intercept)" %in% colnames(x)) x = x[,-which(colnames(x) == "(Intercept)")]
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

now deploy

library(golubEsets)
data(Golub_Train)
litg = Golub_Train[ 100:150, ]
g1 = MLearn(ALL.AML~. , litg, nnetI, xvalSpec("LOG",5, balKfold.xvspec(5), fsFun=fsFun.rowtQ3), size=3, decay=.01 )
confuMat(g1)
fsHistory(g1)

the fsFun is messy, and the acceptance criterion can be factored out to give more flexibility, eventually

