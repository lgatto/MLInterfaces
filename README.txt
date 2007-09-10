
README.txt for MLInterfaces, Sept 9 2007

Version 2.0 of MLInterfaces is imminent; I hope it will be available by
Sept 24.  Version 1.11.10+ includes a number of new approaches to
simplify maintenance and extensibility

Basic approach:
A learner defined in a package with a namespace, that uses a formula interface,
and has a predict method, is the most reasonable case and the one that the
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
of the learner's output.
  b) Cross-validation is carried out using 
      MLearn(formula, data, learnerSchema, xvalSpec, ...)
Instances of xvalSpec define the partitioning of the data.  Of note is that
the former implementations of xval/xvalML did not generate MLOutput instances,
but the new implementation will generate objects identical in nature to those
produced in the train/test scheme described in 1a)
  At present MLearn+xvalSpec yields has all functionality of previous versions (including
support for functionally specified partition) except i) Martin Morgan had
some infrastructure simplifying cluster computing for xval, and ii) Stephen
Henderson had contributed a feature selection facility.  I am working on ii).
  c) So far, only supervised learning is handled in the new approach.  My
plan is to continue with cross-validation first, and then address the
unsupervised methods.

2) Back-compatibility and deprecation.  I have left the *B methods in place,
but they have been sequestered into a single file Bmethods.R.  Eventually
they will be deprecated, possibly by Sept 24.  The old version of MLearn,
which uses a string to identify the learner and a switch statement to
act, is retained for now but I do not intend to improve it in any way.
There was a problem with parameter capture in this design that motivated
the move to a schematic approach to learner specification.  It can stay 
indefinitely; we will just discourage use of strings as opposed to
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

