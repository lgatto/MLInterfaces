# -- localBridge.R -- defines functions in namespaces with 
#    predict methods and formula interfaces when needed
#    currently handles knn, lvq, dlda, RAB (latter fully defined here)
# -- RAB -- renamed to rab to allow old RAB to work

rab = function(formula, data, maxiter=200, maxdepth=1) {
 if (as.character(as.list(formula)[[3]])[1] == ".") stop("please state the formula explicitly, no \".\"")
 theCall = match.call()
 out = list()
 tmpy = model.response(model.frame(formula, data=data))
 if (!is.factor(tmpy)) stop("response vbl must be factor with levels -1, 1")
 tmpy = as.character(tmpy)
 numy = as.numeric(tmpy)
 if (!all(tmpy %in% c("-1","1"))) stop("response must be coded -1, 1")
 N = nrow(data)
 w = rep(1/N, N)
 data = data.frame(data, w=w)
 print("real adaboost iterations:")
 for (i in 1:maxiter) {
    cat(i)
    data$w = w
    tr = rpart(formula, data=data, maxdepth=maxdepth, weights=w)
    p = rpart:::predict.rpart(tr)[,2]
    p[p < .000001] = .000001
    p[p > .999999] = .999999
    f = .5*log(p/(1-p))
    w = w*exp(-numy*f)
    w = w/sum(w)
    out[[i]] = list(tree=tr, weight=w)
    }
res = list(ans=out, formula=formula, call=theCall)
class(res) = "RAB"
res
}

predict.RAB = function( object, ...) {
 logit = function(x) {
     x[x < .000001] = .000001
     x[x > .999999] = .999999
     log(x/(1-x))
     }
 argl = list(...)
 if (length(argl) > 0) {
   newdata = argl[["newdata"]]
   ans = lapply(object$ans, function(x) rpart:::predict.rpart(x$tree, newdata)) # why??
 }
 else ans = lapply(object$ans, function(x) rpart:::predict.rpart(x$tree))
 if (length(dim(ans[[1]])) == 2) newf = lapply(ans, function(x) .5*logit(x[,2]))
 else newf = lapply(ans, function(x) .5*logit(x[2]))
 addf = newf[[1]]
 for (i in 2:length(newf)) addf = addf+newf[[i]]
 factor(ifelse(addf>0, 1, -1))
}


# -- knn

knn2 = function (formula, data, ...) 
{
    mf = model.frame(formula, data)
    cl = model.response(mf)
    x = mf[, -1]
    ans = class::knn(x, x, cl, prob=TRUE, ...)
    ans = list(traindat = x, ans = ans, traincl = cl)
    class(ans) = "knn2"
    ans
}

predict.knn2 = function(object, newdata, ...) {
 class::knn( object$traindat, newdata, object$traincl , prob=TRUE, ... )
}

# -- knn.cv

knn.cv2 = function (formula, data, ...) 
{
    mf = model.frame(formula, data)
    cl = model.response(mf)
    x = mf[, -1]
    ans = class::knn.cv( x, cl, prob=TRUE, ...)
    ans = list(traindat = x, ans = ans, traincl = cl)
    class(ans) = "knn.cv2"
    ans
}

predict.knn.cv2 = function(object, newdata, ...) {
 class::knn.cv( newdata, object$traincl , prob=TRUE, ... )
}

# -- lvq

lvq = function(formula, data, ...) {
 mf = model.frame(formula, data)
 cl = model.response(mf)
 x = mf[,-1]
 cb1 = class::lvqinit(x, cl, ...)
 cb1 = class::olvq1(x, cl, cb1 )
 ans = list(traindat=x, ans=cb1, traincl=cl)
 class(ans) = "lvq"
 ans
}


predict.lvq = function(object, newdata, ...) {
 class::lvqtest(object$ans, newdata)
} 

# -- dlda

dlda2 = function (formula, data, ...) 
{
    mf = model.frame(formula, data)
    cl = model.response(mf)
    x = mf[, -1]
    require(sma)
# deal with global visibility
    ans = get("stat.diag.da")(x, as.numeric(cl), x, ...)$pred
    ans = list(traindat = x, ans = ans, traincl = cl)
    class(ans) = "dlda2"
    ans
}

predict.dlda2 = function(object, newdata, ...) {
 require(sma)
# deal with global visibility
 get("stat.diag.da")( object$traindat, as.numeric(object$traincl), newdata, ... )$pred
}

# -- rdacvML -- bridges to rda::rda.cv which requires a run of rda

rdaCV = function( formula, data, ... ) {
 passed = list(...)
 if ("genelist" %in% names(passed)) stop("please don't supply genelist parameter.")
 # data input to rda needs to be GxN
 x = model.matrix(formula, data)
 if ("(Intercept)" %in% colnames(x))
   x = x[, -which(colnames(x) %in% "(Intercept)")]
 x = t(x)
 mf = model.frame(formula, data)
 resp = as.numeric(factor(model.response(mf)))
 run1 = rda( x, resp, ... )
 rda.cv( run1, x, resp )
}

rdaFixed = function( formula, data, alpha, delta, ... ) {
 passed = list(...)
 if ("genelist" %in% names(passed)) stop("please don't supply genelist parameter.")
 # data input to rda needs to be GxN
 x = model.matrix(formula, data)
 if ("(Intercept)" %in% colnames(x))
   x = x[, -which(colnames(x) %in% "(Intercept)")]
 x = t(x)
 featureNames = rownames(x)
 mf = model.frame(formula, data)
 resp = as.numeric(resp.fac <- factor(model.response(mf)))
 finalFit=rda( x, resp, genelist=TRUE, alpha=alpha, delta=delta, ... )
 list(finalFit=finalFit, x=x, resp.num=resp, resp.fac=resp.fac, featureNames=featureNames,
    keptFeatures=featureNames[ which(apply(finalFit$gene.list,3,function(x)x)==1) ])
}

rdacvML = function(formula, data, ...) {
 run1 = rdaCV( formula, data, ... )
 perf.1se = cverrs(run1)$one.se.pos
 del2keep = which.max(perf.1se[,2])
 parms2keep = perf.1se[del2keep,]
 alp = run1$alpha[parms2keep[1]]
 del = run1$delta[parms2keep[2]]
 fit = rdaFixed( formula, data, alpha=alp, delta=del, ... )
 class(fit) = "rdacvML"
 attr(fit, "xvalAns") = run1
 fit
}

rdaML = function(formula, data, ...) {
 dots = list(...)
 nd = names(dots)
 if (!(all(c("alpha", "delta") %in% nd))) stop("alpha and delta must be supplied with rdaI")
 fit = rdaFixed( formula, data, ... )
 class(fit) = "rdaML"
 fit
}

print.rdaML = function(x, ...) {
 cat("rdaML S3 instance. components:\n")
 print(names(x))
}

print.rdacvML = function(x, ...) {
 cat("rdacvML S3 instance. components:\n")
 print(names(x))
 cat("---\n")
 cat("elements of finalFit:\n")
 print(names(x$finalFit))
 cat("---\n") 
 cat("the rda.cv result is in the xvalAns attribute of the main object.\n")
}

rda.xvalAns = function(cfo) {
 if (!is(cfo, "classifierOutput")) stop("works for classifierOutput instance from MLearn/rdacvI")
 attr( RObject(cfo), "xvalAns" )
}

plotXvalRDA = function(cfo, ...) {
 if (!is(cfo, "classifierOutput")) stop("works for classifierOutput instance from MLearn/rdacvI")
 plot( rda.xvalAns(cfo), ... )
}

predict.rdacvML = function(object, newdata, ...) {
 newd = data.matrix(newdata)
 fnames = rownames(object$x)
 newd = newd[, fnames]
 if (any(dim(newd) == 0))
     inds =predict(object$finalFit, object$x, object$resp.num, xnew=object$x)
 else inds = try(predict(object$finalFit, object$x, object$resp.num, xnew=t(newd)))
 factor(levels(object$resp.fac)[inds])
}

predict.rdaML = function(object, newdata, ...) {
 newd = data.matrix(newdata)
 fnames = rownames(object$x)
 newd = newd[, fnames]
 if (any(dim(newd) == 0))
     inds =predict(object$finalFit, object$x, object$resp.num, xnew=object$x)
 else inds = try(predict(object$finalFit, object$x, object$resp.num, xnew=t(newd)))
 factor(levels(object$resp.fac)[inds])
}



cverrs = function (x, type = c("both", "error", "gene"), nice = FALSE, 
    ...) 
{
    if (class(x) != "rdacv") {
        stop("You must supply a cross-validation object.")
    }
    else {
        minerr <- min(x$cv.err)/x$n
        one.se <- ceiling((sqrt(minerr * (1 - minerr)/x$n) * 
            1.645 + minerr) * x$n)
        pos <- which(x$cv.err == one.se, TRUE)
        minpos <- which(x$cv.err == min(x$cv.err), TRUE)
        tmperr <- x$cv.err
        dimnames(tmperr) <- list(x$alpha, x$delta)
        tmpgene <- x$ngene
        dimnames(tmpgene) <- list(x$alpha, x$delta)
        return(list(one.se.pos = pos, min.cv.pos = minpos))
    }
}

# -- logitboost
logitboost2 = function (formula, data, ...) 
{
    require(boost, quietly=TRUE)
    mf = model.frame(formula, data)
    cl = model.response(mf)
    cll = levels(cl)
    cl = 1*(cl==levels(cl)[1])
    x = data.matrix(mf[, -1])
    ans = boost::logitboost(x, cl, x, ...)
    ans = list(traindat = data.matrix(x), ans = ans, traincl = cl,
       cll=cll)
    class(ans) = "logitboost2"
    ans
}

predict.logitboost2 = function(object, newdata, ...) {
 require(boost, quietly=TRUE)
 cllev = object$cll
 vars = colnames(object$traindat)
 newdata = newdata[,vars,drop=FALSE]
 app = boost::logitboost( object$traindat, object$traincl,  data.matrix(newdata), ... )
 tmp = apply(app, 1, function(x)1*(mean(x>=.5)>=.5))
 factor(ifelse(tmp==1, cllev[1], cllev[2]))
}

## gbm -- the problem here is that it cannot use a factor as dependent
## variable, and only returns numeric predictions -- unusual for classification procedures

# our solution -- continue to require user to supply a factor reponse variable
# but coerce it to 0-1 setting first level of response to zero and second to one
# then call gbm

gbm2 = function(formula, data, ...) {
 require(gbm, quietly=TRUE)
 mf = model.frame(formula, data)
 resp = model.response(mf)
 if (!is(resp, "factor")) stop("dependent variable must be a factor in MLearn")
 if (length(levels(resp)) !=2 ) stop("dependent variable must have two levels")
 nresp = as.numeric(resp == levels(resp)[2])
 fwn = formula
 fwn[[2]] = as.name("nresp")
 newf = as.formula(fwn)
 data$nresp = nresp
 ans = gbm( newf, data, ... )
 class(ans) = "gbm2"
 ans
}
 
# now we need a suitable prediction method.  we know predict.gbm will only return
#  a numeric vector.  so just hand it back as a factor; to make sense of it
#  need to use as.numeric(as.character())

predict.gbm2 = function(object, newdata, ...) {
 require(gbm, quietly=TRUE)
 ans = predict.gbm(object, newdata, ...)
 factor(ans)
}
