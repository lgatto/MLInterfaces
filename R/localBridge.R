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
    ans = stat.diag.da(x, as.numeric(cl), x, ...)$pred
    ans = list(traindat = x, ans = ans, traincl = cl)
    class(ans) = "dlda2"
    ans
}

predict.dlda2 = function(object, newdata, ...) {
 require(sma)
 stat.diag.da( object$traindat, as.numeric(object$traincl), newdata, ... )$pred
}

