setClass("raboostCont", representation(formula="formula", call="call"), contains="list")
setClass("daboostCont", contains="raboostCont")

setMethod("show", "raboostCont", function(object) {
 if (is(object, "daboostCont")) cat("discrete adaboost object,", length(object), "iterations.\n")
 else cat("real adaboost object,", length(object), "iterations.\n")
 cat("the call was:\n")
 print(object@call)
})

RAB = function(formula, data, maxiter=200, maxdepth=1) {
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
 new("raboostCont", out, formula=formula, call=theCall)
}

setGeneric("Predict", function(object, ...) standardGeneric("Predict"))
setMethod("Predict", c("raboostCont"), function( object, ...) {
 logit = function(x) {
     x[x < .000001] = .000001
     x[x > .999999] = .999999
     log(x/(1-x))
     }
 argl = list(...)
 if (length(argl) > 0) {
   newdata = argl[["newdata"]]
   ans = lapply(object, function(x) rpart:::predict.rpart(x$tree, newdata))
 }
 else ans = lapply(object, function(x) rpart:::predict.rpart(x$tree))
 if (length(dim(ans[[1]])) == 2) newf = lapply(ans, function(x) .5*logit(x[,2]))
 else newf = lapply(ans, function(x) .5*logit(x[2]))
 addf = newf[[1]]
 for (i in 2:length(newf)) addf = addf+newf[[i]]
 ifelse(addf>0, 1, -1)
})

setMethod("Predict", c("daboostCont"), function( object, ...) {
 logit = function(x) {
     x[x < .000001] = .000001
     x[x > .999999] = .999999
     log(x/(1-x))
     }
 argl = list(...)
 if (length(argl) > 0) {
   newdata = argl[["newdata"]]
   ans = lapply(object, function(x) rpart:::predict.rpart(x$tree, newdata, type="class"))
 }
 else ans = lapply(object, function(x) rpart:::predict.rpart(x$tree, type="class"))
 anac = function(x) as.numeric(as.character(x))
 addf = object[[1]]$cm*anac(ans[[1]])
 for (i in 2:length(ans)) addf = addf+object[[i]]$cm*anac(ans[[i]])
 ifelse(addf>0, 1, -1)
})
	
DAB = function(formula, data, maxiter=200, maxdepth=1) {
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
 print("Discrete adaboost iterations:")
 for (i in 1:maxiter) {
    cat(i)
    data$w = w
    tr = rpart(formula, data=data, maxdepth=maxdepth, weights=w)
    predcl = rpart:::predict.rpart(tr, type="class")
    err = mean(1*(fail <- (tmpy != as.character(predcl))))
    cm = log((1-err)/err)
    w = w*exp(cm*(1*fail))
    w = w/sum(w)
    out[[i]] = list(tree=tr, weight=w, cm=cm)
    }
 tmp = new("raboostCont", out, formula=formula, call=theCall)
 new("daboostCont", tmp)
}

mkfmla = function(respn, nvec) {
 as.formula(paste(respn, "~", paste(make.names(nvec), collapse="+")))
}

tonp = function(x, pos="Yes") {
 tmp = ifelse(x==pos,1,-1)
 factor(tmp)
}


RAB4es = function (respn, eset, val, maxit=10, maxdepth=4)
{
    ee = data.frame(t(exprs(eset)))
    nn = names(ee)
    ff = mkfmla(respn, nn)
    tmp = tonp(eset[[respn]], val)
    ee = data.frame(ee, tmp)
    names(ee)[ncol(ee)] = respn
    RAB(ff, data=ee, maxit=maxit, maxdepth=maxdepth)
}

