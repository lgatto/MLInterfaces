
fs.absT = function(N) function(formula, data) {
 # take features using top N t stat in abs val
 # facilitation of a rowttests with a formula/data.frame takes a little work
 mf = model.frame(formula, data)
 mm = model.matrix(formula, data)
 respind = attr( terms(formula, data=data), "response" )
 x = mm
 if ("(Intercept)" %in% colnames(x)) x = x[,-which(colnames(x) == "(Intercept)")]
 if (N > dim(x)[2]) stop("N in fs.absT exceeds number of features available")
 y = mf[, respind]
 respname = names(mf)[respind]
 nuy = length(unique(y))
 if (nuy > 2) warning("number of unique values of response exceeds 2")
 ans = abs( rowttests(t(x), factor(y), tstatOnly=TRUE)[[1]] )
 names(ans) = colnames(x)
 ans = names( sort(abs(ans),decr=TRUE)[1:N] )
 btick = function(x) paste("`", x, "`", sep="")  # support for nonsyntactic varnames
 as.formula( paste(respname, paste(btick(ans), collapse="+"), sep="~"))
}

fs.probT = function(p) function(formula, data) {
 # take features using features giving t stats in the 100pth percentile or above
 # facilitation of a rowttests with a formula/data.frame takes a little work
 if (p <= 0 | p >=1 ) stop("p in fs.percT must be in (0,1)")
 mf = model.frame(formula, data)
 mm = model.matrix(formula, data)
 respind = attr( terms(formula, data=data), "response" )
 x = mm
 if ("(Intercept)" %in% colnames(x)) x = x[,-which(colnames(x) == "(Intercept)")]
 y = mf[, respind]
 respname = names(mf)[respind]
 nuy = length(unique(y))
 if (nuy > 2) warning("number of unique values of response exceeds 2")
 ans = abs( rowttests(t(x), factor(y), tstatOnly=TRUE)[[1]] )
 names(ans) = colnames(x)
 ans = names( ans[ which(ans > quantile(ans, p) ) ] )
 btick = function(x) paste("`", x, "`", sep="")  # support for nonsyntactic varnames
 as.formula( paste(respname, paste(btick(ans), collapse="+"), sep="~"))
}

