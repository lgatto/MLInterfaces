partPlot = function(x, p, doXax=TRUE, ...) {
 rzsco = function(x) (x-median(x))/mad(x)
 d = cbind(as.numeric(p), data.matrix(x))
 d = apply(d, 2, rzsco)
 d = d[order(d[,1]),]
 image(t(d), axes=FALSE, xlab="part.; features", ylab="cases", ...)
 if (doXax) axis(1, at=seq(.01,.99,len=ncol(d)), c("part.", colnames(d)[-1]))
}
