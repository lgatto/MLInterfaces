partPlot <- function(x, p, doXax=TRUE, ...) {
  ## Not scaling kmeans' partitions p, only matrix x - L. Gatto 10 Nov 2012
  rzsco <- function(x) (x-median(x))/mad(x)
  x <- data.matrix(x)
  x <- apply(x, 2, rzsco)
  d <- cbind(as.numeric(p), x)
  d <- d[order(d[,1]),]
  image(t(d), axes=FALSE, xlab="part.; features", ylab="cases", ...)
  if (doXax) axis(1, at=seq(.01,.99,len=ncol(d)), c("part.", colnames(d)[-1]))
}
