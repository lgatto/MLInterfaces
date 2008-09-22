
#setMethod("show", "clusteringOutput", function(object) {
# cat("MLInterfaces clustering output container\n")
# cat("The call was:\n")
# print(object@call)
#})
#
#setMethod("RObject", "clusteringOutput", function(x) x@RObject)
#
#
#setMethod("plot", "clusteringOutput", function(x, y, ...) {
# opar = par(no.readonly=TRUE)
# on.exit(par(opar))
# par(mfrow=c(2,2))
# if (x@learnerSchema@mlFunName=="hclust") plclust(RObject(x))
# else if (x@learnerSchema@mlFunName=="pam") plot(RObject(x))
## else {
##    if (missing(y)) stop("second arg must be matrix with feature data on all records")
#    partPlot(y, x@partition, las=2)
#    }
# plot(x@silhouette, main="silhouette")
# plot(x@prcomp, main="PCA screeplot")
# plot(x@prcomp$x[,1], x@prcomp$x[,2], col=x@partition,
#   xlab="PC1", ylab="PC2", main="PCA colored by partition")
#})
