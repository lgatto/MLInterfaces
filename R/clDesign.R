
#setGeneric("MLearnC", 
#   function(formula, data, schema, ...)
#     standardGeneric("MLearnC"))

setClass("clusteringSchema", representation(
   package="character", mlFunName="character",
   distFun="function", converter="function"))

setMethod("show", "clusteringSchema", function(object) {
  cat("MLInterfaces clusteringSchema for calling\n")
  cat(object@mlFunName, "in package", object@package, ";\n")
  cat("use getDist or getConverter on this schema object to see specifics.\n")
})
setGeneric("getDist", function(x)standardGeneric("getDist"))
setGeneric("getConverter", function(x)standardGeneric("getConverter"))
setMethod("getDist", "clusteringSchema", function(x) x@distFun)
setMethod("getConverter", "clusteringSchema", function(x) x@converter)

## require(cluster)
setOldClass("silhouette")

setOldClass("prcomp")
setClass("prcompObj", contains="prcomp")

setClass("clusteringOutput", representation(
        partition="numeric", silhouette="silhouette", 
        prcomp="prcompObj", distFun="function", converter="function",
        call="call", learnerSchema="clusteringSchema",
        RObject="ANY"), prototype=prototype(
                  partition=numeric(0),
                  silhouette={x = 0; class(x)="silhouette"; x},
                  prcomp={x = 0; class(x)="prcomp"; new("prcompObj", x)},
                  distFun = dist, converter=function(){}, call=new("call"))
                  )

setMethod("show", "clusteringOutput", function(object) {
 cat("clusteringOutput: partition table\n")
 print(table(object@partition))
 cat("The call that created this object was:\n")
 print(object@call)
})

#setGeneric("RObject", function(x) standardGeneric("RObject"))
setMethod("RObject", "clusteringOutput", function(x) x@RObject)

setMethod("plot", "clusteringOutput", function(x, y, ...) {
  opar = par(no.readonly=TRUE)
  on.exit(par(opar))
  par(mfrow=c(2,2))
  NO_PLOT_METHOD = c("kmeans")
  if (x@learnerSchema@mlFunName %in% NO_PLOT_METHOD) {
    if (missing(y)) stop("second arg must be matrix with feature data on all records")
    partPlot(y, x@partition, las=2, ...)
  }
  else if (x@learnerSchema@mlFunName == "pam") {
    plot(RObject(x), which=1, ...)
  } else {
    plot(RObject(x), ...)
  }
  plot(x@silhouette, main="silhouette")
  plot(x@prcomp, main="PCA screeplot")
  plot(x@prcomp$x[,1], x@prcomp$x[,2], col=x@partition,
       xlab="PC1", ylab="PC2", main="PCA colored by partition")
})



hclustConverter = function(tpl) function(obj, dstruct) {
#
# tasks of the converter: generate a clusteringOutput instance
# with entities unique to the clustering algorithm, principally the RObject,
# the partition
#
# in this case we need a closure to pass selections for cutree
#
   tpl[[length(tpl)+1]] = obj
   names(tpl)[length(tpl)] = "tree"
   part = do.call(get("cutree", "package:stats"), tpl)
   new("clusteringOutput", RObject=obj, partition=part)
}

kmeansConverter = function(obj, dstruct) {
#
# tasks of the converter: generate a clusteringOutput instance
# with entities unique to the clustering algorithm, principally the RObject,
# the partition
#
   new("clusteringOutput", RObject=obj, partition=obj$clus)
}

pamConverter = function(obj, dstruct) {
#
# tasks of the converter: generate a clusteringOutput instance
# with entities unique to the clustering algorithm, principally the RObject,
# the partition
#
   new("clusteringOutput", RObject=obj, partition=obj$clustering)
}

hclustI = function(distFun, cutParm) {
   if (missing(distFun)) stop("distFun must be supplied as a function")
   if (!is.list(cutParm)) stop("cutParm must be a list with element name in c('h', 'k')")
   new("clusteringSchema",
      package="stats", mlFunName="hclust",
         distFun=distFun, converter=hclustConverter(cutParm))
}

kmeansI = 
   new("clusteringSchema",
      package="stats", mlFunName="kmeans",
         distFun=function(x)x, converter=kmeansConverter)

pamI = function(distFun) {
   if (missing(distFun)) stop("distFun must be supplied as a function")
   new("clusteringSchema",
      package="cluster", mlFunName="pam",
         distFun=distFun, converter=pamConverter)
}

setMethod("MLearn", c("formula", "data.frame",
   "clusteringSchema", "ANY"),
   function(formula, data, .method, trainInd, ...) {
   lfun = do.call("::", list(pkg=.method@package, name=.method@mlFunName))
#
# formula work
#
  dframe = try(model.frame(formula, data, na.action=na.fail))
  if (inherits(dframe, "try-error"))
     stop("problem in model.frame -- could be NA in data.  must stop.")
  rawdata = data = data.matrix(dframe)
  resp = model.response(dframe)
  if (!is.null(resp)) {
    kpcol = attr(attr(dframe, "terms"), "term.labels")
    data = data[, kpcol]
    rawdata = rawdata[, kpcol]
    }
#
# end formula work
#
   dstruc = .method@distFun(data)  # distFun could be identity
   ans = lfun( dstruc, ... )
   CLOb = .method@converter(ans, dstruc) # clusteringOutput
   if (!inherits(data, "dist")) dstruc = dist(data) # force euclidean for now
   CLOb@silhouette = cluster::silhouette(CLOb@partition, dstruc)
   CLOb@prcomp = new("prcompObj", prcomp(data))
   CLOb@learnerSchema = .method
   CLOb@call = match.call()
   CLOb
})
     
#library(MASS)
#data(crabs)
#
#cl1 = MLearn(~CW+RW+CL+FL+BD, data=crabs, hclustI(distFun=dist, cutParm=list(k=4)))
#cl2 = MLearn(~CW+RW+CL+FL+BD, data=crabs, kmeansI, centers=5, algorithm="Hartigan-Wong")
