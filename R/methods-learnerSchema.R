setMethod("show", "learnerSchema", function(object) {
 cat("MLInterfaces schema for", object@mlFunName, "in", object@packageName, "\n")
})

makeLearnerSchema = function(packname, mlfunname, converter, predicter) {
  if (missing(predicter)) 
    return(new("learnerSchema",
               packageName=packname,
               mlFunName=mlfunname,
               converter=converter))
  return(new("learnerSchema",
             packageName=packname,
             mlFunName=mlfunname,
             converter=converter,
             predicter=predicter))
}

makeClusteringSchema = function(packname, mlfunname, distMethod, converter,
  agglomMethod, algorithm, ...) {
 if (missing(agglomMethod)) agglomMethod="none"
 if (missing(algorithm)) algorithm="none"
 new("clusteringSchema", packageName=packname, mlFunName=mlfunname,
   distMethod=distMethod, converter=converter, 
   agglomMethod=agglomMethod, algorithm=algorithm, extras=list(...)) }
