
setMethod("show", "clusteringOutput", function(object) {
 cat("MLInterfaces clustering output container\n")
 cat("The call was:\n")
 print(object@call)
})

setMethod("RObject", "classifierOutput", function(obj) obj@RObject)

