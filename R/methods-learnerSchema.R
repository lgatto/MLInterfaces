setMethod("show", "learnerSchema", function(object) {
 cat("MLInterfaces schema for", object@mlFunName, "in", object@packageName, "\n")
})

makeLearnerSchema = function(packname, mlfunname, converter) {
 new("learnerSchema", packageName=packname, mlFunName=mlfunname,
   converter=converter ) }

