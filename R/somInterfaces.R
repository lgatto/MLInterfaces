# description: interface to som {som} 
# arguments:
#	exprObj		exprSet
#	xdim		x dimension
#	ydim		y dimension
#	metric		for distance matrix 
# value:
# 	object of class "classifPred"
#	where sampLabels are the labels of the original sample
# example:
# somOut <- somB(golubMerge[100:200,], "ALL.AML", 2, 3)
#####################

setClass("somout", contains="list")
setMethod("show", "somout", function(object) {
 cat("somB output\n")
 print(object$call)
 cat("available elements:\n")
 print(names(object))
})

setGeneric("somB", function(exprObj, classifLab, xdim=3, ydim=3, init="linear", alpha, alphaType="inverse", 
		neigh="gaussian", topol="rect", radius, rlen, err.radius=1, inv.alp.c, metric="euclidean"){
		standardGeneric("somB")
})

setMethod("somB", c("exprSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", 
		"ANY", "ANY"), 
		function(exprObj, classifLab, xdim, ydim, init, alpha, alphaType, neigh, topol, 
		radius, rlen, err.radius, inv.alp.c, metric){

		if(missing(alpha)){ alpha <- NULL }
		if(missing(radius)){ radius <- NULL }
		if(missing(rlen)){ rlen <- NULL }
		if(missing(inv.alp.c)){ inv.alp.c <- NULL }
		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		out <- som::som(dat, xdim, ydim, init=init, alpha=alpha, alphaType=alphaType, neigh=neigh,
				topol=topol, radius=radius, rlen=rlen, err.radius=err.radius,
				inv.alp.c=inv.alp.c)

		new("somout", list(method="som", somout=out, call=match.call()))
})
	


setClass("SOMBout", contains="list")
setMethod("show", "SOMBout", function(object) {
 cat("SOMB output\n")
 print(object$call)
 cat("available elements:\n")
 print(names(object))
})

setMethod("SOMB", c("exprSet", "character", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY", "ANY"), 
		function(exprObj, classifLab, kx, ky, topo, rlen, alpha, radii, init, metric){

		dat <- t(exprs(exprObj))
		dis <- dist(dat, method=metric)
		sgrid <- class::somgrid(xdim=kx, ydim=ky, topo=topo)
		out <- class::SOM(dat, sgrid, rlen=rlen, alpha=alpha, radii=radii, init)
#		new("classifPred", sampLabels=exprObj[[classifLab]], distMat=dis, classifObj=out)
# this function has no commonality with the others, just return a list for now
		new("SOMBout", list(method="SOM", SOMout=out, SOMgrid=sgrid, distMat=dis,
			call=match.call()))

})
