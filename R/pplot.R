#
# code to provide planar displays of classifier predictions
#
# currently assumes use of two raw expression vectors to define
# the plane
#

getGrid <- function (eset2) 
{
    lims <- apply(exprs(eset2), 1, range)
    if (ncol(lims) > 2) 
        warning("grid based only on first 2 genes")
    nn <- colnames(lims)
    v1 <- seq(lims[1, 1], lims[2, 1], len = 200)
    v2 <- seq(lims[1, 2], lims[2, 2], len = 200)
    og <- expand.grid(v1, v2)
    names(og) <- make.names(nn)
    og
}

setGeneric("planarPlot", function(clo, eset, classifLab ) standardGeneric("planarPlot"))

setMethod("planarPlot", c("classifOutput", "exprSet", "character"), 
	function(clo, eset, classifLab) {
 require(RColorBrewer)
 pal <- brewer.pal("Set2", n=8)
 ff <- getGrid(eset)
 if (clo@method %in% c("nnet", "rpart"))
    ps <- predict( clo@RObject, newdata=ff, type="class")
 else if (clo@method == "randomForest")
    {
    names(ff) <- rownames(exprs(eset))
    ps <- predict( clo@RObject, newdata=ff )
    }
 else
    ps <- predict( clo@RObject, newdata=ff )
 plot( ff[,1], ff[,2], col=pal[as.numeric(factor(ps))], pch=19,
 xlab = names(ff)[1], ylab=names(ff)[2] )
 text( exprs(eset)[1,], exprs(eset)[2,], lab=eset[[classifLab]])
})
