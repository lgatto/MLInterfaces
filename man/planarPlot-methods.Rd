\name{planarPlot-methods}
\docType{methods}
\alias{planarPlot}
\alias{planarPlot-methods}
\alias{planarPlot,classifOutput,exprSet,character-method}
\title{ Methods for Function planarPlot in Package `MLInterfaces' }
\description{
show the classification boundaries on the plane dictated
by two genes in an exprSet
}
\section{Methods}{
\describe{
\item{clo = "classifOutput", eset = "exprSet", classifLab = "character"}{ ~~describe this method here }
}}
\examples{
library(ALL)
data(ALL)
#
# restrict to BCR/ABL or NEG
#
bio <- which( ALL$mol.biol \%in\% c("BCR/ABL", "NEG"))
#
# restrict to B-cell
#
isb <- grep("^B", as.character(ALL$BT))
kp <- intersect(bio,isb)
all2 <- ALL[,kp]
#
# sample 2 genes at random
#
set.seed(1234)
ng <- nrow(exprs(all2))
pick <- sample(1:ng, size=2, replace=FALSE)
library(hgu95av2)
gg <- all2[pick,]
sym <- unlist(mget(featureNames(gg), hgu95av2SYMBOL))
featureNames(gg) <- sym
class <- as.character(all2$mol.biol)
gg@phenoData@data$class <- factor(class)
gg@phenoData@varMetadata = rbind(gg@phenoData@varMetadata,class="class" )
newpd = new("phenoData", pData=pData(gg), varLabels=varMetadata(gg)[,1])
gg = new("exprSet", exprs=exprs(gg), phenoData=newpd)
cl1 <- which( class == "NEG" )
cl2 <- which( class != "NEG" )
#
# create balanced training sample
#
trainInds <- c( sample(cl1, size=floor(length(cl1)/2) ),
      sample(cl2, size=floor(length(cl2)/2)) )
#
# run rpart
#
tgg <- rpartB( gg, "class", trainInds, minsplit=4 )
opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
planarPlot( tgg, gg, "class" )
title("rpart")
#
# run nnet
#
ngg <- nnetB( gg, "class", trainInds, size=8 )
planarPlot( ngg, gg, "class" )
title("nnet")
#
# run knn
#
kgg <- knnB( gg, "class", trainInds, k=3, l=1 )
planarPlot( kgg, gg, "class" )
title("3-nn")
#
# run svm
#
sgg <- svmB( gg, "class", trainInds )
planarPlot( sgg, gg, "class" )
title("svm")
par(opar)
}
\keyword{methods}
