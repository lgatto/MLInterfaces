
newPredClass <- function(x) new("predClass", x)
newGroupIndex <- function(x) new("groupIndex", x)

newProbMat <- function(x) if(length(x)>0)new("probMat", x) else new("probMat")
newProbArray <- function(x) if(length(x)>0)new("probArray", x) else new("probArray")
newMembMat <- function(x) if(length(x)>0)new("membMat", x) else new("membMat")
newQualScore <- function(x) if(length(x)>0)new("qualScore", x) else new("qualScore")
newSilhouetteVec <- function(x) if(length(x)>0)new("silhouetteVec", x) else new("silhouetteVec")

