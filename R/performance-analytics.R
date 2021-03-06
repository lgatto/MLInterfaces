## IMPORTANT: Confusion matrix expects
##  *known*  classes to be along the *columns* and
##  *predicted* ones to be along the *rows*

naAs0 <- function(x) {
    x[is.na(x)] <- 0
    x
}

## for testing
makeConfuMat <- function(i = 0:5, j = 15:20, k = 3) {
    if (k > 26)
        k <- 26
    m <- matrix(sample(i, 9, replace=TRUE), k)
    dimnames(m) <- list(predicted = LETTERS[1:k],
                        known = LETTERS[1:k])
    diag(m) <- sample(j, 3)
    as.table(m)
}


## See sensitivity and specificity methods in caret
## Want all the following to work on
## - 2 characters/factors/numerics
## - a table

## -> write the S4 methods and use caret

.tp <- function(mat) {
    ans <- diag(mat)
    if (nrow(mat) == 2)
        ans <- ans[1]
    return(ans)
}

.tn <- function(mat) {
    if (nrow(mat) == 2) {
        ans <- mat[2,2]
    } else {
        ans <- sapply(seq_len(nrow(mat)),
                      function(i) sum(mat[-i, ][, -i]))
        names(ans) <- rownames(mat)    
    }
    return(ans)
}

.fp <- function(mat) {
    ans <- rowSums(mat)-diag(mat)
    if (nrow(mat) == 2)
        ans <- ans[1]
    return(ans)
}

.fn <- function(mat) {
    ans <- colSums(mat)-diag(mat)
    if (nrow(mat) == 2)
        ans <- ans[2]
    return(ans)
}

.accuracy <- function(mat, naAs0. = FALSE) {
    if (naAs0.) mat <- naAs0(mat)
    sum(diag(mat))/sum(mat)
}

.sensitivity <- 
    .recall <- function(mat, naAs0. = FALSE) {
        if (naAs0.) mat <- naAs0(mat)
        diag(mat)/colSums(mat)
    }


.specificity <- function(mat, naAs0. = FALSE) {
    if (naAs0.) mat <- naAs0(mat)
    TN <- .tn(mat)
    FP <- .fp(mat)
    TN/(TN+FP)
}

.precision <- function(mat, naAs0. = FALSE) {
    if (naAs0.) mat <- naAs0(mat)
    diag(mat)/rowSums(mat)
}

.F1 <- function(mat, naAs0. = FALSE) {
    if (naAs0.) mat <- naAs0(mat)
    r <- .recall(mat, naAs0.)
    p <- .precision(mat, naAs0.)
    ans <- (2*p*r)/(p+r)
    if (naAs0.) ans <- naAs0(ans)
    return(ans)
}


.macroF1 <- function(p, r, naAs0. = FALSE) {
    if (naAs0.) p <- naAs0(p)            
    if (missing(r)) { 
        F1 <- .F1(p, naAs0. = naAs0)
    } else {
        if (naAs0.) r <- naAs0(r)            
        F1 <- (2*p*r)/(p+r)
    }
    if (naAs0.) F1 <- naAs0(F1)
    mean(F1) ## macro F1
}

## ._F1 <- function(mat, i) {
##   TP <- .tp(mat)[i]
##   FN <- .fn(mat)[i]
##   FP <- .fp(mat)[i] 
##   p <- sum(TP)/sum(TP, FP)
##   r <- sum(TP)/sum(TP, FN)
##   return((2*p*r)/(p+r))  
## }

## .macroF1_OLD <- function(p, r) {
##   if (!all.equal(names(p), names(r)))
##     stop("precision and recall do not match.")
##   p <- mean(p)
##   r <- mean(r)
##   return((2*p*r)/(p+r))
## }

setGeneric("acc", function(obj, ...) standardGeneric("acc"))
setMethod("acc", "table", function(obj, ...) .accuracy(obj, ...))

setGeneric("tp", function(obj, ...) standardGeneric("tp"))
setMethod("tp", "table", function(obj) .tp(obj))

setGeneric("tn", function(obj, ...) standardGeneric("tn"))
setMethod("tn", "table", function(obj) .tn(obj))

setGeneric("fn", function(obj, ...) standardGeneric("fn"))
setMethod("fn", "table", function(obj) .fn(obj))

setGeneric("fp", function(obj, ...) standardGeneric("fp"))
setMethod("fp", "table", function(obj) .fp(obj))

setGeneric("F1", function(obj, ...) standardGeneric("F1"))
setMethod("F1", "table", function(obj, ...) .F1(obj, ...))

setGeneric("specificity", function(obj, ...) standardGeneric("specificity"))
setMethod("specificity", "table", function(obj, ...) .specificity(obj, ...))

## Generic defined in methods-classification.R
setMethod("macroF1", c("table","missing"),
          function(obj, type, naAs0. = FALSE, ...) {
              return(.macroF1(obj, naAs0., ...))
          })

setMethod("macroF1", c("numeric","numeric"),
          function(obj, type, ...) {
              ## obj is precision
              ## type is recall
              return(.macroF1(obj, type, ...))
          })

setMethod("recall", c("table","missing"),
          function(obj, type, ...) return(.recall(obj, ...)))

setMethod("precision", c("table","missing"),
          function(obj, type, ...) return(.precision(obj, ...)))


confuTab <- function(obj, naAs0. = FALSE) {
    .makeConfuTab <- function(x) {    
        m <- matrix(x, nrow = 2)
        dimnames(m) <- list(predicted = c(TRUE, FALSE),
                            known = c(TRUE, FALSE))                        
        as.table(m)
    }
    if (naAs0.) mat <- naAs0(mat)    
    TP <- tp(obj)
    FP <- fp(obj)
    FN <- fn(obj)
    TN <- tn(obj)  
    ans <- lapply(seq_len(nrow(obj)),
                  function(i)
                      .makeConfuTab(c(TP[i], FN[i], FP[i], TN[i])))
    names(ans) <- rownames(obj)
    return(ans)
}
