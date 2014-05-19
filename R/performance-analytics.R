## IMPORTANT: Confusion matrix expects
##  *known*  classes to be along the *columns* and
##  *predicted* ones to be along the *rows*

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

.accuracy <- function(mat)
  sum(diag(mat))/sum(mat)

.sensitivity <- 
  .recall <- function(mat) 
  diag(mat)/colSums(mat)



.specificity <- function(mat) {
  TN <- .tn(mat)
  FP <- .fp(mat)
  TN/(TN+FP)
}

.precision <- function(mat) 
  diag(mat)/rowSums(mat)

.F1 <- function(mat) {
  r <- .recall(mat)
  p <- .precision(mat)
  return((2*p*r)/(p+r))
}


.macroF1 <- function(p, r) {
  if (missing(r)) { 
    F1 <- mean(.F1(p))
  } else { 
    F1 <- (2*p*r)/(p+r)
  }
  mean(F1) ## macro F1
}

._F1 <- function(mat, i) {
  TP <- .tp(mat)[i]
  FN <- .fn(mat)[i]
  FP <- .fp(mat)[i] 
  p <- sum(TP)/sum(TP, FP)
  r <- sum(TP)/sum(TP, FN)
  return((2*p*r)/(p+r))  
}

.macroF1_OLD <- function(p, r) {
  if (!all.equal(names(p), names(r)))
    stop("precision and recall do not match.")
  p <- mean(p)
  r <- mean(r)
  return((2*p*r)/(p+r))
}

setGeneric("acc", function(obj, ...) standardGeneric("acc"))
setMethod("acc", "table", function(obj) .accuracy(obj))

setGeneric("tp", function(obj, ...) standardGeneric("tp"))
setMethod("tp", "table", function(obj) .tp(obj))

setGeneric("tn", function(obj, ...) standardGeneric("tn"))
setMethod("tn", "table", function(obj) .tn(obj))

setGeneric("fn", function(obj, ...) standardGeneric("fn"))
setMethod("fn", "table", function(obj) .fn(obj))

setGeneric("fp", function(obj, ...) standardGeneric("fp"))
setMethod("fp", "table", function(obj) .fp(obj))

setGeneric("F1", function(obj, ...) standardGeneric("F1"))
setMethod("F1", "table", function(obj) .F1(obj))

setGeneric("specificity", function(obj, ...) standardGeneric("specificity"))
setMethod("specificity", "table", function(obj) .specificity(obj))

setMethod("macroF1", c("table","missing"),
          function(obj, type, ...) {
            return(.macroF1(obj))
          })

setMethod("recall", c("table","missing"),
          function(obj, type, ...) return(.recall(obj)))

setMethod("precision", c("table","missing"),
          function(obj, type, ...) return(.precision(obj)))


confuTab <- function(obj) {
  .makeConfuTab <- function(x) {    
    m <- matrix(x, nrow = 2)
    dimnames(m) <- list(predicted = c(TRUE, FALSE),
                        known = c(TRUE, FALSE))                        
    as.table(m)
  }
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
