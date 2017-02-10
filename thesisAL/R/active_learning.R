#' Main active learning method
#'
#' The missing labels in y are denoted by NA.
#' This method takes x as a matrix of all the data
#'
#' @param X the full data matrix, n x d, including all unlabeled data (d observations of n variables)
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param almethod the AL method name
#' @param n is the number of unlabeled points which will be given to the AL method as part of a "stream"
#' @param ... additional parameters for the active learning method
#'
#' @return an index corresponding to the row of X to learn the label of next
#' @export
active_learning <- function(X, y, almethod = "us", n, ...){
  stopifnot(nrow(X) == length(y), is.matrix(X), any(is.na(y)),
            is.factor(y), length(levels(y)) == 2)

  unlabel_index_c <- sample(which(is.na(y)), n)

  if(almethod == "us") uncertainty_sample(X,y,unlabel_index_c, ...)
  else if (almethod == "rs") random_sample(unlabel_index_c, n = 1, ...)
  else if (almethod == "qbc") qbc_sample(X,y,unlabel_index_c, ...)
  else stop("AL method '",method,"' does not exist")
}
