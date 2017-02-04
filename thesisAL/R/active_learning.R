#' Main active learning method
#'
#' The missing labels in y are denoted by NA.
#' This method takes x as a matrix of all the data
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param method the method name
#' @param ... additional parameters for the active learning method
#'
#' @return an index corresponding to the row of X to learn the label of next
#' @export
active_learning <- function(X, y, method = "lda", ...){
  stopifnot(nrow(X) == length(y), is.matrix(X), any(is.na(y)),
            is.factor(y), length(levels(y)) == 2)

  if(method == "lda") .active_learning_lda(X, y, ...)
}
