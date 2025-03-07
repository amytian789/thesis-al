#' Main active learning engine
#'
#' The missing labels in y are denoted by NA.
#' This method takes X as a matrix of all the data
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param almethod the AL method name
#' @param n is the number of unlabeled points to be "pooled"
#' @param ... additional parameters for the active learning method
#'
#' @return an index corresponding to the row of X to learn the label of next
#' @export

active_learning <- function(X, y, almethod = "us", n, ...){
  
  stopifnot(nrow(X) == length(y), is.matrix(X), any(is.na(y)),
            is.factor(y), length(levels(y)) == 2)
  
  if (n == 0) {
    unlabel_index_c <- which(is.na(y))
  } else unlabel_index_c <- sample(which(is.na(y)), n)

  switch(almethod,
         us=uncertainty_sample(X,y,unlabel_index_c, ...),
         rs=random_sample(unlabel_index_c, ns = 1, ...),
         qbc=qbc_sample(X,y,unlabel_index_c, ...),
         qbb=qbb_sample(X,y,unlabel_index_c,...),
         qbc_prune=qbc_prune(X=X, y=y, ...),
         cluster=cluster_sample(X,y,unlabel_index_c, ...)
         )
}
