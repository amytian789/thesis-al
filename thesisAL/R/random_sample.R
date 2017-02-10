#' Random querying
#'
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param n is the number of queries to return
#' @param ... additional parameters for the active learning method
#'
#' @return a vector of indices to query
#' @export
random_sample <- function(unlabel_index_c, n=1, ...) {
  if (length(unlabel_index_c) < n) stop("There are not enough unlabeled entries to sample ",n," queries.")
  sample(unlabel_index_c,n)
}