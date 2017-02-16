#' Query by Committee (Pruning function)
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param index is the classification of X[index,] which was queried
#' @param committee_pred is the list of committee predictions for index
#' @param k is the current iteration number that the AL_engine is on
#' @param pt is the pruning threshold (any error value above it is pruned)
#' @param err is the error-to-iteration ratio of each committee member (0, the best, -> 1, the worst)
#' @param is_prune is TRUE when pruning is desired, FALSE when not
#' @param ... additional parameters for the active learning method
#'
#' @return a list with the updated error and indices to delete from the committee
#' @export
qbc_prune <- function(X, y, index, committee_pred, k, pt = 0.5, err, is_prune, ...) {
  if (missing(err) || is.null(err) || is.na(err)) {
    stop("Committee error ratio is required for QBC pruning")
  }
  prune <- vector() # Do not know how long prune will be until the end
  # Do not prune if committee size is 1 or it's the first round
  if (length(committee_pred) == 1 | k == 1) {
    list(err, prune)
  } else {
    # Update error value
    for (i in 1:length(committee_pred)) {
      if (committee_pred[i] == y[index]) iv <- 0 else iv <- 1
      err[i] <- err[i] + (iv - err[i])/k
      if (err[i] > pt & is_prune) {
        prune <- c(prune,i)
      }
    }
    list(err, prune)
  }
}