#' Uncertainty Sampling with bivariate labels
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param unlabel_index_c is a vector of n pre-selected (pooled) indices
#' @param classifier the classifier name
#' @param isR = T if previous classifier_method return is recycled
#' @param tout is a trained classifier from classifier_method defined in simulation.R
#' @param ... additional parameters for the active learning method
#'
#' @return an index to query
#' @export

uncertainty_sample <- function(X, y, unlabel_index_c, classifier, isR = FALSE, tout = NULL, ...){
  
  if (length(classifier) > 1 || missing(classifier) || is.null(classifier) || is.na(classifier)) {
    stop("A single classifier is required for uncertainty sampling")
  }
  if (isR & is.null(tout)) {
    stop("Re-feed classifier_method return to next uncertainty_sample call")
  }
  
  # Check that the classifier is compatible with uncertainty sampling
  c <- try(caret::modelLookup(classifier))
  if (!any(c$probModel)) {
    stop(classifier," must return posterior probabilities")
  }
  
  # Split X and y to retrieve labeled and unlabeled pairs
  unlabel_index <- which(is.na(y))
  x_lab <- X[-unlabel_index,]
  y_lab <- y[-unlabel_index]
  x_ulab <- X[unlabel_index_c,]
  
  if (!isR) {
    tout <- caret::train(x_lab,y_lab,classifier)
    p <- as.matrix(stats::predict(tout, newdata=x_ulab, type="prob"))
  } else {
    # Reuse the trained classifier from the classifier_method call
    p <- as.matrix(stats::predict(tout, newdata=x_ulab, type="prob"))
  }
  
  # Return corresponding X index of posterior closest to 0.5
  p <- apply(p, 1, function(x) abs(x[1]-0.5))
  index <- unlabel_index_c[which(p == min(p))]
  if (length(index) > 1) index <- sample(index,1)
  index
}