#' Uncertainty Sampling with bivariate labels
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param unlabel_index_c is a vector of n pre-selected indices that the AL method may choose from 
#' @param classifier the classifier name
#' @param ... additional parameters for the active learning method
#'
#' @return a vector of indices to query
#' @export
uncertainty_sample <- function(X, y, unlabel_index_c, classifier = "lda", ...) {
  # check that the classifier is compatible with uncertainty sampling
  c <- try(modelLookup(classifier))
  if (!any(c$probModel)) {
    stop(classifier," must return posterior probabilities")
  }
  
  # split x and y to retrieve labeled and unlabeled pairs
  unlabel_index <- which(is.na(y))
  x_lab <- X[-unlabel_index,]
  y_lab <- y[-unlabel_index]
  x_ulab <- X[unlabel_index_c,]
  
  # train function depeneds on the "Caret" package
  tout <- train(x_lab,y_lab,classifier)
  p <- as.matrix(predict(tout, newdata=x_ulab, type="prob"))
 
  # return corresponding X index of posterior closest to 0.5
  p <- apply(p, 1, function(x) abs(x[1]-0.5))
  unlabel_index_c[which(p == min(p))]
}