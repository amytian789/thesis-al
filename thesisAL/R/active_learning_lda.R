#uncertainty sampling lda
.active_learning_lda <- function(X, y){
  stopifnot(any(is.na(y)))

  idx <- which(!is.na(y))

  res <- MASS::lda(x = X[idx,], grouping = y[idx])

  posterior <- stats::predict(res, X[-idx,])$posterior
  uncer <- apply(posterior, 1, function(x){abs(x[1] - x[2])})

  idx_unlabled <- c(1:nrow(X))[-idx]
  idx_unlabled[which.min(uncer)]
}
