AL_engine <- function(X, y, y_unlabeled, al_method,
                      classifier_method, return_method, iter, n){

  stopifnot(nrow(X) == length(y), is.matrix(X), is.factor(y), length(levels(y)) == 2)
  idx <- which(is.na(y_unlabeled))
  stopifnot(length(idx) > 0, all(y[-idx] == y_unlabeled[-idx]), length(y) == length(y_unlabeled),
            is.factor(y_unlabeled))

  res <- vector("list", iter)

  for(i in 1:iter){
    next_sample <- active_learning(X, y_unlabeled, al_method, n)
    
    y_unlabeled[next_sample] <- y[next_sample]

    idx <- which(!is.na(y_unlabeled))
    classifier <- classifier_method(X[idx,], y_unlabeled[idx])

    res[[i]] <- return_method(classifier, X, y)
  }

  res
}
