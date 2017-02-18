AL_engine <- function(X, y, y_unlabeled, al_method,
                      classifier_method, return_method, iter, n, ...) {

  stopifnot(nrow(X) == length(y), is.matrix(X), is.factor(y), length(levels(y)) == 2)
  idx <- which(is.na(y_unlabeled))
  stopifnot(length(idx) > 0, all(y[-idx] == y_unlabeled[-idx]), length(y) == length(y_unlabeled),
            is.factor(y_unlabeled))

  res <- rep(0,iter)
  
  ### SET THE COMMITTEE HERE
  cm <- c("rf","nb","pls","svmRadialWeights")
  err<- rep(0,length(cm))
  
  for(i in 1:iter){
    # If QBC, the procedure is a little different....
    if (al_method == "qbc") {
      next_sample <- active_learning(X=X, y=y_unlabeled, almethod=al_method, n=n, committee = cm,...)
      y_unlabeled[next_sample[[1]]] <- y[next_sample[[1]]]
      
      # Update error and prune as desired
      if (i > iter/2) {
        prune <- active_learning(X=X, y=y_unlabeled, almethod="qbc_prune", n = n, index=next_sample[[1]],
                                committee_pred=next_sample[[2]], k = i, err = err, is_prune = TRUE, ...)
        err <- prune[[1]]
        # check if there's stuff to prune
        if (length(prune[[2]] != 0)) {
          cm <- cm[-unlist(prune[[2]])]
          err <- err[-unlist(prune[[2]])]
        }
      }
      else {
        prune <- active_learning(X=X, y=y_unlabeled, almethod="qbc_prune", n = n, index=next_sample[[1]],
                                 committee_pred=next_sample[[2]], k = i, err = err, is_prune = FALSE, ...)
        err <- prune[[1]]
      }
    }
    # Everything else (not QBC)
    else {
      next_sample <- active_learning(X, y_unlabeled, al_method, n, ...)
      y_unlabeled[next_sample] <- y[next_sample]
    }
    
    # Compute residual error (committee slot is not used if not QBC method)
    idx <- which(!is.na(y_unlabeled))
    classifier <- classifier_method(X[idx,], y_unlabeled[idx], committee = cm)
    res[i] <- return_method(classifier, X, y, committee = cm)

    if (i == 45) print(cm)
  }
  
  res
}
