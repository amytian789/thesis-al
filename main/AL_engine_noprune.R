AL_engine_noprune <- function(X, y, y_unlabeled, al_method,
                      classifier_method, return_method, iter, n, ...){
  
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
      if (i != 1 & as.character(substitute(classifier_method))=="qbc_majority") {
        # QBC Majority method re-trains committee after the oracle
        # Save computation time by passing those results to QBC algo
        next_sample <- active_learning(X=X, y=y_unlabeled, almethod=al_method, n=n, committee = cm, 
                                       isMajority = TRUE, tout = tout, ...)
      } else {
        next_sample <- active_learning(X=X, y=y_unlabeled, almethod=al_method, n=n, committee = cm, ...)
      }
      y_unlabeled[next_sample[[1]]] <- y[next_sample[[1]]]
      
      # Compute residual error
      idx <- which(!is.na(y_unlabeled))
      tout <- classifier_method(X[idx,], y_unlabeled[idx], committee = cm)
      res[i] <- return_method(tout, X, y, committee = cm)
    }
    # Everything else (not QBC, not US)
    else {
      if (i != 1 & al_method == "us") {
        # classifier_method re-trains random forest after the oracle
        # Save computation time by passing those results to US algo
        # Of course, this only works since classifier = "rf", and the
        # classifier_method function also uses "rf"
        next_sample <- active_learning(X, y_unlabeled, al_method, n, isR = TRUE, tout = tout, ...)
      } else {
        next_sample <- active_learning(X, y_unlabeled, al_method, n, ...)
      }
      y_unlabeled[next_sample] <- y[next_sample]
      
      # Compute residual error
      idx <- which(!is.na(y_unlabeled))
      tout <- classifier_method(X[idx,], y_unlabeled[idx])
      res[i] <- return_method(tout, X, y)
    }
  }
  res
}
