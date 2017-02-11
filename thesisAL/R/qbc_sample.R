#' Query by Committee
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param unlabel_index_c is a vector of n pre-selected indices that the AL method may choose from 
#' @param committee the list of committee classifiers
#' @param dis is the disagreement measure between committee classifications
#' @param ... additional parameters for the active learning method
#'
#' @return a vector of indices to query AND the committee predictions of this round
#' @export
qbc_sample <- function(X, y, unlabel_index_c, committee, dis = "vote_entropy", ...) {
  if (missing(committee) || is.null(committee)) {
    stop("A committee is required for QBC")
  }
  
  # Standard QBC
  unlabel_index <- which(is.na(y))
  x_lab <- X[-unlabel_index,]
  y_lab <- y[-unlabel_index]
  x_ulab <- X[unlabel_index_c,]
  
  p <- list()
  for (i in 1:length(committee)) {
    tout <- train(x_lab,y_lab,committee[i])
    p[[i]] <- predict(tout, newdata=x_ulab)
  }
  ################# start to edit:
#   sapply(1:length(committee), function(x){
#     tout <- train(x_lab, y_lab, committe[i]
#                   predict(tout, newdata = x_ulab)
#   })
  
  

  # Compute disagreement methods (utilizing the functions from the activelearning package)
  if(dis == "vote_entropy") d <- vote_entropy(p, type="class")
  else if (dis == "post_entropy") d <- post_entropy(p, type="class")
  else if (dis == "kullback") d <- kullback(p, type="class")
  else stop("Disagreement method '",dis,"' does not exist")
  
  index <- unlabel_index_c[which(d == max(d))]
  if (length(index) > 1) index <- sample(index,1)
  
  # gather the each committee's prediction
  pre <- rep(0,length(committee))
  for (i in 1:length(committee)) {
    # predict function returns a factor
    pre[i] <- as.numeric(as.character(p[[i]][which(unlabel_index_c==index)]))
  }
  list(index, pre)
}

#' Query by Committee (Pruning function)
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param index is the classification of X[index,] which was queried
#' @param committee_pred is the list of committee predictions for index
#' @param k is the current iteration number that the AL_engine is on
#' @param pt is the pruning threshold (anything below it is pruned)
#' @param err is the current error-to-iteration ratio of each committee member
#' @param is_prune is TRUE when pruning is desired, FALSE when not
#' @param ... additional parameters for the active learning method
#'
#' @return a list with the updated error and indices to delete from the committee
#' @export
qbc_prune <- function(X, y, index, committee_pred, k, pt = 0.25, err, is_prune, ...) {
  if (missing(err) || is.null(err) || is.na(err)) {
    stop("Committee error ratio is required for QBC pruning")
  }
  prune <- vector()
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









# Disagreement methods (From activeleaning package)
#' @importFrom itertools2 izip
#' @importFrom entropy entropy
vote_entropy <- function(x, type='class', entropy_method='ML') {
  it <- do.call(itertools2::izip, x)
  disagreement <- sapply(it, function(obs) {
    entropy(table(unlist(obs)), method=entropy_method)
  })
  disagreement
}

#' @importFrom entropy entropy.plugin
post_entropy <- function(x, type='posterior') {
  avg_post <- Reduce('+', x) / length(x)
  apply(avg_post, 1, function(obs_post) {
    entropy.plugin(obs_post)
  })
}

kullback <- function(x, type='posterior') {
  avg_post <- Reduce('+', x) / length(x)
  kullback_members <- lapply(x, function(obs) {
    rowSums(obs * log(obs / avg_post))
  })
  
  Reduce('+', kullback_members) / length(kullback_members)
}