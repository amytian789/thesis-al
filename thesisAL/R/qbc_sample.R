#' Query by Committee
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param unlabel_index_c is a vector of n pre-selected (pooled) indices
#' @param committee the list of committee classifiers
#' @param dis is the disagreement measure between committee classifications
#' @param isMajority is if overall classifier Majority Vote or Random Forest
#' @param tout is a list of trained classifiers from Majority Vote computation 
#' @param ... additional parameters for the active learning method
#'
#' @return a list with: an index to query AND the committee predictions
#' @export

qbc_sample <- function(X, y, unlabel_index_c, committee, 
                       dis = "vote_entropy", isMajority = FALSE, tout = NULL, ...){
  
  if (missing(committee) || is.null(committee)) stop("A committee is required")
  if (isMajority & is.null(tout)) {
    stop("Re-feed the majority vote return to the next QBC_sample call")
  }
  
  unlabel_index <- which(is.na(y))
  x_lab <- X[-unlabel_index,]
  y_lab <- y[-unlabel_index]
  x_ulab <- X[unlabel_index_c,]
  p <- vector("list",length(committee))
  
  if (!isMajority) {
    for (i in 1:length(committee)) {
      tout <- caret::train(x_lab,y_lab,committee[i])
      p[[i]] <- predict(tout, newdata=x_ulab)
    }
  } else {
    # Reuse the trained classifiers from the majority vote call
    for (i in 1:length(committee)) {
      p[[i]] <- predict(tout[[i]], newdata=x_ulab)
    }
  }

  # Compute disagreement (functions from the activelearning package)
  d <- switch(dis,
              vote_entropy=vote_entropy(p),
              post_entropy=post_entropy(p),
              kullback=kullback(p)
  )
  
  index <- unlabel_index_c[which(d == max(d))]
  if (length(index) > 1) index <- sample(index,1)
  # Gather each committee's prediction
  pre <- rep(0,length(committee))
  for (i in 1:length(committee)) {
    # Predict function returns a factor
    pre[i] <- as.numeric(as.character(p[[i]][which(unlabel_index_c==index)]))
  }
  
  list(index, pre)
}