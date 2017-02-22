#' Query by Bagging
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param unlabel_index_c is a vector of n pre-selected (pooled) indices
#' @param classifier the name of a classification model
#' @param dis is the disagreement measure between committee classifications
#' @param num_class is the number of desired committee members
#' @param r in (0,1). r*(labeled set) = training set for each num_class round
#' @param ... additional parameters for the active learning method
#'
#' @return an index to query
#' @export

qbb_sample <- function(X, y, unlabel_index_c, classifier, 
                       dis = "vote_entropy", num_class, r, ...){
  
  if(r<=0 || r>=1) stop("r must be in (0,1)")
  
  x_ulab <- X[unlabel_index_c,]
  
  # Randomly sample from the labeled set to create a classifier
  label_index <- which(!is.na(y))
  committee <- vector("list",num_class)
  for (i in 1:num_class) {
    idx <- sample(label_index,round(length(label_index)*r,0))
    committee[[i]] <- caret::train(X[idx,],y[idx],classifier)
  }
  
  # Utilize the resulting classifiers as a committee
  p <- vector("list",length(committee))
  for (i in 1:length(committee)) {
    p[[i]] <- stats::predict(committee[[i]], x_ulab)
  }
  
  # Compute disagreement (functions from the activelearning package)
  d <- switch(dis,
              vote_entropy=vote_entropy(p),
              post_entropy=post_entropy(p),
              kullback=kullback(p)
              )
  
  index <- unlabel_index_c[which(d == max(d))]
  if (length(index) > 1) index <- sample(index,1)
  index
}