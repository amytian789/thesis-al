#' Query by Bagging
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param unlabel_index_c is a vector of n pre-selected indices that the AL method may choose from 
#' @param classifier_train is a function that returns a classifier given labeled data
#' @param classifier_predict is a function that returns labeled predictions given a classifier
#' @param num_class is the number of times to randomly sample the training set for a new committee classifier 
#' @param r in (0,1]. r*(# of labeled points) = # of points to randomly sample for each of the num_class rounds
#' @param dis is the disagreement measure between committee classifications
#' @param ... additional parameters for the active learning method
#'
#' @return an index to query
#' @export
qbc_sample <- function(X, y, unlabel_index_c, classifier_train, classifier_predict, num_class, r, dis = "vote_entropy", ...){
  if(r<=0 || r>0) stop("r must be in (0,1]. r*(# of labeled points) = # of points to randomly sample for each of the num_class rounds")
  
  label_index <- which(!is.na(y))
  x_lab <- X[label_index,]
  y_lab <- y[label_index]
  x_ulab <- X[-label_index_c,]
  
  # Randomly sample from the labeled set to create a classifier
  committee <- vector("list",num_class)
  for (i in 1:num_class) {
    idx <- sample(label_index,round(length(label_index)*r,0))
    committee[[i]] <- classifier_train(x_lab[idx,],y_lab[idx])
  }
  
  # Utilize the resulting classifiers as a committee
  p <- vector("list",length(committee))
  for (i in 1:length(committee)) {
    p[[i]] <- classifier_predict(committee[[i]], x_ulab)
  }
  
  # Compute disagreement (utilizing the functions from the activelearning package)
  d <- switch(dis,
              vote_entropy=vote_entropy(p, type="class"),
              post_entropy=post_entropy(p, type="class"),
              kullback=kullback(p, type="class")
              )
  
  index <- unlabel_index_c[which(d == max(d))]
  if (length(index) > 1) index <- sample(index,1)
  index
}