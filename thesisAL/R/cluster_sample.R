#' Query by Min-Max Clustering
#'
#' @param X the full data matrix, n x d, including all unlabeled data
#' @param y a factor vector with 2 levels and NAs for unlabeled data
#' @param unlabel_index_c is a vector of n pre-selected indices that the AL method may choose from 
#' @param dis is the distance measure between data
#' @param ... additional parameters for the active learning method
#'
#' @return a vector of indices to query
#' @export
cluster_sample <- function(X, y, unlabel_index_c, dis = "euclidean", ...) {
  label_index <- which(!is.na(y))
  x_lab <- X[label_index,]
  y_lab <- y[label_index]
  x_ulab <- X[unlabel_index_c,]
  y_ulab <- y[unlabel_index_c]
  
  # Select the point furthest from points that are already labeled
  q <- rep(0,length(y_ulab))
  for (i in 1:length(y_ulab)) {
    min <- Inf
    for (j in 1:length(y_lab)) {
      temp <- cs_distance(X[unlabel_index_c[i],],X[label_index[j],],dis)
      if (min > temp) min <- temp
    }
    q[i] <- min
  }
  unlabel_index_c[which(q==max(q))]
}

# General Support Function for Distance Computation
cs_distance <- function(a,b,dis = "euclidean"){
  d <- switch(dis,
         euclidean=cs_euclidean_distance(a,b)
         )
}

# Euclidean Distance Computation
cs_euclidean_distance <- function(a,b) {
  sqrt( sum( mapply( function(x,y) (x-y)^2, a, b)))
}