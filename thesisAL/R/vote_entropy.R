# Disagreement method (From activeleaning package)
#' @importFrom itertools2 izip
#' @importFrom entropy entropy
vote_entropy <- function(x, type='class', entropy_method='ML') {
  it <- do.call(itertools2::izip, x)
  disagreement <- sapply(it, function(obs) {
    entropy::entropy(table(unlist(obs)), method=entropy_method)
  })
  disagreement
}