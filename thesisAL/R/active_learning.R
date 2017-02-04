#the missing labels in y are denoted by NA
#this method takes x as a matrix of all the data
active_learning <- function(X, y, method = "lda", ...){
  stopifnot(nrow(X) == length(y), is.matrix(X), any(is.na(y)),
            is.factor(y), length(levels(y)) == 2)

  if(method == "lda") .active_learning_lda(X, y, ...)
}
