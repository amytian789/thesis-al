source("../main/AL_header.R")

################################
# set up the data

date <- Sys.Date()
iter <- 100

set.seed(10)
X <- rbind(MASS::mvrnorm(100, rep(0,2), diag(2)),
           MASS::mvrnorm(100, c(0.5,0), diag(2)))
y <- as.factor(c(rep(0, 100), rep(1, 100)))

#set up the unlabeled data
y_unlabeled <- y
y_unlabeled[c(11:100, 111:200)] <- NA

##################################
# set up the methods

al_method <- function(X, y){
  active_learning(X, y, method = "lda")
}

random_method <- function(X, y){
  sample(which(is.na(y)), 1)
}

classifier_method <- function(X, y){
  MASS::lda(X, y)
}

#set up the return method
return_method <- function(classifier, X, y){
  pred <- stats::predict(classifier, X)$class

  length(which(pred != y))/length(y)
}

###################################

#run the engine
set.seed(10)
lda_results <- AL_engine(X, y, y_unlabeled, al_method, classifier_method,
                         return_method, iter)

set.seed(10)
random_results <- AL_engine(X, y, y_unlabeled, random_method, classifier_method,
                         return_method, iter)

save.image(file = paste0("../results/lda_", date, ".RData"))


