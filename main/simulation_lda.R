source("../main/AL_header.R")

date <- Sys.Date()

set.seed(10)
X <- rbind(MASS::mvrnorm(100, rep(0,2), diag(2)),
           MASS::mvrnorm(100, c(0.5,0), diag(2)))
y <- as.factor(c(rep(0, 100), rep(1, 100)))

#set up the unlabeled data
y_unlabeled <- y
y_unlabeled[c(11:100, 111:200)] <- NA

al_method <- al_method_closure(active_learning, method = "lda")
classifier_method <- classifer_method_closure(MASS::lda)

#set up the return method
return_method <- function(classifier, X, y){
  pred <- stats::predict(classifier, X)$class
  length(which(pred != y))/length(y)
}

iter <- 100

#run the engine
set.seed(10)
lda_results <- AL_engine(X, y, y_unlabeled, al_method, classifier_method,
                         return_method, iter)

save.image(file = paste0("../results/lda_", date, ".RData"))


