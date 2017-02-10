source("../main/AL_header.R")

library(caret)

################################
# set up the data

date <- Sys.Date()
iter <- 100 

set.seed(10)
X <- rbind(MASS::mvrnorm(100, rep(0,2), diag(2)),
           MASS::mvrnorm(100, c(0.5,0), diag(2)))
y <- rep(0,200)
for (i in 1:200) {
  if (X[i,1] > 0 & X[i,2] > 0) y[i] = 1
}
y <- factor(y)

#set up the unlabeled data
y_unlabeled <- y
y_unlabeled[c(11:100, 111:200)] <- NA

##################################
# set up the methods

classifier_method <- function(X, y){
  MASS::lda(X, y)
}

#set up the return method
return_method <- function(classifier, X, y){
  pred <- stats::predict(classifier, X)$class

  length(which(pred != y))/length(y)
}

###################################

# The number of random unlabeled points it "streams" to the AL / RS method
n = 15

#run the engine (average over 1000 random samples)
set.seed(10)
us_lda_results <- AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "us", classifier_method = classifier_method,
                            return_method = return_method, iter = iter, n = n, classifier = "lda")

set.seed(10)
qbc_results <- AL_engine(X=X, y=y, y_unlabeled=y_unlabeled, al_method = "qbc", classifier_method = classifier_method,
                            return_method = return_method, iter = iter, n = n, dis = "vote_entropy", pt = 0.75)

set.seed(10)
random_results <- AL_engine(X, y, y_unlabeled, al_method = "rs", classifier_method,
                            return_method, iter, n)

# classifier performance given everything
pred <- stats::predict(MASS::lda(X, y), X)$class
perf_results <- rep(length(which(pred != y))/length(y),iter)

###################################

#unwind the results
us_lda_vec <- unlist(us_lda_results)
random_vec <- unlist(random_results)
qbc_vec <- unlist(qbc_results)

#plot
ymax <- max(c(us_lda_vec, random_vec))
graphics::plot(1:iter, us_lda_vec, ylim = c(0, ymax), lwd = 2, type = "l", main="Performance of AL", xlab="Interations", ylab="Error")
graphics::lines(1:iter, random_vec, lwd = 2, col = "red")
graphics::lines(1:iter, qbc_vec, lwd = 2, col = "blue")
graphics::lines(1:iter, perf_results, lwd = 2, col = "green")

########## This code doesn't seem too work right now
# save.image(file = paste0("../results/us_", date, ".RData"))